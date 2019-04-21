package ru.ifmo.rain.pinchuk.concurrent;

import info.kgeorgiy.java.advanced.concurrent.ScalarIP;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.function.BinaryOperator;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;

/**
 * {@inheritDoc}
 */
public class IterativeParallelism implements ScalarIP {

    private static class Worker<T, E> extends Thread {
        T result;
        private final BinaryOperator<T> reduce;
        private final Function<E, T> map;
        private final List<? extends E> list;

        Worker(BinaryOperator<T> reduce, Function<E, T> map, List<? extends E> list) {
            this.map = map;
            this.reduce = reduce;
            this.list = list;
        }

        public void run() {
            result = list.stream().map(map).reduce(reduce).get();
        }
    }

    private static class Parallelism<T, E> {
        private final Function<E, T> map;
        private final BinaryOperator<T> reduce;

        private final List<List<? extends E>> chunks;

        Parallelism(int n, BinaryOperator<T> reduce, Function<E, T> map, List<? extends E> list) throws InterruptedException {
            if (n < 0) {
                throw new IllegalArgumentException("Thread count cannot be negative");
            }

            int threads = list.size() < n ? list.size() : n;
            chunks = new ArrayList<>(threads);

            int chunkSize = list.size() / threads;
            int remains = list.size() % threads;

            int j = 0;
            for (int i = 0; i < threads; i++) {
                int begin = j;
                j += chunkSize;
                j += (remains > 0) ? 1 : 0;
                remains = (remains > 0) ? remains - 1 : remains;
                int end = j;

                chunks.add(list.subList(begin, end));
            }

            this.map = map;
            this.reduce = reduce;
        }

        T run() throws InterruptedException {
            List<Worker<T, E>> workers = chunks.stream().map(x -> new Worker<T, E>(reduce, map, x)).collect(Collectors.toList());
            for (Worker<T, E> w : workers) {
                w.start();
            }

            for (Worker<T, E> w : workers) {
                w.join();
            }
            return workers.stream().map(x -> x.result).reduce(reduce).get();
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public <T> T maximum(int i, List<? extends T> list, Comparator<? super T> comparator) throws InterruptedException {
        Parallelism<T, T> wrk = new Parallelism<>(
                i,
                (T m, T el) -> comparator.compare(m, el) < 0 ? el : m,
                (T e) -> e,
                list
        );
        return wrk.run();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public <T> T minimum(int i, List<? extends T> list, Comparator<? super T> comparator) throws InterruptedException {
        Parallelism<T, T> wrk = new Parallelism<>(
                i,
                (T m, T el) -> comparator.compare(m, el) > 0 ? el : m,
                (T e) -> e,
                list
        );
        return wrk.run();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public <T> boolean all(int i, List<? extends T> list, Predicate<? super T> predicate) throws InterruptedException {
        Parallelism<Boolean, T> wrk = new Parallelism<>(
                i,
                (Boolean m, Boolean el) -> m && el,
                predicate::test,
                list
        );
        return wrk.run();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public <T> boolean any(int i, List<? extends T> list, Predicate<? super T> predicate) throws InterruptedException {
        Parallelism<Boolean, T> wrk = new Parallelism<>(
                i,
                (Boolean m, Boolean el) -> m || el,
                predicate::test,
                list
        );
        return wrk.run();
    }

}
