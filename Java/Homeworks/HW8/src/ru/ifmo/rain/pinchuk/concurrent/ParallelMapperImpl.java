package ru.ifmo.rain.pinchuk.concurrent;

import info.kgeorgiy.java.advanced.mapper.ParallelMapper;

import java.util.*;
import java.util.function.Function;

public class ParallelMapperImpl implements ParallelMapper {
    private final List<Worker> workers;
    private final Queue<Task> queue = new LinkedList<>();

    private static class Worker extends Thread {
        final Queue<Task> queue;

        Worker(Queue<Task> queue) {
            this.queue = queue;
        }

        public void run() {
            try {
                while (true) {
                    Task t;
                    synchronized (queue) {
                        while (queue.isEmpty()) {
                            queue.wait();
                        }

                        t = queue.remove();
                        queue.notifyAll();

                    }
                    t.run();
                }
            } catch (InterruptedException e) {
                interrupt();
            }
        }
    }

    private static class Task<T, R> {
        Boolean done = false;
        R result = null;

        final T data;
        final Function<? super T, ? extends R> function;

        Task(Function<? super T, ? extends R> function, T data) {
            this.data = data;
            this.function = function;
        }

        synchronized void run() {
            result = function.apply(data);
            done = true;
            this.notifyAll();
        }
    }

    public ParallelMapperImpl(int threads) {
        if (threads < 0) {
            throw new IllegalArgumentException("Thread count cannot be negative");
        }

        workers = Arrays.asList(new Worker[threads]);

        for (int i = 0; i < threads; i++) {
            workers.set(i, new Worker(queue));
            workers.get(i).start();
        }
    }

    @Override
    public <T, R> List<R> map(Function<? super T, ? extends R> function, List<? extends T> list) throws InterruptedException {
        ArrayList<Task<T, R>> tasks = new ArrayList<>();
        ArrayList<R> results = new ArrayList<>();

        for (T el : list) {
            tasks.add(new Task<>(function, el));
        }

        for(Task<T,R> t : tasks) {
            synchronized (queue) {
                queue.add(t);
                queue.notifyAll();
            }
        }

        for (Task<T, R> t : tasks) {
            synchronized (t) {
                while (!t.done) {
                    t.wait();
                }
                results.add(t.result);
            }
        }

        return results;
    }

    @Override
    public void close() {
        boolean interrupted = false;
        for (Worker w : workers) {
            w.interrupt();
        }
        for (Worker w : workers) {
            try {
                w.join();
            } catch (InterruptedException e) {
                Thread.interrupted();
                interrupted = true;
            }
        }
        if (interrupted) {
            Thread.currentThread().interrupt();
        }
    }
}
