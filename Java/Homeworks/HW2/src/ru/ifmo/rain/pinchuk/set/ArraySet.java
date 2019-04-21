package ru.ifmo.rain.pinchuk.set;

import java.util.*;


public class ArraySet<T extends Comparable<? super T>> extends AbstractSet<T> implements SortedSet<T> {

    private List<T> data;
    private Comparator<T> comparator;

    public ArraySet() {
        this(Collections.emptySet());
    }

    public ArraySet(Comparator<T> comparator) {
        this();
        this.comparator = comparator;
    }

    public ArraySet(Collection<T> collection) {
        this(collection, null);
    }

    public ArraySet(Collection<T> collection, Comparator<T> comparator) {
        Set<T> sorted = new TreeSet<T>(comparator);
        sorted.addAll(collection);

        data = new ArrayList<T>(sorted);
        this.comparator = comparator;
    }

    private ArraySet(List<T> data, Comparator<T> comparator) {
        this.data = data;
        this.comparator = comparator;
    }

    @Override
    public Iterator<T> iterator() {
        return Collections.unmodifiableList(data).iterator();
    }

    @Override
    public int size() {
        return data.size();
    }

    @Override
    public Comparator<? super T> comparator() {
        return comparator;
    }

    @Override
    public SortedSet<T> subSet(T fromElement, T toElement) {
        Comparator<? super T> comp = comparator == null ? Comparator.naturalOrder() : comparator;

        if (comp.compare(fromElement, toElement) > 0) {
            throw new IllegalArgumentException();
        }

        return range(fromElement, toElement, true, true);
    }

    @Override
    public SortedSet<T> headSet(T toElement) {
        return range(null, toElement, false, true);
    }

    @Override
    public SortedSet<T> tailSet(T fromElement) {
        return range(fromElement, null, true, false);
    }

    @Override
    public T first() {
        if (size() == 0) {
            throw new NoSuchElementException();
        }
        return data.get(0);
    }

    @Override
    public T last() {
        if (size() == 0) {
            throw new NoSuchElementException();
        }
        return data.get(size() - 1);
    }

    @SuppressWarnings("unchecked")
    @Override
    public boolean contains(Object elem) {
        if (elem == null) {
            throw new NullPointerException();
        }
        return Collections.binarySearch(data, (T) elem, comparator) >= 0;
    }

    private int upperBound(T elem) {
        int pos = Collections.binarySearch(data, elem, comparator);
        if (pos < 0) {
            return -(pos + 1);
        }
        return pos;
    }


    private SortedSet<T> range(T fromElement, T toElement, boolean start, boolean stop) {
        if (start && fromElement == null || stop && toElement == null) {
            throw new NullPointerException();
        }

        int from = start ? upperBound(fromElement) : 0;
        int to = stop ? upperBound(toElement) : size();

        List<T> newList;
        if (from != to) {
            newList = data.subList(from, to);
        } else {
            newList = Collections.emptyList();
        }

        return new ArraySet<T>(newList, comparator);
    }
}
