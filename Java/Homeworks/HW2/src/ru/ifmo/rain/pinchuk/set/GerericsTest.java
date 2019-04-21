package ru.ifmo.rain.pinchuk.set;

public class GerericsTest {
    static class GenClass<T> {
        T z() {
            return (T) null;
        }
    }

    public static void main(String[] args) {
        GenClass<Float> t = new GenClass<>();

        Float f = t.z();
    }
}
