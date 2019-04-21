package ru.ifmo.rain.pinchuk.walk;

public class FnvHash {
    private int curHash = 0x811c9dc5;

    public void update(byte [] data, int length) {
        for (int i = 0; i < length; i++) {
            curHash = (curHash * 0x01000193) ^ (data[i] & 0xff);
        }
    }

    public int getHash() {
        return curHash;
    }
}
