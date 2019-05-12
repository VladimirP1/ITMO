package ru.ifmo.rain.pinchuk.javaadv.udp;

import java.net.InetSocketAddress;

public class ThreadManager {
    private int queriesTotal;
    private int queriesCnt;
    private int threadId;
    private String queryPrefix;

    private InetSocketAddress host;

    public InetSocketAddress getHost() {
        return host;
    }

    public String getCurQuery() {
        if (queriesCnt > queriesTotal) {
            return null;
        }
        return queryPrefix + threadId + "_" + (queriesCnt - 1);
    }

    public void nextQuery() {
        queriesCnt++;
    }

    public int getThreadId() {
        return  threadId;
    }

    ThreadManager(int id, InetSocketAddress host, String prefix, int total) {
        queriesTotal = total;
        queriesCnt = 0;
        threadId = id;
        queryPrefix = prefix;
        this.host = host;
    }
}
