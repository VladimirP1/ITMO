package ru.ifmo.rain.pinchuk.javaadv.udp;

import info.kgeorgiy.java.advanced.hello.HelloClient;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.util.*;

public class HelloClientImpl implements HelloClient {
    private final Set<QueryThread> workingThreads = new TreeSet<>();
    private final int SELECTOR_TIMEOUT_MS = 50;

    private void maybeNewJob(QueryThread thread, boolean next) {
        ThreadManager m = (ThreadManager) thread.getAttachment();

        if (next) {
            m.nextQuery();
        }

        String query = m.getCurQuery();

        if (query != null) {
            workingThreads.remove(thread);
            thread.doQuery(query, m.getHost(), true);
            workingThreads.add(thread);
        } else {
            workingThreads.remove(thread);
            thread.close();
        }
    }

    /**
     * Program's entry point, for standalone usage
     *
     * @param args program's arguments
     */

    public static void main(String[] args) {
        if (args.length != 5) {
            System.out.println("Illegal argument count. \nUsage: HelloClient host port prefix threads queries");
            return;
        }

        if (Arrays.stream(args).anyMatch(Objects::isNull)) {
            System.out.println("None of args can be null\n");
            return;
        }

        String host = args[0], prefix = args[2];

        int port, threads, queries;

        try {
            port = Integer.parseInt(args[1]);
            threads = Integer.parseInt(args[3]);
            queries = Integer.parseInt(args[4]);
        } catch (NumberFormatException e) {
            System.out.println("Bad number format");
            return;
        }

        new HelloClientImpl().run(host, port, prefix, threads, queries);
    }

    /**
     * Program's entry point
     *
     * @param host    The host to send to
     * @param port    The destination port
     * @param prefix  Prefix of query strings
     * @param threads The number of threads to use
     * @param queries Total number of requests for each thread
     */

    @Override
    public void run(String host, int port, String prefix, int threads, int queries) {
        Selector selector;
        try {
            selector = Selector.open();
        } catch (IOException e) {
            System.err.println("Can't open selector");
            return;
        }

        for (int i = 0; i < threads; ++i) {
            QueryThread thread = new QueryThread(selector, i);
            ThreadManager manager = new ThreadManager(i, new InetSocketAddress(host, port), prefix, queries);
            thread.setAttachment(manager);

            maybeNewJob(thread, true);

            workingThreads.add(thread);
        }


        while (!workingThreads.isEmpty()) {
            while (!workingThreads.isEmpty()) {
                QueryThread thread = workingThreads.iterator().next();
                if (!thread.getTlExceeded()) {
                    break;
                }

                maybeNewJob(thread, false);

                //System.out.println("TL exceeded");
            }

            try {
                selector.select(SELECTOR_TIMEOUT_MS);
            } catch (IOException e) {
                System.out.println("Select failes");
                e.printStackTrace();
                break;
            }

            for (final Iterator<SelectionKey> i = selector.selectedKeys().iterator(); i.hasNext(); ) {
                final SelectionKey key = i.next();
                try {
                    QueryThread thread = (QueryThread) key.attachment();
                    thread.event();

                    if (thread.isIdle()) {
                        maybeNewJob(thread, true);
                    }
                } finally {
                    i.remove();
                }
            }
        }

    }
}
