package ru.ifmo.rain.pinchuk.javaadv.udp;

import info.kgeorgiy.java.advanced.hello.HelloServer;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.DatagramChannel;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.charset.StandardCharsets;
import java.util.*;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

public class HelloServerImpl implements HelloServer {
    private ExecutorService worker;
    private Selector selector;
    private DatagramChannel channel;
    private ByteBuffer recvbuf;
    private SelectionKey key;
    private final int BUFFER_SIZE = 1204;
    private final int MAX_IN_QUEUE = 128;
    private final Queue<SendTask> sendQueue = new LinkedList<>();

    private static class SendTask {
        public final String msg;
        public final SocketAddress addr;

        public SendTask(String msg, SocketAddress addr) {
            this.msg = msg;
            this.addr = addr;
        }
    }

    /**
     * Program's entry point, for standalone usage
     * @param args program's arguments
     */

    public static void main(String... args) {
        if (args.length != 2) {
            System.out.println("Illegal argument count. \nUsage: HelloServer port threads");
            return;
        }

        if (Arrays.stream(args).anyMatch(Objects::isNull)) {
            System.out.println("None of args can be null\n");
            return;
        }

        int port, threads;
        try {
            port = Integer.parseInt(args[0]);
            threads = Integer.parseInt(args[1]);
        } catch(NumberFormatException e) {
            System.out.println("Bad number format");
            return;
        }

        final HelloServerImpl hs = new HelloServerImpl();
        hs.start(port, threads);
    }

    private void event(SelectionKey key) {
        final DatagramChannel chan = (DatagramChannel) key.channel();

        if ((key.readyOps() & SelectionKey.OP_WRITE) != 0 && !sendQueue.isEmpty()) {
            SendTask task = sendQueue.poll();

            recvbuf.clear();
            recvbuf.put(task.msg.getBytes(StandardCharsets.UTF_8));
            recvbuf.flip();

            try {
                chan.send(recvbuf, task.addr);
            } catch (IOException e) {
                sendQueue.add(task);

                System.out.println("Can't send");
                e.printStackTrace();
            }
        }

        if ((key.readyOps() & SelectionKey.OP_READ) != 0 && sendQueue.size() < MAX_IN_QUEUE) {
            try {
                recvbuf.clear();
                SocketAddress addr = chan.receive(recvbuf);
                String msg = new String(recvbuf.array(), 0, recvbuf.position());

                sendQueue.offer(new SendTask("Hello, " + msg, addr));
            } catch (IOException e) {
                System.out.println("Can't receive");
                e.printStackTrace();
            }
        }
    }

    private boolean mainLoop() {
        final int interested =
                (sendQueue.size() >= MAX_IN_QUEUE ? 0 : SelectionKey.OP_READ) |
                        (sendQueue.size() == 0 ? 0 : SelectionKey.OP_WRITE);
        key.interestOps(interested);

        try {
            selector.select();
        } catch (IOException e) {
            e.printStackTrace();
            return false;
        }

        for (final Iterator<SelectionKey> i = selector.selectedKeys().iterator(); i.hasNext(); ) {
            final SelectionKey key = i.next();

            event(key);

            i.remove();
        }
        return true;
    }

    private void run(int port, int threads) {
        recvbuf = ByteBuffer.allocate(BUFFER_SIZE);

        try (Selector selector = Selector.open();
             DatagramChannel channel = DatagramChannel.open()) {
            channel.bind(new InetSocketAddress(port));
            channel.configureBlocking(false);
            key = channel.register(selector, SelectionKey.OP_READ);

            this.selector = selector;
            this.channel = channel;

            synchronized (this) {
                this.notifyAll();
            }

            while (mainLoop() && !Thread.currentThread().isInterrupted());

            sendQueue.clear();
        } catch (IOException e) {
            System.err.println("Can't open selector or channel");
        }
    }

    /**
     * Program's entry point
     * @param port port to bind to
     * @param threads number of threads to use [dummy for the nonblocking version]
     */

    @Override
    public void start(int port, int threads) {
        if (worker != null) {
            throw new IllegalStateException("Run called multiple times");
        }

        worker = Executors.newSingleThreadExecutor();
        worker.submit(()->run(port, threads));

        try {
            synchronized (this) {
                this.wait();
            }
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
    }

    /**
     * Tries its best to close all reosurces.
     */
    @Override
    public void close() {
        try {
            selector.close();
            channel.close();
        } catch(IOException e) {}

        worker.shutdownNow();

        try {
            worker.awaitTermination(10, TimeUnit.SECONDS);
        }catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }

        worker = null;
    }
}
