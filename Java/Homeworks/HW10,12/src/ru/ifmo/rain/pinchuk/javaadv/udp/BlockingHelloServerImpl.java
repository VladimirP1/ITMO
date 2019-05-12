package ru.ifmo.rain.pinchuk.javaadv.udp;

import info.kgeorgiy.java.advanced.hello.HelloServer;

import java.io.IOException;
import java.net.*;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Objects;
import java.util.concurrent.*;

public class BlockingHelloServerImpl implements HelloServer {
    private DatagramSocket socket;
    private ExecutorService listener;
    private ExecutorService tellers;

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


        BlockingHelloServerImpl hs = new BlockingHelloServerImpl();

        hs.start(port, threads);
    }

    private void send(String message, SocketAddress dest) {
        byte[] msg = message.getBytes(StandardCharsets.UTF_8);
        DatagramPacket packet = new DatagramPacket(msg, msg.length, dest);
        try {
            socket.send(packet);
        } catch(IOException e) {
            System.err.println("Cannot send");
            e.printStackTrace();
        }

    }

    private void listen(int port) {
        byte buf[] = new byte[1024];

        try (DatagramSocket socket = new DatagramSocket(port)) {
            this.socket = socket;

            synchronized (this) {
                this.notifyAll();
            }

            while (!Thread.interrupted()) {
                DatagramPacket packet = new DatagramPacket(buf, buf.length);
                try {
                    socket.receive(packet);
                } catch(SocketException e) {
                    continue;
                }
                if ( packet.getAddress() == null) {
                    System.out.println("Timeout");
                    continue;
                }
                String response = new String(packet.getData(), 0, packet.getLength());

                tellers.submit(() -> send("Hello, " + response,
                        new InetSocketAddress(packet.getAddress(), packet.getPort())));

            }

        } catch (SocketException e) {
            System.err.println("Cannot create socket");
            e.printStackTrace();
        } catch (IOException e) {
            System.err.println("Cannot recieve");
            e.printStackTrace();
        }
    }

    /**
     * Program's entry point
     * @param port port to bind to
     * @param threads number of threads to use [dummy for the nonblocking version]
     */

    @Override
    public void start(int port, int threads) {
        if (listener != null) {
            throw new IllegalStateException("Run called multiple times");
        }

        listener = Executors.newSingleThreadExecutor();
        tellers = new ThreadPoolExecutor(threads, threads, 30, TimeUnit.SECONDS,
                new ArrayBlockingQueue<>(1000), new ThreadPoolExecutor.DiscardPolicy());

        listener.submit(() -> listen(port));

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
        listener.shutdownNow();
        tellers.shutdownNow();

        if (socket != null) {
            socket.close();
        }

        try {
            listener.awaitTermination(10, TimeUnit.SECONDS);
            tellers.awaitTermination(10, TimeUnit.SECONDS);
        }catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }

        listener = null;
    }
}
