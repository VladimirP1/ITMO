package ru.ifmo.rain.pinchuk.javaadv.udp;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.DatagramChannel;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.charset.StandardCharsets;

public class QueryThread implements Comparable<QueryThread> {
    private final int id;
    private InetSocketAddress host;
    private long timeRegistered;
    private SelectionKey key;
    private DatagramChannel channel;
    private String request;
    private ByteBuffer buf;
    private Object attachment;
    private State state = State.INIT;
    private final int TIME_LIMIT_MS = 1000;

    /**
     * Gets the attachment - the object, which has been associated with this thread by {@link #setAttachment(Object)}
     * @return the attachment value
     */

    public Object getAttachment() {
        return attachment;
    }

    /**
     * Sets the attachment - the object, which can later be retrieved by {@link #getAttachment()}
     * @param attachment the attachment to assign to this thread
     * */

    public void setAttachment(Object attachment) {
        this.attachment = attachment;
    }

    private enum State {
        INIT,
        WAIT,
        DONE
    }

    /**
     * Opens a DatagramChannel, sets it to nonblock mode and adds to selector.
     * @param selector the selector to add the channel to
     * @param id thread id, to differentiate threads with same creation time
     */

    public QueryThread(final Selector selector, int id) {
        this.id = id;
        try {
            channel = DatagramChannel.open();
            channel.configureBlocking(false);
            key = channel.register(selector, 0, this);

            buf = ByteBuffer.allocate(channel.socket().getReceiveBufferSize());
        } catch (IOException e) {
            System.out.println("Cannot create query 'thread'");
            e.printStackTrace();
        }
    }

    private void setState(State s) {
        state = s;
        switch (s) {
            case INIT:
                key.interestOps(SelectionKey.OP_WRITE);
                break;
            case WAIT:
                key.interestOps(SelectionKey.OP_READ);
                break;
            case DONE:
                key.interestOps(0);
                break;

        }

    }

    /**
     * Sets up for echo of string test to host.
     * @param query the string to send to host
     * @param host the destination host
     * @param updateTL wether to reset the time elapsed to zero or not.
     */

    public void doQuery(String query, InetSocketAddress host, boolean updateTL) {
        this.request = query;
        buf.clear();
        buf.put(query.getBytes(StandardCharsets.UTF_8));
        buf.flip();
        this.host = host;
        setState(State.INIT);
        if(updateTL) {
            timeRegistered = System.currentTimeMillis();
        }
    }

    public void event() {
        switch(state) {
            case INIT:
                if ((key.readyOps() & SelectionKey.OP_WRITE) != 0) {
                    try {
                        int sent = channel.send(buf, host);
                        if (sent > 0) {
                            setState(State.WAIT);
                        }
                    } catch (IOException e) {
                        System.out.println("Cannot send");
                    }
                }
                break;
            case WAIT:
                if ((key.readyOps() & SelectionKey.OP_READ) != 0) {
                    try {
                        buf.clear();
                        SocketAddress addr = channel.receive(buf);
                        if (addr != null) {
                            int len = buf.position();
                            buf.flip();
                            String response = new String(buf.array(), 0, len, StandardCharsets.UTF_8);

                            setState(State.DONE);

                            System.out.println(request + " | " + response);

                            if (!response.endsWith(request)) {
                                doQuery(request, host, false);
                            }
                        }
                    } catch (IOException e) {
                        System.out.println("Cannot recieve");
                    }
                }
                break;
            case DONE:

                break;
        }
    }

    /**
     * Closes all resources and sets idle state.
     */

    public void close() {
        state = State.DONE;
        try {
            if (channel != null) {
                channel.close();
            }
        } catch(IOException e) {}
    }

    /**
     * Checks the time limit.
     * @return True if the time limit has been exceeded. False otherwise.
     */

    public boolean getTlExceeded() {
        return TIME_LIMIT_MS + timeRegistered < System.currentTimeMillis();
    }

    /**
     * Checks if this thread is idle
     * @return True if the iternal state machine has finished its work. False otherwise.
     */

    public boolean isIdle() {
        return state == State.DONE;
    }

    /**
     * Compares QueryThreads by time limit end times.
     * @param query Right hand side of comparison
     * @return -1, 0, +1 by convention
     */
    @Override
    public int compareTo(QueryThread query) {
        int result = (int)Math.max(Math.min(this.timeRegistered - query.timeRegistered, 1), -1);

        return (result == 0) ? id - query.id : result;
    }
}
