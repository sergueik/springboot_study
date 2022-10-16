package com.clientdemo.client.senderreceiver;

import java.util.Date;
import java.util.Objects;
import java.util.concurrent.TimeoutException;

public class SenderReceiver<T> {
    private volatile T data;
    private final Long timeout;
    private final Date start;
    private TimeoutException timeoutException;

    private boolean transfer = true;

    public SenderReceiver() {
        this(0L);
    }

    public SenderReceiver(Long timeout) {
        this.start = new Date();
        this.timeout = timeout;
    }

    public synchronized void receive() {
        while (transfer) {
            if (timeout != 0 && start.before(new Date(System.currentTimeMillis() - timeout))) {
                timeoutException = new TimeoutException();
                Thread.currentThread().interrupt();
                return;
            }
            try {
                wait(timeout);
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        }
        Thread.currentThread().interrupt();
    }

    public synchronized void send(final T data) {
        transfer = false;
        this.data = data;
        notifyAll();
    }

    public T getData() throws TimeoutException {
        if (Objects.nonNull(timeoutException))
            throw timeoutException;
        return data;
    }
}
