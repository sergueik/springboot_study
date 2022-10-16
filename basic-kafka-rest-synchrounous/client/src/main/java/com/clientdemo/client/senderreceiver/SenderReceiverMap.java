package com.clientdemo.client.senderreceiver;

import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

public class SenderReceiverMap<T, V> {
    private final ConcurrentMap<T, SenderReceiver> senderReceiverConcurrentMap;

    public SenderReceiverMap() {
        senderReceiverConcurrentMap = new ConcurrentHashMap<>();
    }

    public Thread add(T id) {
        return add(id, 0L);
    }

    public Thread add(T id, Long timeout) {
        SenderReceiver<V> responseWait = new SenderReceiver<V>(timeout);
        senderReceiverConcurrentMap.put(id, responseWait);
        Runnable task = responseWait::receive;
        return new Thread(task);
    }

    public SenderReceiver<V> get(T id) {
        return senderReceiverConcurrentMap.get(id);
    }

    public Boolean containsKey(T id) {
        return senderReceiverConcurrentMap.containsKey(id);
    }

    public SenderReceiver remove(T id) {
        return senderReceiverConcurrentMap.remove(id);
    }

}
