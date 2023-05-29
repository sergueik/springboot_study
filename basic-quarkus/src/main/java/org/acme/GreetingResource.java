package org.acme;

//import org.eclipse.microprofile.config.inject.ConfigProperty;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

import io.micrometer.core.instrument.Tags;
import io.micrometer.core.instrument.Timer;
import org.jboss.logging.Logger;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.NoSuchElementException;

import io.micrometer.core.instrument.MeterRegistry;

@Path("/example")
@Produces(MediaType.TEXT_PLAIN)
public class GreetingResource {
    private static List<byte[]> memoryList = new ArrayList<>();

    private static final Logger LOG = Logger.getLogger(GreetingResource.class);
    LinkedList<Long> linkedList = new LinkedList<>();
    private final MeterRegistry registry;
//    @ConfigProperty(name = "target", defaultValue = "World")
//    String message;

//    @ConfigProperty(defaultValue = "Students", name="application.greeting.recipient")
//    String recipient;

    GreetingResource(MeterRegistry registry) {
        this.registry = registry;
        registry.gaugeCollectionSize("example.linkedList.size", Tags.empty(), linkedList);
    }

    @GET
    @Path("gauge/{number}")
    public Long checkListSize(long number) {
        if (number % 2 == 0) {
            // add even numbers to the list
            linkedList.add(number);
        } else {
            // remove items from the list for odd numbers
            try {
                number = linkedList.removeFirst();
            } catch (NoSuchElementException nse) {
                number = 0;
            }
        }
        return number;
    }

    @GET
    @Path("prime/{number}")
    public String checkIfPrime(long number) {
        if (number < 1) {
            registry.counter("example.prime.number", "type", "not-natural").increment();
            return "Only natural numbers can be prime numbers.";
        }
        if (number == 1 ) {
            registry.counter("example.prime.number", "type", "one").increment();
            return number + " is not prime.";
        }
        if (number % 2 == 0) {
            registry.counter("example.prime.number", "type", "even").increment();
            return number + " is not prime.";
        }

        if ( testPrimeNumber(number) ) {
            registry.counter("example.prime.number", "type", "prime").increment();
            return number + " is prime.";
        } else {
            registry.counter("example.prime.number", "type", "not-prime").increment();
            return number + " is not prime.";
        }
    }

    protected boolean testPrimeNumber(long number) {
        Timer timer = registry.timer("example.prime.number.test");
        return Boolean.TRUE.equals(timer.record(() -> {
            for (int i = 3; i < Math.floor(Math.sqrt(number)) + 1; i = i + 2) {
                if (number % i == 0) {
                    return false;
                }
            }
            return true;
        }));
    }


    @GET
    @Path("/hello/{name}")
    public String hello(String name) {
        LOG.info("hello");
        return "Hello " + name + "!";
    }

    @GET
    @Path("/addList/{number}")
    public String addList(int number) {

        increaseJvmCost(number);
        return "ArrayList now: " + memoryList + "!";
    }

    private static void allocateMemory() {
        byte[] data = new byte[10 * 1024 * 1024];
        memoryList.add(data);
    }

    public static void increaseJvmCost(int iterations) {
        for (int i = 0; i < iterations; i++) {
            allocateMemory();
        }
    }


}