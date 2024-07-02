package example;
/**
 * Copyright 2024 Serguei Kouzmine
 */

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.hasKey;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.Socket;

import java.nio.file.Paths;
import java.time.Duration;
import java.util.Arrays;
import java.util.Stack;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.apache.commons.lang3.StringUtils;

import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.junit.Ignore;

import java.util.concurrent.TimeUnit;

// origin: https://hellokoding.com/java-8-completablefuture-supplyasync-tutorial-with-examples/
// https://github.com/hellokoding/hellokoding-courses
public class AsyncChainingTest {

	private CompletableFuture<String> completableFuture;
	private CompletableFuture<Void> procedureFuture;

	@Test
	public void test1() throws ExecutionException, InterruptedException {
		completableFuture = CompletableFuture.supplyAsync(() -> "message");
		assertThat(completableFuture.get(), is("message"));
	}

	@Test
	public void test2() throws ExecutionException, InterruptedException {
		completableFuture = CompletableFuture.supplyAsync(() -> "message1");
		completableFuture = completableFuture.thenApplyAsync((String s) -> s.concat(" message2"));
		assertThat(completableFuture.get(), is("message1 message2"));
	}

	@Test
	public void test3() throws ExecutionException, InterruptedException {
		ExecutorService executorService = Executors.newFixedThreadPool(2);

		completableFuture = CompletableFuture.supplyAsync(() -> "message1", executorService)
				.thenApplyAsync((s) -> s.concat(" message2"), executorService);

		shutdownAndAwaitTermination(executorService);

		assertThat(completableFuture.get(), is("message1 message2"));
	}

	@Test
	public void test4() throws ExecutionException, InterruptedException {
		completableFuture = CompletableFuture.supplyAsync(() -> "message1")
				.thenComposeAsync((String s) -> CompletableFuture.supplyAsync(() -> s.concat(" message2")));

		assertThat(completableFuture.get(), is("message1 message2"));
	}

	@Test
	public void test5() throws ExecutionException, InterruptedException {
		CompletableFuture<String> completableFuture1 = CompletableFuture.supplyAsync(() -> "message1");
		CompletableFuture<String> completableFuture2 = CompletableFuture.supplyAsync(() -> " message2");

		completableFuture = completableFuture1.thenCombineAsync(completableFuture2, (s1, s2) -> s1.concat(s2));

		assertThat(completableFuture.get(), is("message1 message2"));
	}

	/*
	 * @Test public void test7() { CompletableFuture<String> cf1 =
	 * CompletableFuture.supplyAsync(() -> "Future"); CompletableFuture<String>
	 * cf2 = CompletableFuture.supplyAsync(() -> " is awesome!");
	 * CompletableFuture<String> cf3 = CompletableFuture.supplyAsync(() -> "!");
	 * 
	 * @SuppressWarnings("rawtypes") CompletableFuture[] cfs = new
	 * CompletableFuture[3]; cfs[0] = cf1; cfs[1] = cf2; cfs[2] = cf3; // { cf1,
	 * cf2, cf3 };
	 * 
	 * CompletableFuture<Void> allCf = CompletableFuture.allOf(cfs);
	 * allCf.join();
	 * 
	 * String result =
	 * Arrays.stream(cfs).map(CompletableFuture::join).collect(Collectors.
	 * joining()); assertThat(result,is("Future is awesome!!")); }
	 */
	@Test
	public void test8() throws ExecutionException, InterruptedException {
		CompletableFuture<String> cf1 = CompletableFuture.supplyAsync(() -> "text1");
		CompletableFuture<String> cf2 = CompletableFuture.supplyAsync(() -> " text2");
		CompletableFuture<String> cf3 = CompletableFuture.supplyAsync(() -> " text3");

		CompletableFuture<Object> anyCf = CompletableFuture.anyOf(cf1, cf2, cf3);
		System.out.println(anyCf.get());

		assertThat(anyCf.isDone(), is(true));
	}

	@Test
	public void test9() throws ExecutionException, InterruptedException {
		completableFuture = CompletableFuture.supplyAsync(() -> "Future");
		procedureFuture = completableFuture.thenAcceptAsync(System.out::println);
		assertThat(procedureFuture.get(), nullValue());
	}

	@Test
	public void test10() throws ExecutionException, InterruptedException {
		completableFuture = CompletableFuture.supplyAsync(() -> "text");

		procedureFuture = completableFuture.thenRunAsync(() -> System.out.println("!"));

		assertThat(procedureFuture.get(), nullValue());
	}

	// origin: shutdownAndAwaitTermination example in
	// https://docs.oracle.com/javase%2F8%2Fdocs%2Fapi%2F%2F/java/util/concurrent/ExecutorService.html
	// see also:
	// https://github.com/hellokoding/hellokoding-courses/blob/master/java-examples/java-core/src/main/java/com/hellokoding/java/concurrent/ConcurrentUtils.java#L8
	// com.hellokoding.java.concurrent.ConcurrentUtils.stop;
	public static void shutdownAndAwaitTermination(ExecutorService pool) {

		pool.shutdown(); // Disable new tasks from being submitted

		try {
			// Wait a while for existing tasks to terminate
			if (!pool.awaitTermination(2, TimeUnit.SECONDS)) {
				pool.shutdownNow(); // Cancel currently executing tasks

				// Wait a while for tasks to respond to being cancelled
				if (!pool.awaitTermination(2, TimeUnit.SECONDS))
					System.err.println("Pool did not terminate");
			}
		} catch (InterruptedException ie) {
			// (Re-)Cancel if current thread also interrupted
			pool.shutdownNow(); 

			// Preserve interrupt status
			Thread.currentThread().interrupt();
		}
	}

}
