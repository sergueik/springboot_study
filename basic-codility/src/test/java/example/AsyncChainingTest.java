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
import java.util.concurrent.CompletionStage;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.RejectedExecutionException;
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
import java.util.function.Function;
import java.util.function.Supplier;

// origin: https://hellokoding.com/java-8-completablefuture-supplyasync-tutorial-with-examples/
// https://github.com/hellokoding/hellokoding-courses
public class AsyncChainingTest {

	private CompletableFuture<String> completableFuture;
	private CompletableFuture<Void> procedureFuture;
	private Supplier<String> supplier = null;
	private Function<String, String> function = null;
	private final String text1 = "message1";
	private final String text2 = "message2";

	@Test
	public void test1() throws ExecutionException, InterruptedException {
		supplier = () -> text1;
		completableFuture = CompletableFuture.supplyAsync(supplier);
		assertThat(completableFuture.get(), is(text1));
	}

	@Test
	public void test2() throws ExecutionException, InterruptedException {
		supplier = () -> text1;
		function = (String s) -> s.concat(text2);
		completableFuture = CompletableFuture.supplyAsync(supplier);
		completableFuture = completableFuture.thenApplyAsync(function);
		assertThat(completableFuture.get(), is(text1.concat(text2)));
	}

	@Test
	// TODO: java.util.concurrent.RejectedExecutionException
	public void test3() throws ExecutionException, InterruptedException, RejectedExecutionException {
		supplier = () -> text1;
		function = (String s) -> s.concat(text2);

		ExecutorService executorService = Executors.newFixedThreadPool(2);
		completableFuture = CompletableFuture.supplyAsync(supplier, executorService).thenApplyAsync(function,
				executorService);


		assertThat(completableFuture.get(), is(text1.concat(text2)));
		shutdownAndAwaitTermination(executorService);
	}

	@Test
	public void test4() throws ExecutionException, InterruptedException {
		supplier = () -> text1;
		// TODO: how to decompose
		Function<String, CompletionStage<String>> function2 = (String s) -> CompletableFuture
				.supplyAsync(() -> s.concat(text2));
		completableFuture = CompletableFuture.supplyAsync(supplier).thenComposeAsync(function2);

		assertThat(completableFuture.get(), is(text1.concat(text2)));
	}

	@Test
	public void test5() throws ExecutionException, InterruptedException {
		supplier = () -> text1;
		Supplier<String> supplier2 = () -> text2;
		CompletableFuture<String> completableFuture1 = CompletableFuture.supplyAsync(supplier);
		CompletableFuture<String> completableFuture2 = CompletableFuture.supplyAsync(supplier2);

		completableFuture = completableFuture1.thenCombineAsync(completableFuture2, (s1, s2) -> s1.concat(s2));

		assertThat(completableFuture.get(), is(text1.concat(text2)));
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
		supplier = () -> text1;
		completableFuture = CompletableFuture.supplyAsync(supplier);
		procedureFuture = completableFuture.thenAcceptAsync(System.out::println);
		assertThat(procedureFuture.get(), nullValue());
	}

	@Test
	public void test10() throws ExecutionException, InterruptedException {
		supplier = () -> text1;
		final Runnable action = () -> System.out.println("!");
		completableFuture = CompletableFuture.supplyAsync(supplier);

		procedureFuture = completableFuture.thenRunAsync(action);

		assertThat(procedureFuture.get(), nullValue());
	}

	@Test
	@SuppressWarnings("unused")
	// https://www.baeldung.com/java-exceptions-completablefuture
	// NOTE: Method test11() should be public
	// org.junit.runners.model.InvalidTestClassError
	public void test11() throws ExecutionException, InterruptedException {
		String input = null;
		String result = null;
		try {
			result = CompletableFuture.supplyAsync(() -> {
				// NOTE: Null pointer access: The variable input can only be
				// null at
				// this location
				if (input == null || input.isEmpty()) {
					throw new IllegalArgumentException("Supplied empty input");
				}
				// NOTE: the "return" code in supplier is dead
				return input;
			}).handle((data, e) -> {
				if (e == null) {
					return data;
				} else {
					System.err.println("Processing exception: " + e.toString());
					// Processing exception:
					// java.util.concurrent.CompletionException:
					// java.lang.IllegalArgumentException:
					// Supplied empty input
					return "default";
				}
			}).get();
		} catch (Exception e) {
			// should never reach this code
			System.err.println("Exception: " + e.toString());
			result = null;
		}
		assertThat(result, is("default"));
	}

	@Test
	@SuppressWarnings("unused")
	public void test12() throws ExecutionException, InterruptedException {
		String input = null;
		String result = null;
		try {
			result = CompletableFuture.supplyAsync(() -> {
				if (input == null || input.isEmpty()) {
					throw new IllegalArgumentException("Supplied empty input");
				}
				// NOTE: the "return" code in supplier is dead
				return input;
			}).exceptionally((Throwable e) -> "default").get();
		} catch (Exception e) {
			// should never reach this code
			System.err.println("Exception: " + e.toString());
			result = null;
		}
		assertThat(result, is("default"));
	}

	@Test
	public void test13() throws ExecutionException, InterruptedException {
		final String input = "";
		String result = null;
		final String exceptionType = "java.util.concurrent.ExecutionException";
		final String causeExceptionType = "java.lang.IllegalArgumentException.IllegalArgumentException";
		try {
			CompletableFuture.supplyAsync(() -> {
				if (input.isEmpty())
					throw new IllegalArgumentException("Supplied empty input");
				return "";
			}).whenComplete((String data, Throwable exception) -> {
				return;
			});
		} catch (Exception e) {

			System.err.println("Exception: " + e.toString());
			// NOTE:
			// assertThat(e.getClass(), is(ExecutionException.class));
			// fail to compile:
			// The method assertThat(T, Matcher<? super T>)
			// in the type MatcherAssert
			// is not applicable for the arguments
			// (Class<capture#1-of ? extends ExecutionException>,
			// Matcher<ExecutionException>)

			assertThat(e.getClass().getCanonicalName(), is(exceptionType));
			assertThat(e.getCause().getClass().getCanonicalName(), is(causeExceptionType));

		}

		final Stack<String> data = new Stack<>();
		try {
			result = data.peek();
			data.pop();
		} catch (Exception e) {
			System.err.println("Exception: " + e.toString());
			// java.util.EmptyStackException
		}
		assertThat(result, nullValue());
	}

	// origin: shutdownAndAwaitTermination example in
	// https://docs.oracle.com/javase%2F8%2Fdocs%2Fapi%2F%2F/java/util/concurrent/ExecutorService.html
	// see also:
	// https://github.com/hellokoding/hellokoding-courses/blob/master/java-examples/java-core/src/main/java/com/hellokoding/java/concurrent/ConcurrentUtils.java#L8
	// com.hellokoding.java.concurrent.ConcurrentUtils.stop;
	public static void shutdownAndAwaitTermination(ExecutorService pool) {

		final int timeout = 2;
		pool.shutdown(); // Disable new tasks from being submitted

		try {
			// Wait a while for existing tasks to terminate
			if (!pool.awaitTermination(timeout, TimeUnit.SECONDS)) {
				pool.shutdownNow(); // Cancel currently executing tasks

				// Wait a while for tasks to respond to being cancelled
				if (!pool.awaitTermination(timeout, TimeUnit.SECONDS))
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
