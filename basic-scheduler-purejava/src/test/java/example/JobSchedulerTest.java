package example;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import java.util.ArrayList;
import java.util.List;

import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.doNothing;

import static org.junit.Assert.assertTrue;
import java.util.Date;

public class JobSchedulerTest {

	private Runnable task;
	private JobScheduler sut;
	private final long interval = 5000L;
	private final Date laterDate = new Date(
			System.currentTimeMillis() + interval);

	@BeforeClass
	public static void setUpClass() {
	}

	@AfterClass
	public static void tearDownClass() {
	}

	@Before
	public void setUp() {
		sut = new JobScheduler(0);
		task = mock(Runnable.class);
		// https://www.baeldung.com/mockito-void-methods
		// Mockito.any(Object.class)
		// The method when(T) in the type Mockito is not applicable for the
		// arguments (void)
		// when(task.run()).thenReturn();
		doNothing().when(task).run();
	}

	@After
	public void tearDown() {
	}

	@Test
	public void test1() {
		sut.execute(task);
		verify(task, times(1)).run();
	}

	@Test
	public void test2() {
		sut.executeAt(task, laterDate);
		verify(task, never()).run();
	}

	@Test
	public void test3() {
		sut.executeAt(task, laterDate);
		try {
			Thread.sleep(2 * interval);
		} catch (InterruptedException e) {
		}
		verify(task, times(1)).run();
	}

	@Test
	public void test4() {
		sut.executeIn(task, interval);
		try {
			Thread.sleep(interval / 2);
		} catch (InterruptedException e) {
		}
		verify(task, times(0)).run();
	}

	@Test
	public void test5() {
		sut.executeIn(task, interval);
		try {
			Thread.sleep(2 * interval);
		} catch (InterruptedException e) {
		}
		verify(task, times(1)).run();
	}

	// NOTE: Time-consuming
	@Test
	public void test6() {
		sut.executeIn(task, interval);
		sut.executeAtAndRepeat(task, laterDate, JobScheduler.PER_SECOND);
		try {
			Thread.sleep(interval + JobScheduler.PER_MINUTE);
		} catch (InterruptedException e) {
		}
		verify(task, atLeast(59)).run();
	}
}
