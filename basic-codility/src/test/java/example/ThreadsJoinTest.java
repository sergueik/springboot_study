package example;
/**
 * Copyright 2024 Serguei Kouzmine
 */

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

// better matchers
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.hasKey;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.hasKey;

import java.util.Set;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;

// https://www.baeldung.com/java-thread-join
public class ThreadsJoinTest {

	private boolean debug = true;

	public static class SampleThread extends Thread {
		public int processingCount = 0;

		SampleThread(int processingCount) {
			this.processingCount = processingCount;
			System.err.println("Thread Created");
		}

		@Override
		public void run() {
			System.err.println("Thread " + this.getName() + " started");
			while (processingCount > 0) {
				try {
					Thread.sleep(1000);
				} catch (InterruptedException e) {
					System.err.println("Thread " + this.getName() + " interrupted");
				}
				processingCount--;
				System.err.println("Inside Thread " + this.getName() + ", processingCount = " + processingCount);
			}
			System.err.println("Thread " + this.getName() + " exiting");
		}
	}

	@Test
	public void test1() throws InterruptedException {
		Thread sampleThread = new SampleThread(1);
		sampleThread.start();
		System.err.println("Invoking join");
		sampleThread.join(0);
		System.err.println("Returned from join");
		assertFalse(sampleThread.isAlive());
	}

	@Test
	public void test2() throws InterruptedException {
		Thread sampleThread = new SampleThread(10);
		sampleThread.start();
		sampleThread.join(1000);
		assertTrue(sampleThread.isAlive());
	}
}
