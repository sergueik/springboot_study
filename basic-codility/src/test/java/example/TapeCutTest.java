package example;
/**
 * Copyright 2024 Serguei Kouzmine
 */

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.Set;
import java.util.Arrays;
import java.util.HashSet;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;

// https://app.codility.com/programmers/lessons/3-time_complexity/tape_equilibrium/
public class TapeCutTest {

	private boolean debug = true;

	// @Ignore
	@Test
	public void test4() {

		int[] data = { 3, 1, 2, 4, 3 };
		int res = bestCut(data);
		// assertTrue(res == 1);
	}

	public int bestCut(int[] A) {
		int total = 0; // total
		int left = 0;
		int best_diff = Integer.MAX_VALUE; // initial setting: Integer.MAX_VALUE
		int diff = 0;
		for (int i = 0; i < A.length; i++) {
			total = total + A[i];
		}
		for (int i = 0; i < A.length; i++) {
			if (i == A.length - 1)
				continue;
			left = left + A[i];
			System.err.println("left: " + left);
			diff = total - 2 * left; // the difference
			System.err.println("diff: " + diff);
			if (diff < 0)
				diff = -diff;
			best_diff = best_diff < diff ? best_diff : diff;
		}

		System.err.println("best diff: " + best_diff);
		return best_diff;
	}

}
