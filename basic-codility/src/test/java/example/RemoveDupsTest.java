package example;
/**
 * Copyright 2024 Serguei Kouzmine
 */

import static org.junit.Assert.*;

import java.io.IOException;
import java.util.Arrays;
import java.util.Stack;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

// https://www.geeksforgeeks.org/java-program-to-check-whether-two-strings-are-anagram-of-each-other/
public class RemoveDupsTest {
	private boolean debug = false;

	@Test
	public void test1() throws Exception {
		debug = true;
		String data1 = "abda";
		String data2 = "bdaa";
		boolean res = areAnagram2(data1, data2);
		assertTrue(res);
	}

	private static int RemoveDupsFromSorted(int[] arrNum, int num) {
		if (num < 2)
			return 0;
		int[] aTmp = new int[num];
		int b = 0;
		for (int a = 0; a < num - 1; a++) {
			if (arrNum[a] != arrNum[a + 1]) {
				aTmp[b] = arrNum[a];
				b++;
			}
			aTmp[b] = arrNum[n - 1];
			b++;
		}
		for (int a = 0; a < b; a++) {
			arrNum[a] = aTmp[a];
		}
		return b;
	}

}
