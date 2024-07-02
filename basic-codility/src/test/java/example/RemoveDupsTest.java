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
import org.junit.Ignore;
import org.junit.Test;

public class RemoveDupsTest {
	private boolean debug = true;

	@Ignore
	@Test
	public void test1() throws Exception {
		int[] data = new int[] { 1, 2, 3, 3, 4, 5, 7, 8, 8, 8, 8, 8, 9 };
		int res = RemoveDupsFromSorted(data, data.length - 1);
		assertTrue(res == 8);
	}

	private int RemoveDupsFromSorted(int[] arrNum, int num) {
		if (num < 2)
			return 0;
		int[] aTmp = new int[num];
		int b = 0;
		for (int a = 0; a < num - 1; a++) {
			if (arrNum[a] != arrNum[a + 1]) {
				aTmp[b] = arrNum[a];
				b++;
			}
			if (debug)
				System.err.println("a=" + a);
			aTmp[b] = arrNum[a + 1];
			b++;
		}
		for (int a = 0; a < b; a++) {
			arrNum[a] = aTmp[a];
		}
		return b;
	}

}
