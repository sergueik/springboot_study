package example;
/**
 * Copyright 2024 Serguei Kouzmine
 */

import static org.junit.Assert.*;

import java.io.IOException;
import java.util.Stack;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;

public class ChainingExceptionTest {

	@Ignore
	@Test
	public void test1() throws Exception {
		try {
			divide(5, 0);
		} catch (ArithmeticException ae) {
			System.err.println("caught : " + ae);
			throw new Exception("Exception to prevent finally");
		} finally {
			System.err.println("finally.");
		}
	}

	public static void divide(int a, int b) {
		if (b == 0) {
			ArithmeticException ae = new ArithmeticException("top layer");
			ae.initCause(new IOException("cause"));
			throw ae;
		} else {
			System.out.println(a / b);
		}
	}

}
