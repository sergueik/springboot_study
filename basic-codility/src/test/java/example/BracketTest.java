package example;
/**
 * Copyright 2024 Serguei Kouzmine
 */

import static org.junit.Assert.*;

import java.util.Stack;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

public class BracketTest {
	String data = null;
	boolean res = false;

	@Test
	public void test1() {
		data = "{[(a)(fd)]}";
		res = isBalanced(data);
		assertTrue(res);
	}

	@Test
	public void test2() {
		data = "";
		res = isBalanced(data);
		assertTrue(res);
	}

	@Test
	public void test3() {
		data = "{";
		res = isBalanced(data);
		assertFalse(res);
	}

	@Test
	public void test4() {
		data = "{([])";
		res = isBalanced(data);
		assertFalse(res);
	}

	@Test
	public void test5() {
		data = "({[)]}";
		res = isBalanced(data);
		assertEquals(false, res);
	}

	@Test
	public void test6() {
		data = "(a[b]c{d}e)f";
		res = isBalanced(data);
		assertEquals(true, res);
	}

	public boolean isBalanced(String data) {
		Stack<Character> scanned = new Stack<>();
		for (int i = 0; i < data.length(); i++) {
			char ch = data.charAt(i);
			if (ch != '(' && ch != ')' && ch != '[' && ch != ']' && ch != '{' && ch != '}')
				// ignore other characters
				continue;
			if (ch == ')' || ch == ']' || ch == '}') {
				if (scanned.empty()) {
					return false;
				}
				char top = scanned.pop();
				if ((ch == ')' && top != '(') || (ch == ']' && top != '[') || (ch == '}' && top != '{')) {
					return false;
				}
			} else {
				scanned.push(ch);
			}
		}
		return scanned.empty();
	}

}
