package example;
/**
 * Copyright 2024 Serguei Kouzmine
 */

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.Stack;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

public class BracketTest {

	@Test
	public void test1() {
		String data = "{[(a)(fd)]}";
		int res = isBalanced(data);
		assertTrue(res == 0);
	}

	@Test
	public void test2() {
		String data = "";

		int res = isBalanced(data);
		assertTrue(res == 0);
	}

	@Test
	public void test3() {
		String data = "({[)]}";

		int res = isBalanced(data);
		assertEquals(1, res );
	}

	public int isBalanced(String data) {
		Stack<Character> scanned = new Stack<>();
		for (int i = 0; i < data.length(); i++) {
			char ch = data.charAt(i);
			if (ch != '(' && ch != ')' && ch != '[' && ch != ']' && ch != '{' && ch != '}')
				continue;
			if (ch == ')' || ch == ']' || ch == '}') {
				if (scanned.empty()) {
					return 1;
				}
				char top = scanned.pop();
				if ((ch == ')' && top != '(') || (ch == ']' && top != '[') || (ch == '}' && top != '{')) {
					return 1;
				}
			} else {
				scanned.push(ch);
			}
		}
		return scanned.empty() ? 0 : 1;
	}

}
