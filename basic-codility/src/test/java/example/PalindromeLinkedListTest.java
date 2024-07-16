package example;
/**
 * Copyright 2024 Serguei Kouzmine
 */

import java.util.ArrayList;
import java.util.List;

import org.junit.Test;

public class PalindromeLinkedListTest {

	private boolean debug = false;

	private static class ListNode {
		int val;
		ListNode next;

		ListNode(int x) {
			val = x;
			next = null;
		}
	}

	@Test
	public void test1() throws Exception {
		int[] vals = new int[] { 1, 2, 3, 4, 5 };
		ListNode nodes = createListNodes(vals);
		printListNodes(nodes);
		System.err.println("is palindrome: " + isPalindromeNodes(nodes));
	}

	@Test
	public void test2() throws Exception {
		int[] vals = new int[] { 1, 2, 3, 4, 5, 4, 3, 2, 1 };
		ListNode nodes = createListNodes(vals);
		printListNodes(nodes);
		System.err.println("is palindrome: " + isPalindromeNodes(nodes));
	}

	@Test
	public void test3() throws Exception {
		int[] vals = new int[] { 1 };
		ListNode nodes = createListNodes(vals);
		printListNodes(nodes);
		System.err.println("is palindrome: " + isPalindromeNodes(nodes));
	}

	private ListNode createListNodes(int[] data) {
		ListNode next_node = null;
		ListNode new_node = null;
		for (int index = data.length - 1; index >= 0; index--) {
			int val = data[index];
			if (debug)
				System.err.println("adding to tail: " + val);

			new_node = new ListNode(val);
			if (index != data.length - 1) {
				new_node.next = next_node;
			}
			next_node = new_node;
		}

		return new_node;
	}

	private boolean isPalindromeNodes(ListNode nodes) {

		if (nodes == null || nodes.next == null)
			return true;

		ListNode reversedNodes = reverseList(nodes);
		while (nodes != null) {
			if (nodes.val != reversedNodes.val)
				return false;
			nodes = nodes.next;
			reversedNodes = reversedNodes.next;
		}
		return true;
	}

	private void printListNodes(ListNode nodes) {
		List<Integer> results = new ArrayList<>();
		while (nodes != null) {
			results.add(nodes.val);
			nodes = nodes.next;
		}
		System.err.println(results);
	}

	private ListNode reverseList(ListNode nodes) {
		ListNode new_nodes = null;
		ListNode next_node = null;
		while (nodes != null) {
			if (debug)
				System.err.println("prepare to advance to next element of: " + nodes.val);
			next_node = nodes.next;
			// swap
			nodes.next = new_nodes;
			new_nodes = nodes;
			if (debug)
				System.err.println("advancing to next element of: " + nodes.val);
			nodes = next_node;
		}
		return new_nodes;
	}

}
