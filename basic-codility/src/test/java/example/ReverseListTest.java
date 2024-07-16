package example;
/**
 * Copyright 2024 Serguei Kouzmine
 */

import java.util.ArrayList;
import java.util.List;

import org.junit.Test;

public class ReverseListTest {

	private boolean debug = true;

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

		ListNode reversedNodes = reverseList(nodes);
		printListNodes(reversedNodes);
		ListNode doublyReversedNodes = reverseList(reversedNodes);
		printListNodes(doublyReversedNodes);

		// TODO: Assert
		// assertThat(containsListNode(nodes, intersect), is(true));
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

}
