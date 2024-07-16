package example;
/**
 * Copyright 2024 Serguei Kouzmine
 */

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import java.util.HashMap;
import java.util.Map;

import org.junit.Test;

// Intersection of Two Linked Lists
// based on https://github.com/FreeTymeKiyan/LeetCode-Sol-Resa
// see also:
// https://github.com/Seanforfun/Algorithm-and-Leetcode/blob/master/leetcode/160.%20Intersection%20of%20Two%20Linked%20Lists.md
public class LinkedListsIntersectionTest {

	private boolean debug = true;

	private static class ListNode {
		int val;
		ListNode next;

		ListNode(int x) {
			val = x;
			next = null;
		}

		@Override
		public boolean equals(Object other) {
			if (this == other)
				return true;

			if (other == null)
				return false;

			if (other.getClass() != this.getClass())
				return false;

			ListNode otherResult = (ListNode) other;
			return this.val == otherResult.val;
		}

		@Override
		public int hashCode() {
			// NOTE: poor hash code
			return this.val;
		}

	}

	@Test
	public void test1() throws Exception {
		int[] vals1 = new int[] { 4, 1, 8, 4, 5 };
		int[] vals2 = new int[] { 5, 6, 1, 8, 4, 5 };
		ListNode nodes1 = createListNodes(vals1);
		ListNode nodes2 = createListNodes(vals2);

		ListNode intersect = getIntersectionNode(nodes1, nodes2);
		if (intersect != null) {
			System.err.println("Intersect element: " + intersect.val);
		} else {
			System.err.println("No intersect");
		}
		// TODO: Assert
		assertThat(containsListNode(nodes1, intersect), is(true));
		assertThat(containsListNode(nodes2, intersect), is(true));
	}

	// Assert
	private boolean containsListNode(ListNode nodes, ListNode node_to_find) {
		while (nodes != null) {
			if (nodes.val == node_to_find.val) {
				if (debug)
					System.err.println("Found node: " + nodes.val);
				return true;
			}
			System.err.println("advancing to next element of: " + nodes.val);
			nodes = nodes.next;
		}
		return false;
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

	// calculate the size of the lists
	// since it is not double linked, can not go to the end and check

	public ListNode getIntersectionNode(ListNode nodes1, ListNode nodes2) {
		if (nodes1 == null || nodes2 == null)
			return null;
		int lenA = length(nodes1);
		if (debug)
			System.err.println("lenA: " + lenA);
		int lenB = length(nodes2);
		if (debug)
			System.err.println("lenB: " + lenB);

		int diff = Math.abs(lenA - lenB);
		if (debug)
			System.err.println("diff: " + diff);

		if (lenA > lenB) {
			if (debug)
				System.err.println("Walk nodes1: ");

			while (diff-- > 0)
				nodes1 = nodes1.next;
		} else {
			if (debug)
				System.err.println("Walk nodes2: ");
			while (diff-- > 0)
				nodes2 = nodes2.next;
		}
		if (debug)
			System.err.println(String.format("Walk both: %d %d", nodes1.val, nodes2.val));

		for (; nodes1 != null && nodes2 != null; nodes1 = nodes1.next, nodes2 = nodes2.next)
			// TODO: implement equals and hashcode (hashcode one needs to figure
			// out)
			// if (nodes1.equals(nodes2))
			if (nodes1.val == nodes2.val)
				return nodes1;
		return null;
	}

	private int length(ListNode nodes) {
		if (nodes == null)
			return 0;
		int length = 0;
		while (nodes != null) {
			length++;
			// if (debug)
			// System.err.println("advancing to next element of: " +
			// nodes.val);
			nodes = nodes.next;
		}

		return length;
	}

	private Map<Integer, ListNode> lengthAndLastNode(ListNode nodes) {
		Map<Integer, ListNode> result = new HashMap<>();
		if (nodes == null) {
			result.put(0, null);
		}
		int length = 0;
		ListNode node = nodes;
		ListNode last = null;
		while (node != null) {
			length++;
			last = node;
			nodes = node.next;
		}
		result.put(length, last);
		return result;
	}

}
