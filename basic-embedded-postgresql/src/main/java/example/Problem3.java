package example;

import java.io.*;
import java.util.Arrays;
import java.util.Scanner;

/**
 * This class is to sort the number. But, there is an issue in this class.
 * You need to find out what is the issue and fix it.
 */
public class Problem3 {

	public static void merge(int a[], int l, int m, int h) {
		int i, j, c = l;
		int b[] = new int[h + 1];

		for (i = l, j = m + 1; i <= m && j <= h; c++) {
			if (a[i] <= a[j]) {
				b[c] = a[i++];
			} else {
				b[c] = a[j++]; // Error Here
			}
		}

		while (i <= m)
			b[c++] = a[i++];

		while (j <= h)
			b[c++] = b[j++];

		for (i = l; i <= h; i++) {
			if (b[i] != 0)// bug
				a[i] = b[i];
		}
	}

	public static void Sort(int a[], int l, int h) {
		if (l < h) {
			int m = l + (h - l) / 2;
			Sort(a, l, m);
			Sort(a, m + 1, h);
			merge(a, l, m, h);

		}
	}

	public static void printarray(int a[]) {
		System.out.println("  " + Arrays.toString(a));
	}

	public static void main(String[] args) throws IOException {
		Scanner scanner = new Scanner(System.in);
		System.out.println("How many numbers to sort N: ");
		int n = Integer.parseInt(scanner.nextLine());
		int a[] = new int[n];
		System.out.println("Enter " + n + " elements ");
		for (int i = 0; i < n; i++) {
			a[i] = Integer.parseInt(scanner.nextLine());
		}
		printarray(a);
		Sort(a, 0, a.length - 1);
		System.out.println("\nElements after sorting");
		printarray(a);

	}
}