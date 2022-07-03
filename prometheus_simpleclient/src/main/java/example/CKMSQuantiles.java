package example;

// The original implementation was copied from
// https://raw.githubusercontent.com/Netflix/ocelli/master/ocelli-core/src/main/java/netflix/ocelli/stats/CKMSQuantiles.java
// Revision d0357b8bf5c17a173ce94d6b26823775b3f999f6 from Jan 21, 2015.
// However, it has been heavily refactored in the meantime.

/*
 Copyright 2012 Andrew Wang (andrew@umbrant.com)

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
 */

import java.util.Arrays;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.ListIterator;

final class CKMSQuantiles {

	final Quantile[] quantiles;

	int n = 0;

	final LinkedList<Sample> samples = new LinkedList<Sample>();

	private final int compressInterval = 128;
	private int insertsSinceLastCompress = 0;

	private final double[] buffer = new double[compressInterval];
	private int bufferPos = 0;

	public CKMSQuantiles(Quantile... quantiles) {
		if (quantiles.length == 0) {
			throw new IllegalArgumentException("quantiles cannot be empty");
		}
		this.quantiles = quantiles;
	}

	/**
	 * Add an observed value
	 */
	public void insert(double value) {
		buffer[bufferPos++] = value;

		if (bufferPos == buffer.length) {
			flush();
		}

		if (++insertsSinceLastCompress == compressInterval) {
			compress();
			insertsSinceLastCompress = 0;
		}
	}

	private void flush() {
		Arrays.sort(buffer, 0, bufferPos);
		insertBatch(buffer, bufferPos);
		bufferPos = 0;
	}

	/**
	 * Inserts the elements from index 0 to index toIndex from the sortedBuffer.
	 */
	void insertBatch(double[] sortedBuffer, int toIndex) {
		if (toIndex == 0) {
			return;
		}
		ListIterator<Sample> iterator = samples.listIterator();
		int i = 0; // position in buffer
		int r = 0; // sum of g's left of the current sample
		while (iterator.hasNext() && i < toIndex) {
			Sample item = iterator.next();
			while (i < toIndex) {
				if (sortedBuffer[i] > item.value) {
					break;
				}
				insertBefore(iterator, sortedBuffer[i], r);
				r++; // new item with g=1 was inserted before, so increment r
				i++;
				n++;
			}
			r += item.g;
		}
		while (i < toIndex) {
			samples.add(new Sample(sortedBuffer[i], 0));
			i++;
			n++;
		}
	}

	private void insertBefore(ListIterator<Sample> iterator, double value,
			int r) {
		if (!iterator.hasPrevious()) {
			samples.addFirst(new Sample(value, 0));
		} else {
			iterator.previous();
			iterator.add(new Sample(value, f(r) - 1));
			iterator.next();
		}
	}

	/**
	 * Get the estimated value at the specified quantile.
	 */
	public double get(double q) {
		flush();

		if (samples.size() == 0) {
			return Double.NaN;
		}

		if (q == 0.0) {
			return samples.getFirst().value;
		}

		if (q == 1.0) {
			return samples.getLast().value;
		}

		int r = 0; // sum of g's left of the current sample
		int desiredRank = (int) Math.ceil(q * n);

		ListIterator<Sample> iterator = samples.listIterator();
		while (iterator.hasNext()) {
			Sample sample = iterator.next();
			if (r + sample.g + sample.delta > desiredRank + f(desiredRank) / 2) {
				iterator.previous(); // roll back the item.next() above
				if (iterator.hasPrevious()) {
					Sample result = iterator.previous();
					return result.value;
				} else {
					return sample.value;
				}
			}
			r += sample.g;
		}
		return samples.getLast().value;
	}

	/**
	 * Error function, as in definition 5 of the paper.
	 */
	int f(int r) {
		int minResult = Integer.MAX_VALUE;
		for (Quantile q : quantiles) {
			if (q.quantile == 0 || q.quantile == 1) {
				continue;
			}
			int result;
			// We had a numerical error here with the following example:
			// quantile = 0.95, epsilon = 0.01, (n-r) = 30.
			// The expected result of (2*0.01*30)/(1-0.95) is 12. The actual result is
			// 11.99999999999999.
			// To avoid running into these types of error we add 0.00000000001 before
			// rounding down.
			if (r >= q.quantile * n) {
				result = (int) (q.v * r + 0.00000000001);
			} else {
				result = (int) (q.u * (n - r) + 0.00000000001);
			}
			if (result < minResult) {
				minResult = result;
			}
		}
		return Math.max(minResult, 1);
	}

	/**
	 * Merge pairs of consecutive samples if this doesn't violate the error function.
	 */
	void compress() {
		if (samples.size() < 3) {
			return;
		}
		Iterator<Sample> descendingIterator = samples.descendingIterator();
		int r = n; // n is equal to the sum of the g's of all samples

		Sample right;
		Sample left = descendingIterator.next();
		r -= left.g;

		while (descendingIterator.hasNext()) {
			right = left;
			left = descendingIterator.next();
			r = r - left.g;
			if (left == samples.getFirst()) {
				// The min sample must never be merged.
				break;
			}
			if (left.g + right.g + right.delta < f(r)) {
				right.g += left.g;
				descendingIterator.remove();
				left = right;
			}
		}
	}

	static class Sample {

		/**
		 * Observed value.
		 */
		final double value;

		int g = 1;

		final int delta;

		Sample(double value, int delta) {
			this.value = value;
			this.delta = delta;
		}

		@Override
		public String toString() {
			return String.format("Sample{val=%.3f, g=%d, delta=%d}", value, g, delta);
		}
	}

	static class Quantile {

		final double quantile;

		final double epsilon;

		final double u;

		final double v;

		Quantile(double quantile, double epsilon) {
			if (quantile < 0.0 || quantile > 1.0)
				throw new IllegalArgumentException("Quantile must be between 0 and 1");
			if (epsilon < 0.0 || epsilon > 1.0)
				throw new IllegalArgumentException("Epsilon must be between 0 and 1");

			this.quantile = quantile;
			this.epsilon = epsilon;
			u = 2.0 * epsilon / (1.0 - quantile); // if quantile == 1 this will be
																						// Double.NaN
			v = 2.0 * epsilon / quantile; // if quantile == 0 this will be Double.NaN
		}

		@Override
		public String toString() {
			return String.format("Quantile{q=%.3f, epsilon=%.3f}", quantile, epsilon);
		}
	}
}
