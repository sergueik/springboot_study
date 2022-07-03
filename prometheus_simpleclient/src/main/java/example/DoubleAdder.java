/*
 * Written by Doug Lea with assistance from members of JCP JSR-166
 * Expert Group and released to the public domain, as explained at
 * http://creativecommons.org/publicdomain/zero/1.0/
 *
 * Source: http://gee.cs.oswego.edu/cgi-bin/viewcvs.cgi/jsr166/src/jsr166e/DoubleAdder.java?revision=1.12
 */

package example;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;

public class DoubleAdder extends Striped64 implements Serializable {
	private static final long serialVersionUID = 7249069246863182397L;

	final long fn(long v, long x) {
		return Double.doubleToRawLongBits(
				Double.longBitsToDouble(v) + Double.longBitsToDouble(x));
	}

	public DoubleAdder() {
	}

	public void add(double x) {
		Cell[] as;
		long b, v;
		int[] hc;
		Cell a;
		int n;
		if ((as = cells) != null || !casBase(b = base,
				Double.doubleToRawLongBits(Double.longBitsToDouble(b) + x))) {
			boolean uncontended = true;
			if ((hc = threadHashCode.get()) == null || as == null
					|| (n = as.length) < 1 || (a = as[(n - 1) & hc[0]]) == null
					|| !(uncontended = a.cas(v = a.value,
							Double.doubleToRawLongBits(Double.longBitsToDouble(v) + x))))
				retryUpdate(Double.doubleToRawLongBits(x), hc, uncontended);
		}
	}

	public double sum() {
		// On concurrent `sum` and `set`, it is acceptable to `get` an outdated
		// `value`.
		// On concurrent `sum` and `add`, it is acceptable to `get` an outdated
		// `value`.
		// On concurrent `sum` and `set` and `add`, it is possible to `get` an
		// outdated `value`.

		// Correctness is guaranteed by `volatile` memory access ordering and
		// visibility semantics.
		// Program order:
		// - writes in `set` - `busy` (CAS), `cells` (Wc), `base` (Wb), `busy`
		// - reads in `sum` - `cells` (Rc), `base` (Rb), `busy`, `cells` (Cc),
		// `base` (Cb)
		// Note that:
		// - `busy` is written after `cells` and `base`
		// - `busy` is read after `cells` and `base`, then `cells` and `base` is
		// re-read after `busy`
		// In other words:
		// - if we see the write to `busy`, then we must see the write to `cells`
		// and `busy` on re-read
		// - if we don't see the write to `busy`, then we must retry as we have no
		// guarantees
		// Execution order (in the former case):
		// - serial
		// - old result - Rc, Rb, Cc, Cb, Wc, Wb
		// - new result - Wc, Wb, Rc, Rb, Cc, Cb
		// - concurrent
		// - old result - Rc, Wc, Rb, Wb, Cc, Cb - retry (superfluous)
		// - new result - Wc, Rc, Wb, Rb, Cc, Cb
		// - invalid result - Rc, Wc, Wb, Rb, Cc, Cb - retry
		// - invalid result - Wc, Rc, Rb, Wb, Cc, Cb - retry
		Cell[] as = cells;
		long b = base;
		while (as != null && !(busy == 0 && cells == as && base == b)) {
			// busy waiting, retry loop
			Thread.yield();
			as = cells;
			b = base;
		}

		double sum = Double.longBitsToDouble(b);
		if (as != null) {
			int n = as.length;
			for (int i = 0; i < n; ++i) {
				Cell a = as[i];
				if (a != null)
					sum += Double.longBitsToDouble(a.value);
			}
		}
		return sum;
	}

	public void reset() {
		internalReset(0L);
	}

	public void set(double x) {
		// On concurrent `set` and `set`, it should be acceptable to lose one `set`
		// measurement.
		// On concurrent `set` and `add`, it should be acceptable to lose the `add`
		// measurement.

		// Correctness is ensured by different techniques:
		// - `set` waits on contention (blocking)
		// - `add` avoids contention (non-blocking)
		// - `sum` retries on conflicts (non-blocking)
		// Performance characteristics by use cases:
		// - only `set` - `cells` is always `null` - no allocations
		// - only `add` - `cells` allocated on contention
		// - mixed `set` and `add` - `cells` allocated on contention, `cells`
		// deallocated on `set`
		for (;;) {
			Cell[] as;
			if ((as = cells) != null) { // have cells
				if (busy == 0 && casBusy()) {
					try {
						if (cells == as) { // recheck under lock
							// update cells and base (not atomic)
							cells = null;
							base = Double.doubleToLongBits(x);
							break;
						}
					} finally {
						busy = 0;
					}
				}
			} else { // no cells
				// update base (atomic)
				base = Double.doubleToLongBits(x);
				break;
			}
		}
	}

	public double sumThenReset() {
		Cell[] as = cells;
		double sum = Double.longBitsToDouble(base);
		base = 0L;
		if (as != null) {
			int n = as.length;
			for (int i = 0; i < n; ++i) {
				Cell a = as[i];
				if (a != null) {
					long v = a.value;
					a.value = 0L;
					sum += Double.longBitsToDouble(v);
				}
			}
		}
		return sum;
	}

	public String toString() {
		return Double.toString(sum());
	}

	public double doubleValue() {
		return sum();
	}

	public long longValue() {
		return (long) sum();
	}

	public int intValue() {
		return (int) sum();
	}

	public float floatValue() {
		return (float) sum();
	}

	private void writeObject(ObjectOutputStream s) throws IOException {
		s.defaultWriteObject();
		s.writeDouble(sum());
	}

	private void readObject(ObjectInputStream s)
			throws IOException, ClassNotFoundException {
		s.defaultReadObject();
		busy = 0;
		cells = null;
		base = Double.doubleToRawLongBits(s.readDouble());
	}

}
