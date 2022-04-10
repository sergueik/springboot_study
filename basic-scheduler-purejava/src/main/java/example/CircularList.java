package example;

import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;

// origin: http://www.java2s.com/Tutorial/Java/0140__Collections/CircularBuffer.htm
public class CircularList implements List<DataEntry> {

	private DataEntry data[];
	private int head;
	private int tail;

	public CircularList(int size) {
		data = new DataEntry[size];
		head = 0;
		tail = 0;
	}

	private boolean isInfinite = true;

	public boolean store(DataEntry value) {
		if (isInfinite || !bufferFull()) {
			data[tail++] = value;
			if (tail == data.length) {
				tail = 0;
			}
			return true;
		} else {
			return false;
		}
	}

	public DataEntry read() {
		if (head != tail) {
			DataEntry value = data[head++];
			if (head == data.length) {
				head = 0;
			}
			return value;
		} else {
			return null;
		}
	}

	private boolean bufferFull() {
		if (tail + 1 == head) {
			return true;
		}
		if (tail == (data.length - 1) && head == 0) {
			return true;
		}
		return false;
	}

	@Override
	public boolean add(DataEntry value) {
		return store(value);
	}

	@Override
	public void add(int index, DataEntry value) {
		// TODO: implement
		return;
	}

	@Override
	public boolean addAll(Collection<? extends DataEntry> values) {
		boolean status = false;
		for (DataEntry value : values) {
			status |= this.add(value);
		}
		return status;
	}

	@Override
	public boolean addAll(int index, Collection<? extends DataEntry> values) {
		// TODO: implement
		return false;
	}

	@Override
	public void clear() {
		head = 0;
		tail = 0;
	}

	@Override
	public boolean contains(Object o) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public boolean containsAll(Collection<?> c) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public DataEntry get(int index) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public int indexOf(Object o) {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public boolean isEmpty() {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public Iterator<DataEntry> iterator() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public int lastIndexOf(Object o) {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public ListIterator<DataEntry> listIterator() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public ListIterator<DataEntry> listIterator(int index) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public boolean remove(Object o) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public DataEntry remove(int index) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public boolean removeAll(Collection<?> c) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public boolean retainAll(Collection<?> c) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public DataEntry set(int index, DataEntry element) {
		data[index % data.length] = element;
		return null;
	}

	@Override
	public int size() {
		// not really the"size", used to make synchronizedList happily call
		return tail;
	}

	@Override
	public List<DataEntry> subList(int fromIndex, int toIndex) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Object[] toArray() {
		// TODO Auto-generated method stub
		return data;
	}

	// NOTE: generic argument
	@Override
	public <DataEntry> DataEntry[] toArray(DataEntry[] a) {
		return null;
	}

}
