package example.custom;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

public class CircularListTest {

	private static final int size = 10;
	private List<DataEntry> list;
	private static int cnt = 0;
	private static final long value = 0;
	private static final DataEntry entry = new DataEntry(value);

	@Before
	public void setUp() {
		list = Collections.synchronizedList(new CircularList(size));
	}

	// @Ignore
	@Test
	public void test1() {
		for (int i = 0; i != size; i++) {
			synchronized (list) {
				list.add(entry);
				cnt = list.size();
				System.err.println("Cnt is: " + i + "\t" + "List tail is: " + cnt + "\t"
						+ "size: " + size);
			}
		}
		assertThat(cnt, is(0));
	}

	@Test
	public void test2() {
		for (int i = 0; i != size; i++) {
			list.add(entry);
			cnt = list.size();
			System.err.println("Cnt is: " + i + "\t" + "List tail is: " + cnt + "\t"
					+ "size: " + size);

		}
		assertThat(cnt, is(0));
	}

	@Test
	public void test3() {
		int size2 = 5;
		for (int i = 0; i != size2; i++) {
			list.add(entry);
		}
		List<DataEntry> dataList = Arrays.asList((DataEntry[]) list.toArray());
		List<DataEntry> data2 = dataList.stream().filter(o -> o != null)
				.collect(Collectors.toList());
		assertThat(data2, notNullValue());
		assertThat(data2.size(), is(size2));
	}

	@Test
	public void test4() {
		int size2 = 15;
		for (int i = 0; i != size2; i++) {
			list.add(entry);
		}
		List<DataEntry> dataList = Arrays.asList((DataEntry[]) list.toArray());
		List<DataEntry> data2 = dataList.stream().filter(o -> o != null)
				.collect(Collectors.toList());
		assertThat(data2, notNullValue());
		assertThat(data2.size(), is(size));
	}
}
