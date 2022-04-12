package example.custom;

// java lacks plain struct
// prior to Java 14 there were no Records
// https://stackoverflow.com/questions/5168144/does-java-support-structs
public class CounterData {
	public int cnt;
	public long firstValue;
	public String counterName;
	public long secondValue;
	public int multiCount;
}
