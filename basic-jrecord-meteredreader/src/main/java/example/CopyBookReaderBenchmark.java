package example;

import org.openjdk.jmh.annotations.*;
import java.util.concurrent.TimeUnit;

@BenchmarkMode(Mode.Throughput)
@State(Scope.Benchmark)
@OutputTimeUnit(TimeUnit.SECONDS)
public class CopyBookReaderBenchmark {

	@Param({ "example.cbl" })
	public String copybookFile;

	@Param({ "example.bin" })
	public String inputFile;

	@Param({ "1000" })
	public long maxRows;

	@Param({ "cp037" })
	public String page;

	@Benchmark
	public void parseRecordsBencmark() throws Exception {
		Reader.parseRecords(copybookFile, inputFile, maxRows, page);
	}
}
