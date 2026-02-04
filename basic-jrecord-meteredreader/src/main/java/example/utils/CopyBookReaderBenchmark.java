package example.utils;

import org.openjdk.jmh.annotations.*;
import java.util.concurrent.TimeUnit;



@Warmup(iterations = 1, time = 200, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 2, time = 300, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
@BenchmarkMode(Mode.Throughput)
// strongest setting:
// @BenchmarkMode(Mode.SingleShotTime)
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
		new Reader(copybookFile, inputFile, page, maxRows).parseRecords();
	}
}
