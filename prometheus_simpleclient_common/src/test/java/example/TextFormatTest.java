package example;

import static org.junit.Assert.assertEquals;

import java.io.IOException;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

import example.TextFormat;
import io.prometheus.client.Collector;
import io.prometheus.client.CollectorRegistry;
import io.prometheus.client.Counter;
import io.prometheus.client.Gauge;
import io.prometheus.client.Info;
import io.prometheus.client.Summary;

public class TextFormatTest {
	CollectorRegistry registry;
	StringWriter writer;

	@Before
	public void setUp() {
		registry = new CollectorRegistry();
		writer = new StringWriter();
	}

	@Test
	public void testGaugeOutput() throws IOException {
		Gauge noLabels = Gauge.build().name("nolabels").help("help")
				.register(registry);
		noLabels.inc();
		TextFormat.write004(writer, registry.metricFamilySamples());
		assertEquals(
				"# HELP nolabels help\n" + "# TYPE nolabels gauge\n" + "nolabels 1.0\n",
				writer.toString());
	}

	@Test
	public void testValueInfinity() throws IOException {
		Gauge noLabels = Gauge.build().name("nolabels").help("help")
				.register(registry);
		noLabels.set(Double.POSITIVE_INFINITY);
		TextFormat.write004(writer, registry.metricFamilySamples());
		assertEquals("# HELP nolabels help\n" + "# TYPE nolabels gauge\n"
				+ "nolabels +Inf\n", writer.toString());
	}

	@Test
	public void testCounterOutput() throws IOException {
		Counter noLabels = Counter.build().name("nolabels").help("help")
				.register(registry);
		noLabels.inc();
		TextFormat.write004(writer, registry.metricFamilySamples());
		assertEquals(
				"# HELP nolabels_total help\n" + "# TYPE nolabels_total counter\n"
						+ "nolabels_total 1.0\n" + "# HELP nolabels_created help\n"
						+ "# TYPE nolabels_created gauge\n" + "nolabels_created 1234.0\n",
				writer.toString().replaceAll("_created [0-9E.]+", "_created 1234.0"));
	}

	@Test
	public void testCounterWithTotalOutput() throws IOException {
		Counter noLabels = Counter.build().name("nolabels_total").help("help")
				.register(registry);
		noLabels.inc();
		TextFormat.write004(writer, registry.metricFamilySamples());
		assertEquals(
				"# HELP nolabels_total help\n" + "# TYPE nolabels_total counter\n"
						+ "nolabels_total 1.0\n" + "# HELP nolabels_created help\n"
						+ "# TYPE nolabels_created gauge\n" + "nolabels_created 1234.0\n",
				writer.toString().replaceAll("_created [0-9E.]+", "_created 1234.0"));
	}

	@Test
	public void testInfoOutput() throws IOException {
		Info noLabels = Info.build().name("nolabels").help("help")
				.register(registry);
		noLabels.info("foo", "bar");
		TextFormat.write004(writer, registry.metricFamilySamples());
		assertEquals("# HELP nolabels_info help\n" + "# TYPE nolabels_info gauge\n"
				+ "nolabels_info{foo=\"bar\",} 1.0\n", writer.toString());
	}

	@Test
	public void testCounterSamplesMissingTotal() throws IOException {

		class CustomCollector extends Collector {
			public List<MetricFamilySamples> collect() {
				List<MetricFamilySamples> mfs = new ArrayList<MetricFamilySamples>();
				ArrayList<String> labelNames = new ArrayList<String>();
				ArrayList<String> labelValues = new ArrayList<String>();
				ArrayList<MetricFamilySamples.Sample> samples = new ArrayList<Collector.MetricFamilySamples.Sample>();
				MetricFamilySamples.Sample sample = new MetricFamilySamples.Sample(
						"nolabels", labelNames, labelValues, 1.0);
				samples.add(sample);
				mfs.add(new MetricFamilySamples("nolabels", Collector.Type.COUNTER,
						"help", samples));
				return mfs;
			}
		}

		new CustomCollector().register(registry);
		TextFormat.write004(writer, registry.metricFamilySamples());
		assertEquals("# HELP nolabels_total help\n"
				+ "# TYPE nolabels_total counter\n" + "nolabels_total 1.0\n",
				writer.toString());
	}

	@Test
	public void testMetricOutputWithTimestamp() throws IOException {

		class CustomCollector extends Collector {
			public List<MetricFamilySamples> collect() {
				List<MetricFamilySamples> mfs = new ArrayList<MetricFamilySamples>();
				ArrayList<String> labelNames = new ArrayList<String>();
				ArrayList<String> labelValues = new ArrayList<String>();
				ArrayList<MetricFamilySamples.Sample> samples = new ArrayList<Collector.MetricFamilySamples.Sample>();
				MetricFamilySamples.Sample sample = new MetricFamilySamples.Sample(
						"nolabels", labelNames, labelValues, 1.0, 1518123456L);
				samples.add(sample);
				mfs.add(new MetricFamilySamples("nolabels", Collector.Type.UNKNOWN,
						"help", samples));
				return mfs;
			}
		}

		new CustomCollector().register(registry);
		TextFormat.write004(writer, registry.metricFamilySamples());
		assertEquals("# HELP nolabels help\n" + "# TYPE nolabels untyped\n"
				+ "nolabels 1.0 1518123456\n", writer.toString());
	}

	@Test
	public void testSummaryOutput() throws IOException {
		Summary noLabels = Summary.build().name("nolabels").help("help")
				.register(registry);
		noLabels.observe(2);
		TextFormat.write004(writer, registry.metricFamilySamples());
		assertEquals(
				"# HELP nolabels help\n" + "# TYPE nolabels summary\n"
						+ "nolabels_count 1.0\n" + "nolabels_sum 2.0\n"
						+ "# HELP nolabels_created help\n"
						+ "# TYPE nolabels_created gauge\n" + "nolabels_created 1234.0\n",
				writer.toString().replaceAll("_created [0-9E.]+", "_created 1234.0"));
	}

	@Test
	public void testSummaryOutputWithQuantiles() throws IOException {
		Summary labelsAndQuantiles = Summary.build().quantile(0.5, 0.05)
				.quantile(0.9, 0.01).quantile(0.99, 0.001).labelNames("l")
				.name("labelsAndQuantiles").help("help").register(registry);
		labelsAndQuantiles.labels("a").observe(2);
		writer = new StringWriter();
		TextFormat.write004(writer, registry.metricFamilySamples());
		assertEquals(
				"# HELP labelsAndQuantiles help\n"
						+ "# TYPE labelsAndQuantiles summary\n"
						+ "labelsAndQuantiles{l=\"a\",quantile=\"0.5\",} 2.0\n"
						+ "labelsAndQuantiles{l=\"a\",quantile=\"0.9\",} 2.0\n"
						+ "labelsAndQuantiles{l=\"a\",quantile=\"0.99\",} 2.0\n"
						+ "labelsAndQuantiles_count{l=\"a\",} 1.0\n"
						+ "labelsAndQuantiles_sum{l=\"a\",} 2.0\n"
						+ "# HELP labelsAndQuantiles_created help\n"
						+ "# TYPE labelsAndQuantiles_created gauge\n"
						+ "labelsAndQuantiles_created{l=\"a\",} 1234.0\n",
				writer.toString().replaceAll("(_created\\{.*\\}) [0-9E.]+",
						"$1 1234.0"));
	}

	@Test
	public void testGaugeHistogramOutput() throws IOException {
		class CustomCollector extends Collector {
			public List<MetricFamilySamples> collect() {
				List<MetricFamilySamples> mfs = new ArrayList<MetricFamilySamples>();
				ArrayList<String> labelNames = new ArrayList<String>();
				ArrayList<String> labelValues = new ArrayList<String>();
				ArrayList<MetricFamilySamples.Sample> samples = new ArrayList<Collector.MetricFamilySamples.Sample>();
				samples.add(new MetricFamilySamples.Sample("nolabels_bucket",
						Arrays.asList("le"), Arrays.asList("+Inf"), 2.0));
				samples.add(new MetricFamilySamples.Sample("nolabels_gcount",
						labelNames, labelValues, 2.0));
				samples.add(new MetricFamilySamples.Sample("nolabels_gsum", labelNames,
						labelValues, 7.0));
				samples.add(new MetricFamilySamples.Sample("nolabels_created",
						labelNames, labelValues, 1234.0));
				mfs.add(new MetricFamilySamples("nolabels",
						Collector.Type.GAUGE_HISTOGRAM, "help", samples));
				return mfs;
			}
		}
		new CustomCollector().register(registry);
		writer = new StringWriter();
		TextFormat.write004(writer, registry.metricFamilySamples());
		assertEquals(
				"# HELP nolabels help\n" + "# TYPE nolabels histogram\n"
						+ "nolabels_bucket{le=\"+Inf\",} 2.0\n"
						+ "# HELP nolabels_created help\n"
						+ "# TYPE nolabels_created gauge\n" + "nolabels_created 1234.0\n"
						+ "# HELP nolabels_gcount help\n" + "# TYPE nolabels_gcount gauge\n"
						+ "nolabels_gcount 2.0\n" + "# HELP nolabels_gsum help\n"
						+ "# TYPE nolabels_gsum gauge\n" + "nolabels_gsum 7.0\n",
				writer.toString());
	}

	@Test
	public void testLabelsOutput() throws IOException {
		Gauge labels = Gauge.build().name("labels").help("help").labelNames("l")
				.register(registry);
		labels.labels("a").inc();
		TextFormat.write004(writer, registry.metricFamilySamples());
		assertEquals("# HELP labels help\n" + "# TYPE labels gauge\n"
				+ "labels{l=\"a\",} 1.0\n", writer.toString());
	}

	@Test
	public void testLabelValuesEscaped() throws IOException {
		Gauge labels = Gauge.build().name("labels").help("help").labelNames("l")
				.register(registry);
		labels.labels("ąćčęntěd a\nb\\c\"d").inc();
		TextFormat.write004(writer, registry.metricFamilySamples());
		assertEquals(
				"# HELP labels help\n" + "# TYPE labels gauge\n"
						+ "labels{l=\"ąćčęntěd a\\nb\\\\c\\\"d\",} 1.0\n",
				writer.toString());
	}

	@Test
	public void testHelpEscaped() throws IOException {
		Gauge noLabels = Gauge.build().name("nolabels").help("ąćčęntěd h\"e\\l\np")
				.register(registry);
		noLabels.inc();
		TextFormat.write004(writer, registry.metricFamilySamples());
		assertEquals("# HELP nolabels ąćčęntěd h\"e\\\\l\\np\n"
				+ "# TYPE nolabels gauge\n" + "nolabels 1.0\n", writer.toString());
	}

	@Test
	public void testChooseContentType() throws IOException {
		assertEquals(TextFormat.CONTENT_TYPE_004,
				TextFormat.chooseContentType(null));
		assertEquals(TextFormat.CONTENT_TYPE_004, TextFormat.chooseContentType(""));
		assertEquals(TextFormat.CONTENT_TYPE_004,
				TextFormat.chooseContentType("text/plain;version=0.0.4"));
		assertEquals(TextFormat.CONTENT_TYPE_004,
				TextFormat.chooseContentType("foo"));
		assertEquals(TextFormat.CONTENT_TYPE_OPENMETRICS_100,
				TextFormat.chooseContentType(
						"application/openmetrics-text; version=0.0.1,text/plain;version=0.0.4;q=0.5,*/*;q=0.1"));
		assertEquals(TextFormat.CONTENT_TYPE_OPENMETRICS_100, TextFormat
				.chooseContentType("application/openmetrics-text; version=1.0.0"));
	}
}
