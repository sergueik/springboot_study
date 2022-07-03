package example.exemplars;

public interface HistogramExemplarSampler {
	Exemplar sample(double value, double bucketFrom, double bucketTo,
			Exemplar previous);
}
