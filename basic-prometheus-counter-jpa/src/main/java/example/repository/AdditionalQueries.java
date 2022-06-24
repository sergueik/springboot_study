package example.repository;

import java.util.List;

public interface AdditionalQueries {
	public List<Object[]> customFind(String query);
}
