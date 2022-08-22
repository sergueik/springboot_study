package example.repository;

import java.util.List;

public interface AdditionalQueries {
	public List<Object[]> customFind(String query);
	public List<Object[]> customFind(String query, long id);
}
