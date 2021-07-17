package example;

import org.springframework.data.repository.PagingAndSortingRepository;

public interface ModelMongoRepository extends PagingAndSortingRepository<Model, String> {
}     