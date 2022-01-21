package example;

// import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.mongodb.repository.MongoRepository;
import example.Model;

// public interface ModelMongoRepository extends PagingAndSortingRepository<Model, String> {
// NOTE: Model has @Id long
public interface ModelMongoRepository extends MongoRepository<Model, Long> {

}
