package example;

import example.models.KeyValResponse;
import example.models.MatrixResponse;
import example.models.PrometheusResponse;
import example.models.VectorResponse;
import retrofit2.Call;
import retrofit2.http.GET;
import retrofit2.http.Query;

public interface PrometheusRest {
//@formatter:off
  @GET("api/v1/query")
  Call<VectorResponse> query(
    @Query("query") String query,
    @Query("time") String time,
    @Query("timeout") String timeout
  );

  @GET("api/v1/query_range")
  Call<MatrixResponse> queryRange(
    @Query("query") String query,
    @Query("start") String start,
    @Query("end") String end,
    @Query("step") String step,
    @Query("timeout") String timeout
  );

  @GET("api/v1/series")
  Call<KeyValResponse> findSeries(
    @Query("match[]") String match,
    @Query("start") String start,
    @Query("end") String end
  );
//@formatter:on  
}
