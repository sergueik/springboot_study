package example.service;

import com.alibaba.fastjson.JSONObject;
import org.elasticsearch.action.admin.indices.alias.Alias;
import org.elasticsearch.action.index.IndexRequest;
import org.elasticsearch.action.index.IndexResponse;
import org.elasticsearch.action.search.SearchRequest;
import org.elasticsearch.action.search.SearchResponse;
import org.elasticsearch.client.ElasticsearchClient;
import org.elasticsearch.client.RequestOptions;
import org.elasticsearch.client.RestHighLevelClient;
import org.elasticsearch.client.indices.CreateIndexRequest;
import org.elasticsearch.client.indices.CreateIndexResponse;
import org.elasticsearch.client.indices.GetIndexRequest;
import org.elasticsearch.common.settings.Settings;
import org.elasticsearch.common.unit.TimeValue;
import org.elasticsearch.common.xcontent.XContentType;
import org.elasticsearch.index.query.QueryBuilders;
import org.elasticsearch.search.builder.SearchSourceBuilder;
import org.elasticsearch.search.sort.FieldSortBuilder;
import org.elasticsearch.search.sort.SortOrder;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.elasticsearch.core.ElasticsearchRestTemplate;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;

import java.io.IOException;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import example.entity.DemoEntity;

@Service
public class ESService {

	@Autowired
	private RestHighLevelClient client;

	public boolean save(DemoEntity entity, String index) throws IOException {
		String newIndex = getIndexForSave(index);

		IndexRequest indexRequest = new IndexRequest(newIndex);
		Map<String, Object> map = JSONObject
				.parseObject(JSONObject.toJSONString(entity)).getInnerMap();
		indexRequest.source(map, XContentType.JSON);
		IndexResponse indexResponse = client.index(indexRequest,
				RequestOptions.DEFAULT);
		return "created".equals(indexResponse.getResult().getLowercase());
	}

	public List<Map<String, Object>> query(String index, int page, int limit,
			String name, String order) throws IOException {
		String newIndex = getIndexForSearch(index);
		SearchRequest searchRequest = new SearchRequest(newIndex);

		SearchSourceBuilder searchSourceBuilder = new SearchSourceBuilder();

		if (!StringUtils.isEmpty(name)) {
			searchSourceBuilder.query(
					QueryBuilders.matchQuery(name, "name").minimumShouldMatch("100%"));
		}
		searchSourceBuilder.timeout(new TimeValue(10, TimeUnit.SECONDS));
		searchSourceBuilder.from((page - 1) * limit);
		searchSourceBuilder.size(limit);

		switch (order) {
		case "desc":
			searchSourceBuilder
					.sort(new FieldSortBuilder("createTime").order(SortOrder.DESC));
			break;
		case "asc":
			searchSourceBuilder
					.sort(new FieldSortBuilder("createTime").order(SortOrder.ASC));
			break;
		}

		searchRequest.source(searchSourceBuilder);

		SearchResponse searchResponse = client.search(searchRequest,
				RequestOptions.DEFAULT);
		List<Map<String, Object>> list = new ArrayList<>();
		// https://www.javadoc.io/doc/org.elasticsearch/elasticsearch/7.6.2/org/elasticsearch/action/search/SearchResponse.html#getHits()
		// https://www.javadoc.io/doc/org.elasticsearch/elasticsearch/7.6.2/org/elasticsearch/search/SearchHits.html
		Arrays.stream(
				searchResponse.getHits() /* SearchHits */.getHits() /* SearchHit[] */)
				.forEach(i -> {
					Map<String, Object> map = i.getSourceAsMap();
					list.add(map);
				});
		return list;
	}

	private String getIndexForSave(String index) {
		LocalDate now = LocalDate.now();
		return index + "_" + now.getYear() + "-" + now.getMonthValue();
	}

	private String getIndexForSearch(String index) {
		return String.format("%s_alias", index);
	}
}
