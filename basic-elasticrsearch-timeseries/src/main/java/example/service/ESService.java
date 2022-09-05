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

	/**
	 * 保存消息
	 * @param entity 消息
	 * @throws IOException
	 */
	public boolean save(DemoEntity entity, String index) throws IOException {
		String newIndex = getIndexForSave(index);

		IndexRequest request = new IndexRequest(newIndex);
		Map<String, Object> map = JSONObject
				.parseObject(JSONObject.toJSONString(entity)).getInnerMap();
		request.source(map, XContentType.JSON);
		IndexResponse iResponse = client.index(request, RequestOptions.DEFAULT);
		return "created".equals(iResponse.getResult().getLowercase());
	}

	public List<Map<String, Object>> query(String index, int page, int limit,
			String name, String order) throws IOException {
		String newIndex = getIndexForSearch(index);
		SearchRequest searchRequest = new SearchRequest(newIndex);

		SearchSourceBuilder sourceBuilder = new SearchSourceBuilder();

		if (!StringUtils.isEmpty(name)) {
			sourceBuilder.query(
					QueryBuilders.matchQuery(name, "name").minimumShouldMatch("100%"));
		}
		sourceBuilder.timeout(new TimeValue(10, TimeUnit.SECONDS));
		sourceBuilder.from((page - 1) * limit);
		sourceBuilder.size(limit);

		// 排序
		switch (order) {
		case "desc":
			sourceBuilder
					.sort(new FieldSortBuilder("createTime").order(SortOrder.DESC));
			break;
		case "asc":
			sourceBuilder
					.sort(new FieldSortBuilder("createTime").order(SortOrder.ASC));
			break;
		}

		searchRequest.source(sourceBuilder);

		SearchResponse sResponse = client.search(searchRequest,
				RequestOptions.DEFAULT);
		List<Map<String, Object>> list = new ArrayList<>();
		Arrays.stream(sResponse.getHits().getHits()).forEach(i -> {
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
