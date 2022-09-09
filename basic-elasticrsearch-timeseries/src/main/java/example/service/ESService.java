package example.service;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonPrimitive;

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
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.text.SimpleDateFormat;
import java.time.Instant;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import example.HostData;
import example.entity.DemoEntity;
import example.utils.DemoEntitySerializer;
import example.utils.Utils;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@Service
public class ESService {

	private static Utils utils = Utils.getInstance();
	private final boolean debug = true;
	private static final String filemask = "data.txt.*$";
	private static String[] metricNames = { "cpu" };
	private static Map<String, String> data = new HashMap<>();
	private static Map<String, String> extractedMetricNames = new HashMap<>();
	private static Map<String, String> metricExtractors = new HashMap<>();
	private final static boolean directConvertrsion = false;
	private static final SimpleDateFormat simpleDateFormat = new SimpleDateFormat(
			"yyyy-MM-dd HH:mm:ss");
	private static String hostname = "host01";
	private static String osName = utils.getOSName();
	private static HostData hostData = null;
	private static final String MYINDEX = "my_index_test";

	private static final Logger logger = LogManager.getLogger(ESService.class);

	private static Gson gson = directConvertrsion ? new Gson()
			: new GsonBuilder()
					.registerTypeAdapter(DemoEntity.class, new DemoEntitySerializer())
					.create();

	@Autowired
	private RestHighLevelClient client;

	public boolean save(DemoEntity entity, String index) throws IOException {
		String newIndex = getIndexForSave(index);

		IndexRequest indexRequest = new IndexRequest(newIndex);
		String entityStr = gson.toJson(entity);
		if (debug)
			logger.info("Serialized entity: " + entityStr);
		@SuppressWarnings("unchecked")
		Map<String, Object> entityMap = new Gson().fromJson(entityStr, Map.class);
		indexRequest.source(entityMap, XContentType.JSON);
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

	public List<Map<String, String>> listFilesDsNames(String path,
			List<String> collectFolders, List<String> rejectFolders)
			throws IOException {

		final List<Path> result = new ArrayList<>();
		Path basePath = Paths.get(path);
		final String basePathUri = new URL(
				getDataFileUri(basePath.toAbsolutePath().toString())).getFile() + "/";
		System.err.println("Scanning path: " + basePathUri);
		List<Path> folders = new ArrayList<>();
		// Probably quite sub-optimal
		try (Stream<Path> walk = Files.walk(basePath)) {
			// collect folders
			folders = walk.filter(Files::isDirectory).filter(o -> {
				String key = o.getFileName().toString();
				System.err.println("inspect: " + key);
				boolean status = true;
				// NOTE: exact match required
				if ((collectFolders.size() > 0 && !collectFolders.contains(key))
						|| rejectFolders.size() > 0 && rejectFolders.contains(key)) {
					status = false;
				}
				System.err.println("status: " + status);
				return status;
			}).collect(Collectors.toList());
		}
		// collect files in folders
		for (Path folder : folders) {
			Stream<Path> walk = Files.walk(folder);
			walk.filter(Files::isRegularFile)
					.filter(o -> o.getFileName().toString().matches(filemask))
					.forEach(o -> {
						if (debug)
							logger.info("found file: " + o.getFileName().toString());
						result.add(o);
					});
		}
		String newIndex = getIndexForSave(MYINDEX);
		IndexRequest indexRequest = new IndexRequest(newIndex);
		// NOTE: Local variable indexResponse defined in an enclosing scope must be
		// final or effectively final
		// IndexResponse indexResponse;
		List<Map<String, String>> results = readFiles(result);

		results.stream().forEach(o -> {
			// NOTE: Adding dummy "hostname" and "appid"
			o.put("hostame", hostname);
			o.put("appid", "app0");
			// NOTE: rename column and enforcing "yyyy-MM-dd HH:mm:ss"
			// see also:
			// https://stackoverflow.com/questions/535004/unix-epoch-time-to-java-date-object
			// "@1662623820" appears to be a seconds-based epoch value therefore
			// multiply the long from parseLong() with 1000
			// to convert to milliseconds, that Date constructor expects:
			// logger.info("converting " + o.get("timestamp"));
			Date createTime = new Date(Long.parseLong(o.get("timestamp")) * 1000);
			o.remove("timestamp");
			if (createTime != null) {
				o.put("createTime", simpleDateFormat.format(createTime));
			}
			if (debug)
				logger.info("insert: " + " hostname: " + o.get("hostname") + " @"
						+ o.get("createTime"));
			indexRequest.source(o, XContentType.JSON);
			try {
				IndexResponse indexResponse = client.index(indexRequest,
						RequestOptions.DEFAULT);
				if (debug)
					logger.info("response: " + indexResponse.getResult().getLowercase());
			} catch (Exception e) {
				// TODO: process
				e.printStackTrace();
			}
		});
		return results;
	}

	private static String getDataFileUri(String dataFilePath) {
		return osName.equals("windows")
				? "file:///" + dataFilePath.replaceAll("\\\\", "/")
				: "file://" + dataFilePath;
	}

	private List<Map<String, String>> readFiles(List<Path> result) {
		List<Map<String, String>> results = new ArrayList<>();
		System.err.println(String.format("Ingesting %d files: ", result.size()));
		result.stream().forEach(o -> {
			hostData = new HostData(hostname,
					o.getParent().toAbsolutePath().toString(),
					o.getFileName().toString());
			// sync debug settings
			hostData.setDebug(debug);
			// NOTE: metricNames are used in SQL insert when processing metricsData
			hostData.setMetrics(Arrays.asList(metricNames));
			if (debug)
				System.err.println("about to add data: " + Arrays.asList(metricNames));
			hostData.setExtractedMetricNames(extractedMetricNames);
			hostData.setMetricExtractors(metricExtractors);
			hostData.readData();
			long timestamp = hostData.getTimestamp();
			if (timestamp == 0)
				timestamp = Instant.now().toEpochMilli();
			if (debug)
				System.err.println("adding timestamp: " + timestamp);
			data = hostData.getData();
			if (data != null && !data.isEmpty()) {
				if (debug)
					System.err.println("added data: " + data.keySet());
				data.put("timestamp", Long.toString(timestamp, 10));
				data.put("hostname", hostname);
				results.add(data);
			} else {
				if (debug)
					System.err.println("data is empty: " + o.getFileName().toString());
			}
		});
		return results;
	}

}
