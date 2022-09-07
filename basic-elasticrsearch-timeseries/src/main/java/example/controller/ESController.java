package example.controller;

import com.alibaba.fastjson.JSONObject;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;

import java.io.IOException;
import java.util.*;
import example.service.ESService;
import example.entity.DemoEntity;

@Controller
public class ESController {

	@Autowired
	private ESService esService;

	private static final String MYINDEX = "my_index_test";
	private Random generator = new Random(3);

	@ResponseBody
	@RequestMapping("/insert")
	public String insert() {
		DemoEntity entity = new DemoEntity();
		final String dc = "west";
		final String hostname = "hostname" + generator.nextInt(10);
		final String appId1 = "app" + generator.nextInt(10);
		final String appId2 = "app" + generator.nextInt(10);
		entity.setDc(dc);
		entity.setHostname(hostname);
		entity.setAppId(String.format("%s,%s", appId1, appId2));
		// entity.setAppId(UUID.randomUUID().toString());
		entity.setHostname(hostname);
		entity.setCpu((float) (0.1 * (float) (generator.nextInt(50))));
		entity.setCreateTime(new Date());
		try {
			return esService.save(entity, MYINDEX) ? "SUCCESS" : "FAIL";
		} catch (IOException e) {
			e.printStackTrace();
			return "FAIL";
		}
	}

	@ResponseBody
	@RequestMapping("/query")
	public Object query() {
		try {

			final String index = MYINDEX;
			final int page = 1;
			final int limit = 100;
			final String name = null;
			final String order = "desc";
			List<Map<String, Object>> list = esService.query(index, page, limit, name,
					order);
			return list;
		} catch (IOException e) {
			e.printStackTrace();
			return "FAIL";
		}
	}
}
