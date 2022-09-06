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

	@ResponseBody
	@RequestMapping("/insert")
	public String insert() {
		DemoEntity entity = new DemoEntity();
		entity.setId(UUID.randomUUID().toString());
		entity.setName("Obj-" + new Random(10).nextInt(20));
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
			List<Map<String, Object>> list = esService.query(MYINDEX, 1, 3, null,
					"desc");
			return list;
		} catch (IOException e) {
			e.printStackTrace();
			return "FAIL";
		}
	}
}
