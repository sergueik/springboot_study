package example.service;

import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.annotation.Resource;
import org.springframework.stereotype.Service;

import example.dao.Dao;
import example.entity.Result;
import example.entity.Host;

@Service
public class ServiceImpl implements BaseService {

	@Resource(name = "JdbcDao")
	private Dao dao;
	private static final Logger logger = Logger
			.getLogger(ServiceImpl.class.getName());

	@Override
	public Result addHost(Host host) {
		Result result = new Result();
		try {
			int res = dao.addHost(host);
			result.setStatus(res);
		} catch (Exception e) {
			logger.log(Level.SEVERE, null, e);
		}
		return result;
	}

	@Override
	public Result findAllHost() {
		Result result = new Result();
		try {
			List<?> hosts = dao.findAllHost();
			result.setStatus(1);
			result.setData(hosts);
		} catch (Exception e) {
			logger.log(Level.SEVERE, null, e);
		}
		return result;
	}

	@Override
	public Result updateHost(Host hosts) {
		Result result = new Result();
		try {
			int res = dao.updateHost(hosts);
			result.setStatus(res);
		} catch (Exception e) {
			logger.log(Level.SEVERE, null, e);
		}
		return result;
	}

	@Override
	public Result delHostById(String id) {
		Result result = new Result();
		try {
			int res = dao.delHostById(Long.parseLong(id));
			result.setStatus(res);
		} catch (Exception e) {
			logger.log(Level.SEVERE, null, e);
		}
		return result;
	}

	@Override
	public Result findHostById(String id) {
		Result result = new Result();
		try {
			Host res = dao.findHostById(Long.parseLong(id));
			result.setData(res);
		} catch (Exception e) {
			logger.log(Level.SEVERE, null, e);
		}
		return result;
	}

	@Override
	public Result findHostByHostname(String hostname) {
		Result result = new Result();
		try {
			Host res = dao.findHostByHostname(hostname);
			result.setData(res);
		} catch (Exception e) {
			logger.log(Level.SEVERE, null, e);
		}
		return result;
	}
}
