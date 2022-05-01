package example.service;

import example.entity.Result;
import example.entity.Host;

public interface BaseService {

	public Result addHost(Host host);
	public Result findAllHost();
	public Result updateHost(Host host);
	public Result delHostById(String id);
	public Result findHostById(String id);
	public Result findHostByHostname(String hostname);
}
