package example.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import example.model.Axixs;
import example.projection.ServerInstanceApplication;

@Repository
public interface AxixsRepository extends JpaRepository<Axixs, Integer> {

	// NOTE: strongly typed
	@Query("select new example.projection.ServerInstanceApplication(s.serverName, a.applicationName,i.instanceName)"
			+ " from Axixs x join Server s on x.serverId = s.serverId join Application a on x.applicationId = a.applicationId join Instance i on x.instanceId = i.instanceId")
	public List<ServerInstanceApplication> findAllServerInstanceApplications();

	// NOTE: losely typed
	@Query("select s.serverName as hostname, 'dummy' as dc, a.applicationName as application, i.instanceName as env from Axixs x join Server s on x.serverId = s.serverId left join Application a on x.applicationId = a.applicationId join Instance i on x.instanceId = i.instanceId")
	public List<Object[]> findAllData();

}
