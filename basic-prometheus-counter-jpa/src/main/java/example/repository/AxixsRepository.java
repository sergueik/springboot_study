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

	// NOTE: strongly typed - watch the columns to match the constructor arguments
	// NOTE: rows from left join with with null values
	// will be silently removed from the strongly typed result (?)
	// or rather throw
	// org.springframework.core.convert.ConverterNotFoundException:
	// org.springframework.core.convert.ConverterNotFoundException:
	// No converter found capable of converting from type
	// [org.springframework.data.jpa.repository.query.AbstractJpaQuery$TupleConverter$TupleBackedMap]
	// to type
	// [example.projection.ServerInstanceApplication]
	// and test expectations on row counts will not be satisfied
// 	@Query(value = "select sname as hostname,aname as application, iname as env from axixs x join server s on x.sid = s.sid left join application a on x.aid = a.aid join instance i on x.iid = i.iid", nativeQuery = true)
	@Query(value = "select new example.projection.ServerInstanceApplication(sname,aname,iname) from axixs x join server s on x.sid = s.sid join application a on x.aid = a.aid join instance i on x.iid = i.iid", nativeQuery = true)
	public List<ServerInstanceApplication> findAllServerInstanceApplicationsNative();

	// NOTE: losely typed
	@Query("select s.serverName as hostname, 'dummy' as dc, a.applicationName as application, i.instanceName as env from Axixs x join Server s on x.serverId = s.serverId left join Application a on x.applicationId = a.applicationId join Instance i on x.instanceId = i.instanceId")
	public List<Object[]> findAllData();

	@Query(value = "select sname as hostname,'dummy' as dc,aname as application, iname as env from axixs x join server s on x.sid = s.sid left join application a on x.aid = a.aid join instance i on x.iid = i.iid", nativeQuery = true)
	public List<Object[]> findAllDataNative();
}
