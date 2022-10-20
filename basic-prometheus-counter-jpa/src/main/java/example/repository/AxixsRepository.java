package example.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import example.model.Axixs;
import example.model.Server;
import example.projection.ServerInstanceApplication;

@Repository
public interface AxixsRepository
		extends JpaRepository<Axixs, Integer>, AdditionalQueries {

	// NOTE: strongly typed
	@Query("select new example.projection.ServerInstanceApplication(s.serverName, a.applicationName,i.instanceName)"
			+ " from Axixs x join Server s on x.serverId = s.serverId join Application a on x.applicationId = a.applicationId join Instance i on x.instanceId = i.instanceId")
	public List<ServerInstanceApplication> findAllServerInstanceApplications();

	// NOTE: strongly typed
	// NOTE: cannot use *: unexpected token: *
	// NOE: IllegalArgumentException in JPA are not reported verbosely
	// NOTE: org.springframework.core.convert.ConversionFailedException:
	// Failed to convert from type [java.lang.Object[]] to type
	// [example.model.Server]
	// for value '{101, hostname00}';
	// nested exception is
	// org.springframework.core.convert.ConverterNotFoundException:
	// No converter found capable of converting from type [java.lang.Integer] to
	// type [example.model.Server]] with root cause

	// @Query("select s.serverId, s.serverName from Server s where s.serverName =
	// ?1")
	@Query("select new Server(s.serverId, s.serverName) from Server s where s.serverName LIKE ?1%")
	public List<Server> findServer(String serverName);

	// Caused by: java.lang.IllegalArgumentException:
	// org.hibernate.hql.internal.ast.QuerySyntaxException:
	// unexpected token: REGEXP near line 1, column 92
	// [select new Server(s.serverId, s.serverName) from example.model.Server s
	// where s.serverName REGEXP ?1]
	// org.sqlite.SQLiteException:
	// [SQLITE_ERROR] SQL error or missing database
	// (no such function: REGEXP)
	// see also:
	// https://stackoverflow.com/questions/5071601/how-do-i-use-regex-in-a-sqlite-query
	// https://stackoverflow.com/questions/5071601/how-do-i-use-regex-in-a-sqlite-query
	// NOTE: Native SQL is case sensitive in table names, make sure tables are
	// named correctly
	// @Query(nativeQuery = true, value = "SELECT new
	// example.projection.Server(s.sid, s.sname) FROM server s WHERE s.sname
	// REGEXP ?1")
	@Query(nativeQuery = true, value = "SELECT s.sid serverId, s.sname serverName FROM server s WHERE REGEXP_LIKE(sname, ?1 )")
	public List<Object[]> findServersNativeRegexpRawData(
			String serverNamesRegexp);

	// Attempt to construct a nativeQuery creating array of strongly typed -
	// failing
	// NOTE: replacing the WHERE clause
	// WHERE s.sname REGEXP ?1
	// with
	// WHERE s.sname = ?1
	// does not help
	@Query(nativeQuery = true, value = "SELECT new example.projection.Server(s.sid, s.sname) FROM server s WHERE s.sname REGEXP ?1")
	public List<Server> findServersNativeRegexpTyped(String serverNamesRegexp);

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
	// @Query(value = "select sname as hostname,aname as application, iname as env
	// from axixs x join server s on x.sid = s.sid left join application a on
	// x.aid = a.aid join instance i on x.iid = i.iid", nativeQuery = true)
	@Query(nativeQuery = true, value = "select new example.projection.ServerInstanceApplication(sname,aname,iname) from axixs x join server s on x.sid = s.sid join application a on x.aid = a.aid join instance i on x.iid = i.iid")
	public List<ServerInstanceApplication> findAllServerInstanceApplicationsNative();
	// NOTE: use SQL notation: consult the class field mapping in the model package
	@Query(nativeQuery = true, value = "select sname,aname,iname from axixs x join server s on x.sid = s.sid join application a on x.aid = a.aid join instance i on x.iid = i.iid")
	public List<Object[]> findAllServerInstanceApplicationsNativeRaw();

	// NOTE: losely typed
	@Query("select s.serverName as hostname, 'dummy' as dc, a.applicationName as application, i.instanceName as env from Axixs x join Server s on x.serverId = s.serverId left join Application a on x.applicationId = a.applicationId join Instance i on x.instanceId = i.instanceId")
	public List<Object[]> findAllData();

	@Query(value = "select sname as hostname,'dummy' as dc,aname as application, iname as env from axixs x join server s on x.sid = s.sid left join application a on x.aid = a.aid join instance i on x.iid = i.iid", nativeQuery = true)
	public List<Object[]> findAllDataNative();
}
