package example.client.repository;

import com.datastax.driver.core.Session;

/**
 * Repository to handle the Cassandra schema.
 *
 */
public class KeyspaceRepository {
	private Session session;

	public KeyspaceRepository(Session session) {
		this.session = session;
	}

	public void createKeyspace(String keyspaceName, String replicationStrategy, int numberOfReplicas) {
		session.execute(String.format(
				"CREATE KEYSPACE IF NOT EXISTS %s WITH replication = {'class':'%s','replication_factor': %d }",
				keyspaceName, replicationStrategy, numberOfReplicas));
	}

	public void useKeyspace(String keyspace) {
		session.execute("USE " + keyspace);
	}

	public void deleteKeyspace(String keyspaceName) {
		session.execute(String.format("DROP KEYSPACE %s", keyspaceName));
	}
}
