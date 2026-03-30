package example;

import java.io.IOException;

public class Client {

	public static void main(String[] args) throws IOException, Exception {
		// TODO Auto-generated method stub

		SQLDatabaseConnection cnn = new SQLDatabaseConnection();
		cnn.setServer("192.168.0.10");
		cnn.setPort(5000);
		cnn.setUsername("sysadm");
		cnn.setPassword("system");
		cnn.Open();

		// Java database client can only receive either Json or XML as response.
		// MultipleActiveResultSets will return multiple responses reducing round trips
		// and processing.
		// If multiple statements are executed without setting MultipleActiveResultSets
		// only last result is returned.
		// ExtendedResultSets=true; will return more details such as tables, sql query
		// etc.
		cnn.setConnectionString("Database=Orders;MultipleActiveResultSets=true;");

		SQLDatabaseCommand cmd = new SQLDatabaseCommand();
		cmd.setConnection(cnn);

		System.out.println("----Database Server Json Example----");
		String sql = "SELECT * FROM Customers LIMIT 1; SELECT * FROM Orders LIMIT 1; ";
		String json;
		json = cmd.ExecuteReader(false, sql, null);
		System.out.println(json);
		System.out.println("--------------------------------------------------------------------------");

		System.out.println("----Database Server XML Example----");
		String xml = cmd.ExecuteReader(true, sql, null);
		System.out.println(xml);
		System.out.println("--------------------------------------------------------------------------");

		System.out.println("----Cache Server Example----");
		SQLDatabaseCacheServer cs = new SQLDatabaseCacheServer();
		cs.setConnection(cnn);
		String cId = cs.Add("System.String", json.getBytes(), "", "json document", "1 Hour");
		// the above add function returns new cache id which is assigned to cId
		// variable.
		System.out.println("New Cache Id csId: " + cId);
		System.out.println("--------------------------------------------------------------------------");

		System.out.println("----List of all the collections----");
		for (String s : cs.CollectionList())
			System.out.println("Collection Name : " + s);

		System.out.println("--------------------------------------------------------------------------");

		// To drop collection
		Boolean IsDropped = cs.DropCollection("System.String");
		System.out.println("Cache drop example IsDropped: " + IsDropped);

		// Search by tags
		Object[] obj = cs.SearchByTags(true, "System.String", "json document");
		if ((obj != null) && (obj.length > 0)) {
			String objs = new String((byte[]) obj[0]);
			System.out.println(objs);
		}
		System.out.println("--------------------------------------------------------------------------");

		// Add or Update, example concatenates existing json string and removes tags
		json = json + "Add Or Update";
		cs.AddOrUpdate("System.String", json.getBytes(), "101", "", "1 Minute");

		// Update cache , example concatenates existing json string adds new tags
		json = json + "Add Or Update Json Document String";
		cs.Update("System.String", json.getBytes(), "101", "new JsonTags", "1 hour");
		byte[] bout = (byte[]) cs.Get(true, "System.String", "101");
		String jRet = new String(bout);
		System.out.println("Updated Object From Cache: " + jRet);
		System.out.println("--------------------------------------------------------------------------");

		if (cnn.getState() != ConnectionState.Close) {
			cnn.Close();
			System.out.println("Connection Closed");
		}

		System.exit(0);

	}

}
