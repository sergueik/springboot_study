package example;

public class SQLDatabaseCacheServer {

	String EndOfLine = "\0<EOL>\0";

	private SQLDatabaseConnection connection;

	public SQLDatabaseConnection getConnection() {
		return connection;
	}

	public void setConnection(SQLDatabaseConnection connection) {
		this.connection = connection;
	}

	private void CheckIfConnectionIsOpen() {
		if (getConnection() == null)
			throw new Error("SQLDatabaseCommand Connection property not set.");

		if (getConnection().getState() != ConnectionState.Open)
			throw new Error("Connection must be open before command can be executed.");
	}

	public String Add(String CollectionName, Object ObjectToCache, String CacheId, String Tags, String ExpiresIn)
			throws Exception {

		CheckIfConnectionIsOpen();

		if (ObjectToCache == null)
			throw new Exception("Cannot cache a null object.");

		if ((CollectionName == null) || (CollectionName.isEmpty()))
			throw new Exception("CollectionName is required.");
		else
			getConnection().setCacheCollection(CollectionName);

		if ((ExpiresIn == null) || (ExpiresIn.isEmpty()))
			ExpiresIn = "1 Day";

		String rs = getConnection().ExecuteCacheCommand("CacheAdd", ObjectToCache, CacheId, Tags, ExpiresIn);

		if (rs.startsWith("#ERROR#"))
			throw new Exception(rs);

		return rs;
	}

	public Object Get(Boolean ReturnRawBytes, String CollectionName, String CacheId) throws Exception {

		CheckIfConnectionIsOpen();

		if ((CollectionName == null) || (CollectionName.isEmpty()))
			throw new Exception("CollectionName is required.");
		else
			getConnection().setCacheCollection(CollectionName);

		if ((CacheId == null) || (CacheId.isEmpty()))
			throw new Exception("CacheId is required.");

		Object ro = getConnection().ExecuteGetCacheCommand(ReturnRawBytes, CacheId);

		try {

			if ((ro instanceof String) && (ro.toString().startsWith("#ERROR#")))
				throw new Exception(ro.toString());

		} catch (Exception e) {

		}

		return ro;
	}

	public Boolean Remove(String CollectionName, String CacheId) throws Exception {

		CheckIfConnectionIsOpen();

		if ((CollectionName == null) || (CollectionName.isEmpty()))
			throw new Exception("CollectionName is required.");
		else
			getConnection().setCacheCollection(CollectionName);

		if ((CacheId == null) || (CacheId.isEmpty()))
			throw new Exception("CacheId is required.");

		String rs = getConnection().ExecuteRemoveCacheCommand(CacheId);

		if (rs.startsWith("#ERROR#"))
			throw new Exception(rs);

		if (rs.equals("true"))
			return true;
		else
			return false;
	}

	public String Update(String CollectionName, Object ObjectToCache, String CacheId, String Tags, String ExpiresIn)
			throws Exception {

		CheckIfConnectionIsOpen();

		if (ObjectToCache == null)
			throw new Exception("Cannot cache a null object.");

		if ((CollectionName == null) || (CollectionName.isEmpty()))
			throw new Exception("CollectionName is required.");
		else
			getConnection().setCacheCollection(CollectionName);

		if ((CacheId == null) || (CacheId.isEmpty()))
			throw new Exception("CacheId is required for update.");

		if ((ExpiresIn == null) || (ExpiresIn.isEmpty()))
			ExpiresIn = "1 Day";

		String rs = getConnection().ExecuteCacheCommand("CacheUpdate", ObjectToCache, CacheId, Tags, ExpiresIn);

		if (rs.startsWith("#ERROR#"))
			throw new Exception(rs);

		return rs;
	}

	public String AddOrUpdate(String CollectionName, Object ObjectToCache, String CacheId, String Tags,
			String ExpiresIn) throws Exception {

		CheckIfConnectionIsOpen();

		if (ObjectToCache == null)
			throw new Exception("Cannot cache a null object.");

		if ((CollectionName == null) || (CollectionName.isEmpty()))
			throw new Exception("CollectionName is required.");
		else
			getConnection().setCacheCollection(CollectionName);

		if ((CacheId == null) || (CacheId.isEmpty()))
			throw new Exception("CacheId is required for AddOrUpdate.");

		if ((ExpiresIn == null) || (ExpiresIn.isEmpty()))
			ExpiresIn = "1 Day";

		String rs = getConnection().ExecuteCacheCommand("CacheAddOrUpdate", ObjectToCache, CacheId, Tags, ExpiresIn);

		if (rs.startsWith("#ERROR#"))
			throw new Exception(rs);

		return rs;
	}

	public String[] CollectionList() throws Exception {
		CheckIfConnectionIsOpen();

		Object obj = "\0<EMPTY>\0";

		String CollectionList = getConnection().ExecuteCacheCommand("CacheCollectionList", obj, "", "", "");

		if (CollectionList.startsWith("#ERROR#"))
			throw new Exception(CollectionList);

		String[] Collections = CollectionList.split("\0<EOL>\0");

		return Collections;
	}

	public String[] CollectionCacheIds() throws Exception {
		CheckIfConnectionIsOpen();

		Object obj = "\0<EMPTY>\0";

		String CollectionIdsList = getConnection().ExecuteCacheCommand("CacheCollectionCacheIds", obj, "", "", "");

		if (CollectionIdsList.startsWith("#ERROR#"))
			throw new Error(CollectionIdsList);

		String[] CollectionIds = CollectionIdsList.split("\0>EOL>\0");

		return CollectionIds;
	}

	public Boolean DropCollection(String CollectionName) throws Exception {

		CheckIfConnectionIsOpen();

		if ((CollectionName == null) || (CollectionName.isEmpty()))
			throw new Exception("CollectionName is required.");
		else
			getConnection().setCacheCollection(CollectionName);

		Object obj = "\0<EMPTY>\0";

		String rs = getConnection().ExecuteCacheCommand("CacheDropCollection", obj, "", "", "");

		if (rs.startsWith("#ERROR#"))
			throw new Exception(rs);

		if (rs.equals("Success"))
			return true;
		else
			return false;
	}

	public Integer Count(String CollectionName) throws Exception {
		CheckIfConnectionIsOpen();

		if ((CollectionName == null) || (CollectionName.isEmpty()))
			throw new Exception("CollectionName is required.");
		else
			getConnection().setCacheCollection(CollectionName);

		Object obj = "\0<EMPTY>\0";

		String CollectionCount = getConnection().ExecuteCacheCommand("CacheCollectionCount", obj, "", "", "");

		if (CollectionCount.startsWith("#ERROR#"))
			throw new Error(CollectionCount);

		Integer IntCount = Integer.parseInt(CollectionCount);

		return IntCount;
	}

	public Object[] SearchByTags(Boolean ReturnRawBytes, String CollectionName, String Tags) throws Exception {
		CheckIfConnectionIsOpen();

		if ((CollectionName == null) || (CollectionName.isEmpty()))
			throw new Exception("CollectionName is required.");
		else
			getConnection().setCacheCollection(CollectionName);

		return getConnection().ExecuteSearchCacheCommand(ReturnRawBytes, Tags);

	}

}
