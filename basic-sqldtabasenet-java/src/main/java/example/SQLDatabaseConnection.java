package example;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.net.UnknownHostException;
import java.nio.ByteBuffer;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.List;

public class SQLDatabaseConnection {

	Socket tcpClient = new Socket();

	String EndOfLine = "\0<EOL>\0";
	String EndOfHeader = "\0<EOH>\0";
	String EndOfMessage = "\0<EOF>\0";
	public final String[] DatabaseCommands = new String[] { "ExecuteNonQuery", "ExecuteScalar", "ExecuteReader" };

	private String server;
	private int port;
	private String username;
	private String password;
	private String connectionstring;
	private String databasename;
	private Boolean readcache = true;
	private Boolean donotcacheresults = false;
	private Boolean extendedresultsets = false;
	private Boolean multipleactiveresultsets = true;
	private String cachecollection;
	private String cacheexpiresin;
	private Boolean isauthenticated;
	private DatabaseResponseFormats responseformat = DatabaseResponseFormats.Binary;
	private ConnectionState state;

	public ConnectionState getState() {
		return state;
	}

	public void setState(ConnectionState state) {
		this.state = state;
	}

	public String getServer() {
		return server;
	}

	public void setServer(String server) {
		this.server = server;
	}

	public int getPort() {
		return port;
	}

	public void setPort(int port) {
		this.port = port;
	}

	public String getUsername() {
		return username;
	}

	public void setUsername(String username) {
		this.username = username;
	}

	public String getPassword() {
		return password;
	}

	public void setPassword(String password) {
		this.password = password;
	}

	public String getConnectionString() {
		return connectionstring;
	}

	public void setConnectionString(String connectionstring) {
		this.connectionstring = connectionstring;
	}

	public String getDatabaseName() {
		return databasename;
	}

	public void setDatabaseName(String databasename) {
		this.databasename = databasename;
	}

	public Boolean getReadCache() {
		return readcache;
	}

	public void setReadCache(Boolean readcache) {
		this.readcache = readcache;
	}

	public Boolean getDoNotCacheResults() {
		return donotcacheresults;
	}

	public void setDoNotCacheResults(Boolean donotcacheresults) {
		this.donotcacheresults = donotcacheresults;
	}

	public Boolean getExtendedResultSets() {
		return extendedresultsets;
	}

	public void setExtendedResultSets(Boolean extendedresultsets) {
		this.extendedresultsets = extendedresultsets;
	}

	public Boolean getMultipleActiveResultSets() {
		return multipleactiveresultsets;
	}

	public void setMultipleActiveResultSets(Boolean multipleactiveresultsets) {
		this.multipleactiveresultsets = multipleactiveresultsets;
	}

	public String getCacheCollection() {
		return cachecollection;
	}

	public void setCacheCollection(String cachecollection) {
		this.cachecollection = cachecollection;
	}

	public String getCacheExpiresIn() {
		return cacheexpiresin;
	}

	public void setCacheExpiresIn(String cacheexpiresin) {
		this.cacheexpiresin = cacheexpiresin;
	}

	public Boolean getIsAuthenticated() {
		return isauthenticated;
	}

	public void setIsAuthenticated(Boolean isauthenticated) {
		this.isauthenticated = isauthenticated;
	}

	public DatabaseResponseFormats getResponseFormat() {
		return responseformat;
	}

	public void setResponseFormat(DatabaseResponseFormats responseformat) {
		this.responseformat = responseformat;
	}

	public void Open() throws IOException {

		tcpClient.setTcpNoDelay(true);

		if (tcpClient.isConnected())
			throw new Error("Connection is already open.");

		if (server.isEmpty())
			throw new Error("Property Server must be set either with ip or name of server.");

		if (port < 1)
			throw new Error("Property Port must be set.");

		if (username.isEmpty())
			throw new Error("Username is required.");

		if (password.isEmpty())
			throw new Error("Password is required.");

		try {
			tcpClient.connect(new InetSocketAddress(server, port));

			if (tcpClient.isConnected())
				tcpClient.getOutputStream().write(AuthenticationRequest());
			else
				throw new Error("Unable to authenticate, connection is not open.");

			byte[] resultBuff = new byte[0];
			byte[] buff = new byte[1024];
			int bytesRead = -1;
			try {
				bytesRead = tcpClient.getInputStream().read(buff);
				byte[] tbuff = new byte[resultBuff.length + bytesRead];
				System.arraycopy(resultBuff, 0, tbuff, 0, resultBuff.length);
				System.arraycopy(buff, 0, tbuff, resultBuff.length, bytesRead);
				resultBuff = tbuff;
			} catch (IOException e) {
				System.err.println("IOException: " + e.getMessage());
			} catch (Exception e) {
				System.err.println("Exception: " + e.getMessage());
			}

			try {
				byte[] header = new byte[140];
				byte[] data = new byte[resultBuff.length - 154];
				byte[] footer = new byte[7];

				System.arraycopy(resultBuff, 0, header, 0, 140);
				System.arraycopy(resultBuff, 147, data, 0, resultBuff.length - 154);
				System.arraycopy(resultBuff, resultBuff.length - 7, footer, 0, 7);
				String sheader = new String(header);
				String[] headerparts = sheader.split(EndOfLine);

				if (headerparts[1].equalsIgnoreCase("SQLDATABASE_OK") == false)
					throw new Error(headerparts[1]);

			} catch (Exception e) {
				System.err.println("Exception: " + e.getMessage());
			}

			setState(ConnectionState.Open);

		} catch (UnknownHostException e) {
			throw new RuntimeException(e);
		} catch (IOException e) {
			System.err.println("IOException: " + e.getMessage());
		}
	}

	public void Close() throws IOException {
		if (tcpClient.isConnected()) {
			try {
				byte[] b = SendToServer("Close()".getBytes());
			} catch (Exception e) {

			}

			tcpClient.close();
		}

		setState(ConnectionState.Close);
	}

	public byte[] AuthenticationRequest() {
		if (username.isEmpty())
			throw new Error("Username is required.");

		if (password.isEmpty())
			throw new Error("Password is required.");

		StringBuilder sb = new StringBuilder();
		sb.append(username);
		sb.append(EndOfLine);
		sb.append(password);
		sb.append(EndOfLine);
		sb.append(DatabaseResponseFormats.Binary.toString());
		sb.append(EndOfLine);
		sb.append("Authenticate");
		sb.append(EndOfLine);
		sb.append(EndOfMessage);
		return sb.toString().getBytes();

	}

	private byte[] GetDatabaseRequestHeader(String Command) {
		if (getState() != ConnectionState.Open)
			throw new Error("Connection is not open.");

		String conn_str = "";

		if (getResponseFormat() == DatabaseResponseFormats.None)
			throw new Error("Response Format property must be set.");

		if ((getConnectionString().isEmpty()) || (getConnectionString().indexOf("=") == -1)) {
			if (getDatabaseName().isEmpty())
				throw new Error("Database name is required.");
			else
				conn_str += "Database = " + getDatabaseName() + ";";

			if (getReadCache()) // optional default is true since database cache is useful
				conn_str += "ReadCache = false;";

			if (getDoNotCacheResults() == true) // optional default is false since cache is used for next request
				conn_str += "DoNotCacheResults = true;";

			if (getMultipleActiveResultSets() == true) // optional default is false since cache is used for next request
				conn_str += "MultipleActiveResultSets = true;";

			if (getExtendedResultSets() == true) // optional default is false since cache is used for next request
				conn_str += "ExtendedResultSets = true;";
		}

		if (!conn_str.isEmpty())
			setConnectionString(conn_str);

		StringBuilder sb = new StringBuilder();
		sb.append(username);
		sb.append(EndOfLine);
		sb.append(password);
		sb.append(EndOfLine);
		sb.append(getResponseFormat().toString());
		sb.append(EndOfLine);
		sb.append(Command);
		sb.append(EndOfLine);
		sb.append(getConnectionString().toString());
		sb.append(EndOfLine);
		return sb.toString().getBytes();
	}

	public byte[] GetCacheRequestHeader(String Command, String CacheId, String CacheTags, String ExpiresIn) {
		if (getState() != ConnectionState.Open)
			throw new Error("Connection is not open.");

		if (getCacheCollection().isEmpty())
			setCacheCollection("Default");

		StringBuilder sb = new StringBuilder();
		sb.append(username);
		sb.append(EndOfLine);
		sb.append(password);
		sb.append(EndOfLine);
		sb.append(DatabaseResponseFormats.Binary.toString());// Cache server only responds as binary to return original
																// object
		sb.append(EndOfLine);
		sb.append(Command);
		sb.append(EndOfLine);
		sb.append(getCacheCollection());
		sb.append(EndOfLine);
		sb.append(CacheId);
		sb.append(EndOfLine);
		sb.append(CacheTags);
		sb.append(EndOfLine);
		sb.append(ExpiresIn);
		sb.append(EndOfLine);

		return sb.toString().getBytes();
	}

	public byte[] GetRequestFooter() {
		StringBuilder sb = new StringBuilder();
		sb.append(EndOfLine);
		sb.append(EndOfMessage);

		return sb.toString().getBytes();
	}

	private byte[] CreateRequestBytes(byte[] header, byte[] data) {
		byte[] footer = GetRequestFooter();
		byte[] RequestBytes = new byte[header.length + data.length + footer.length];

		System.arraycopy(header, 0, RequestBytes, 0, header.length);
		System.arraycopy(data, 0, RequestBytes, header.length, data.length);
		System.arraycopy(footer, 0, RequestBytes, (header.length + data.length), footer.length);

		return RequestBytes;
	}

	public byte[] SendToServer(byte[] b) {
		if (!tcpClient.isConnected())
			throw new Error("Connection not open");

		try {
			tcpClient.getOutputStream().write(b, 0, b.length);
			setState(ConnectionState.Sent);
		} catch (IOException e) {
			throw new RuntimeException(e);
		}

		byte[] returnedBytes = new byte[0];
		try {
			int bytes = -1;
			do {
				setState(ConnectionState.Wait);

				bytes = -1;
				byte[] buffer = new byte[8192];
				bytes = tcpClient.getInputStream().read(buffer, 0, buffer.length);

				byte[] tbuff = new byte[returnedBytes.length + bytes];
				System.arraycopy(returnedBytes, 0, tbuff, 0, returnedBytes.length);
				System.arraycopy(buffer, 0, tbuff, returnedBytes.length, bytes);
				returnedBytes = tbuff;

				String s = new String(returnedBytes);
				if (s.indexOf("\0<EOF>\0") != -1) {
					break;
				}

			} while (bytes != 0);

		} catch (IOException ioe) {
		} finally {
			setState(ConnectionState.Open);
		}

		return returnedBytes;
	}

	private String[] getResponseHeader(byte[] ResponseBytes) {
		byte[] header = new byte[140];

		System.arraycopy(ResponseBytes, 0, header, 0, 140);

		String ResponseString = new String(header);
		String[] HeaderArray = ResponseString.split(EndOfLine);

		return HeaderArray;
	}

	private byte[] getResponseData(byte[] ResponseBytes) {
		byte[] data = new byte[ResponseBytes.length - 154];

		System.arraycopy(ResponseBytes, 147, data, 0, data.length);

		return data;
	}

	public String ExecuteDatabaseCommand(String DatabaseServerCommand, byte[] b) {

		if (DatabaseServerCommand.isEmpty())
			return "#ERROR# DatabaseServerCommand cannot be empty.";

		if (Arrays.asList(DatabaseCommands).contains(DatabaseServerCommand) == false)
			return "#ERROR# DatabaseServerCommand is invalid, the invalid command is : " + DatabaseServerCommand;

		byte[] RequestBytes = CreateRequestBytes(GetDatabaseRequestHeader(DatabaseServerCommand), b);

		byte[] ResponseBytes = new byte[0];

		try {
			ResponseBytes = SendToServer(RequestBytes);
		} catch (Exception e) {
			return "#ERROR# " + e.getMessage();
		}

		if (ResponseBytes.length > 147) {
			String Header[] = getResponseHeader(ResponseBytes);

			if (Header.length < 3)
				return "#ERROR# Invalid response header returned from server";

			if ((Header[1].isEmpty()) || (Header[1].equalsIgnoreCase("SQLDATABASE_OK"))) {
				byte[] data = getResponseData(ResponseBytes);
				String json = new String(data);
				return json;
			} else {
				return "#ERROR# Unable to read all the bytes from server. Check client server connectivity.";
			}

		} else {
			return "#ERROR# Unable to read all the bytes from server. Check client server connectivity.";
		}
	}

	public static byte[] ObjectToByteArray(Object obj) throws IOException {
		ByteArrayOutputStream out = new ByteArrayOutputStream();
		ObjectOutputStream os = new ObjectOutputStream(out);
		os.writeObject(obj);
		return out.toByteArray();
	}

	public static Object ByteArrayToObject(byte[] data) throws IOException, ClassNotFoundException {
		ByteArrayInputStream in = new ByteArrayInputStream(data);
		ObjectInputStream is = new ObjectInputStream(in);
		return is.readObject();
	}

	public String ExecuteCacheCommand(String CacheServerCommand, Object ObjectToCache, String CacheId, String CacheTags,
			String ExpiresIn) throws Exception {

		byte[] b = new byte[0];

		if (ObjectToCache instanceof byte[]) {
			b = (byte[]) ObjectToCache;
		} else {
			try {
				b = ObjectToByteArray(ObjectToCache);
			} catch (IOException e) {
				return "#ERROR# " + e.getMessage();
			}

		}

		if (ExpiresIn.isEmpty())
			ExpiresIn = "1 Day";

		byte[] RequestBytes = CreateRequestBytes(
				GetCacheRequestHeader(CacheServerCommand, CacheId, CacheTags, ExpiresIn), b);
		byte[] ResponseBytes = null;
		try {
			ResponseBytes = SendToServer(RequestBytes);
		} catch (Exception e) {
			return "#ERROR# " + e.getMessage();
		}

		if ((ResponseBytes != null) && (ResponseBytes.length >= 147)) {
			String Header[] = getResponseHeader(ResponseBytes);

			if (Header.length < 3)
				return "#ERROR# Invalid response header returned from server";

			if ((Header[1].isEmpty()) || (Header[1].equalsIgnoreCase("SQLDATABASE_OK"))) {
				byte[] ResponseData = getResponseData(ResponseBytes);
				String stringData = new String(ResponseData);
				return stringData;
			} else {
				return "#ERROR# Unable to read all the bytes from server. Check client server connectivity.";
			}

		} else {
			return "#ERROR# Unable to read all the bytes from server. Check client server connectivity.";
		}
	}

	public Object ExecuteGetCacheCommand(Boolean ReturnRawBytes, String CacheId) {

		Object CacheObject = new Object();
		byte[] b = new byte[0];

		byte[] RequestBytes = CreateRequestBytes(
				GetCacheRequestHeader(ServerCommands.CacheGet.toString(), CacheId, "", ""), b);
		byte[] ResponseBytes = null;
		try {
			ResponseBytes = SendToServer(RequestBytes);
		} catch (Exception e) {
			return "#ERROR# " + e.getMessage();
		}

		if ((ResponseBytes != null) && (ResponseBytes.length >= 147)) {
			String Header[] = getResponseHeader(ResponseBytes);

			if (Header.length < 3)
				return "#ERROR# Invalid response header returned from server";

			if ((Header[1].isEmpty()) || (Header[1].equalsIgnoreCase("SQLDATABASE_OK"))) {
				byte[] ResponseData = getResponseData(ResponseBytes);

				try {
					if (ReturnRawBytes)
						return ResponseData;
					else
						CacheObject = ByteArrayToObject(ResponseData);
					if (CacheObject == null) {
						return "#ERROR# Unknown Error";
					}
				} catch (Exception e) {
					return "#ERROR# " + e.getMessage();
				}
			} else {
				return "#ERROR# Unable to read all the bytes from server. Check client server connectivity.";
			}

			return CacheObject;
		} else {
			return "#ERROR# Unable to read all the bytes from server. Check client server connectivity.";
		}
	}

	public String ExecuteRemoveCacheCommand(String CacheId) {

		byte[] b = new byte[0];

		byte[] RequestBytes = CreateRequestBytes(
				GetCacheRequestHeader(ServerCommands.CacheRemove.toString(), CacheId, "", ""), b);

		byte[] ResponseBytes = null;
		try {
			ResponseBytes = SendToServer(RequestBytes);
		} catch (Exception e) {
			return "#ERROR# " + e.getMessage();
		}

		if ((ResponseBytes != null) && (ResponseBytes.length >= 147)) {

			String Header[] = getResponseHeader(ResponseBytes);

			if (Header.length < 3)
				return "#ERROR# Invalid response header returned from server";

			if ((Header[1].isEmpty()) || (Header[1].equalsIgnoreCase("SQLDATABASE_OK"))) {
				return "true";
			} else {
				return "false";
			}
		} else {
			return "#ERROR# Unable to read all the bytes from server. Check client server connectivity.";
		}
	}

	public Object[] ExecuteSearchCacheCommand(Boolean ReturnRawBytes, String CacheTags) {
		String ErrorMessage = "";

		ByteBuffer bb;

		List<Object> ObjectsToReturn = new ArrayList<Object>();

		byte[] b = new byte[0];

		byte[] RequestBytes = CreateRequestBytes(
				GetCacheRequestHeader(ServerCommands.CacheSearch.toString(), "", CacheTags, ""), b);
		byte[] ResponseBytes = null;
		try {
			ResponseBytes = SendToServer(RequestBytes);
		} catch (Exception e) {
			ErrorMessage = "#ERROR# " + e.getMessage();
		}

		if ((ResponseBytes != null) && (ResponseBytes.length >= 147)) {

			String Header[] = getResponseHeader(ResponseBytes);

			if (Header.length < 3)
				ErrorMessage = "#ERROR# Invalid response header returned from server";

			if ((Header[1].isEmpty()) || (Header[1].equalsIgnoreCase("SQLDATABASE_OK"))) {
				byte[] ResponseData = getResponseData(ResponseBytes);

				try {
					String ResponseDataString = new String(ResponseData);
					String[] ObjectArrayLength = ResponseDataString.split(EndOfLine);
					int[] ObjectLength = new int[ObjectArrayLength[0].split(",").length];

					int offSet = ResponseDataString.indexOf(EndOfLine) + 7;

					String[] len = ObjectArrayLength[0].split(",");

					for (int i = 0; i < len.length; i++) {
						int l = Integer.parseInt(len[i]);
						ObjectLength[i] = l;
					}

					for (int i = 0; i < ObjectLength.length; i++) {
						byte[] bArray = new byte[ObjectLength[i]];
						System.arraycopy(ResponseData, offSet, bArray, 0, bArray.length);

						if (bArray.length > 0) {
							try {
								if (ReturnRawBytes)
									ObjectsToReturn.add(bArray);
								else
									ObjectsToReturn.add(ByteArrayToObject(bArray));
							} catch (Exception e) {
								ErrorMessage = "#ERROR# " + e.getMessage();
							}
						}

						offSet += bArray.length;
					}
				} catch (Exception e) {
					ErrorMessage = "#ERROR# " + e.getMessage();
				}
			} else {
				ErrorMessage = "#ERROR# Unable to read all the bytes from server. Check client server connectivity.";
			}

		} else {
			ErrorMessage = "#ERROR# Unable to read all the bytes from server. Check client server connectivity.";
		}

		return ObjectsToReturn.toArray();

	}

}
