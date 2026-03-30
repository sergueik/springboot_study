package example;

import java.util.ArrayList;
import java.util.List;

public class SQLDatabaseCommand {

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

	private byte[] GetCommandBytes(String SQLCommand, Object[] Parameters) {
		List<Byte> lst = new ArrayList<Byte>();

		for (byte b : SQLCommand.getBytes()) {
			lst.add(b);
		}

		for (byte b : EndOfLine.getBytes()) {
			lst.add(b);
		}

		if (Parameters != null) {
			for (int i = 0; i < Parameters.length; i++) {
				if (Parameters[i] != null) {
					for (byte b : Parameters[i].toString().getBytes()) {
						lst.add(b);
					}

					for (byte b : EndOfLine.getBytes()) {
						lst.add(b);
					}
				}
			}

		}

		byte[] result = new byte[lst.size()];
		for (int i = 0; i < lst.size(); i++) {
			result[i] = lst.get(i).byteValue();
		}
		return result;
	}

	public String ExecuteNonQuery(Boolean AsXML, String SQLCommand, Object[] Parameters) {
		CheckIfConnectionIsOpen();

		byte[] b = GetCommandBytes(SQLCommand, Parameters);
		if (AsXML)
			getConnection().setResponseFormat(DatabaseResponseFormats.XML);
		else
			getConnection().setResponseFormat(DatabaseResponseFormats.JSON);

		String rs = getConnection().ExecuteDatabaseCommand("ExecuteNonQuery", b);
		if (rs.startsWith("#ERROR#"))
			throw new Error(rs);
		else
			return rs;
	}

	public String ExecuteScalar(Boolean AsXML, String SQLCommand, Object[] Parameters) {
		CheckIfConnectionIsOpen();

		byte[] b = GetCommandBytes(SQLCommand, Parameters);
		if (AsXML)
			getConnection().setResponseFormat(DatabaseResponseFormats.XML);
		else
			getConnection().setResponseFormat(DatabaseResponseFormats.JSON);

		String rs = getConnection().ExecuteDatabaseCommand("ExecuteScalar", b);
		if (rs.startsWith("#ERROR#"))
			throw new Error(rs);
		else
			return rs;
	}

	public String ExecuteReader(Boolean AsXML, String SQLCommand, Object[] Parameters) {
		CheckIfConnectionIsOpen();

		byte[] b = GetCommandBytes(SQLCommand, Parameters);
		if (AsXML)
			getConnection().setResponseFormat(DatabaseResponseFormats.XML);
		else
			getConnection().setResponseFormat(DatabaseResponseFormats.JSON);

		String rs = getConnection().ExecuteDatabaseCommand("ExecuteReader", b);
		if (rs.startsWith("#ERROR#"))
			throw new Error(rs);
		else
			return rs;
	}

}
