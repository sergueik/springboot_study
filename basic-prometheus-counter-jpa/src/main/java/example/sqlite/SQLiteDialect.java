package example.sqlite;

import java.sql.Types;
import java.sql.SQLException;

import org.hibernate.dialect.Dialect;

import org.hibernate.dialect.function.AbstractAnsiTrimEmulationFunction;
import org.hibernate.dialect.function.NoArgSQLFunction;
import org.hibernate.dialect.function.SQLFunction;
import org.hibernate.dialect.function.SQLFunctionTemplate;
import org.hibernate.dialect.function.StandardSQLFunction;
import org.hibernate.dialect.function.VarArgsSQLFunction;
// https://github.com/gwenn/sqlite-dialect/blob/master/src/main/java/org/hibernate/dialect/identity/SQLiteDialectIdentityColumnSupport.java
// import org.hibernate.dialect.identity.SQLiteDialectIdentityColumnSupport;
// import org.hibernate.dialect.identity.IdentityColumnSupport;
import org.hibernate.dialect.pagination.AbstractLimitHandler;
import org.hibernate.dialect.pagination.LimitHandler;
import org.hibernate.dialect.pagination.LimitHelper;
import org.hibernate.dialect.unique.DefaultUniqueDelegate;
import org.hibernate.dialect.unique.UniqueDelegate;
import org.hibernate.engine.spi.RowSelection;
import org.hibernate.exception.DataException;
import org.hibernate.exception.JDBCConnectionException;
import org.hibernate.exception.LockAcquisitionException;
import org.hibernate.exception.spi.SQLExceptionConversionDelegate;
import org.hibernate.exception.spi.TemplatedViolatedConstraintNameExtracter;
import org.hibernate.exception.spi.ViolatedConstraintNameExtracter;

import org.hibernate.internal.util.JdbcExceptionHelper;

import org.hibernate.JDBCException;
import org.hibernate.mapping.Column;
import org.hibernate.ScrollMode;
import org.hibernate.type.StandardBasicTypes;
import org.hibernate.type.StringType;

/**
 * An SQL dialect for SQLite 3.
 */

public class SQLiteDialect extends Dialect {
	private final UniqueDelegate uniqueDelegate;

	public SQLiteDialect() {
		registerColumnType(Types.BIT, "integer");
		registerColumnType(Types.TINYINT, "tinyint");
		registerColumnType(Types.SMALLINT, "smallint");
		registerColumnType(Types.INTEGER, "integer");
		registerColumnType(Types.BIGINT, "bigint");
		registerColumnType(Types.FLOAT, "float");
		registerColumnType(Types.REAL, "real");
		registerColumnType(Types.DOUBLE, "double");
		registerColumnType(Types.NUMERIC, "numeric");
		registerColumnType(Types.DECIMAL, "decimal");
		registerColumnType(Types.CHAR, "char");
		registerColumnType(Types.VARCHAR, "varchar");
		registerColumnType(Types.LONGVARCHAR, "longvarchar");
		registerColumnType(Types.DATE, "date");
		registerColumnType(Types.TIME, "time");
		registerColumnType(Types.TIMESTAMP, "timestamp");
		registerColumnType(Types.BINARY, "blob");
		registerColumnType(Types.VARBINARY, "blob");
		registerColumnType(Types.LONGVARBINARY, "blob");
		// registerColumnType(Types.NULL, "null");
		registerColumnType(Types.BLOB, "blob");
		registerColumnType(Types.CLOB, "clob");
		registerColumnType(Types.BOOLEAN, "integer");

		registerFunction("concat",
				new VarArgsSQLFunction(StringType.INSTANCE, "", "||", ""));

		registerFunction("mod",
				new SQLFunctionTemplate(StringType.INSTANCE, "?1 % ?2"));

		registerFunction("substr",
				new StandardSQLFunction("substr", StringType.INSTANCE));
		// registerFunction( "substr", new StandardSQLFunction( "substr",
		// StandardBasicTypes.STRING ) );

		registerFunction("substring",
				new StandardSQLFunction("substr", StringType.INSTANCE));

		registerFunction("quote",
				new StandardSQLFunction("quote", StandardBasicTypes.STRING));

		registerFunction("random",
				new NoArgSQLFunction("random", StandardBasicTypes.INTEGER));

		registerFunction("round", new StandardSQLFunction("round"));

		registerFunction("trim", new AbstractAnsiTrimEmulationFunction() {
			protected SQLFunction resolveBothSpaceTrimFunction() {
				return new SQLFunctionTemplate(StandardBasicTypes.STRING, "trim(?1)");
			}

			protected SQLFunction resolveBothSpaceTrimFromFunction() {
				return new SQLFunctionTemplate(StandardBasicTypes.STRING, "trim(?2)");
			}

			protected SQLFunction resolveLeadingSpaceTrimFunction() {
				return new SQLFunctionTemplate(StandardBasicTypes.STRING, "ltrim(?1)");
			}

			protected SQLFunction resolveTrailingSpaceTrimFunction() {
				return new SQLFunctionTemplate(StandardBasicTypes.STRING, "rtrim(?1)");
			}

			protected SQLFunction resolveBothTrimFunction() {
				return new SQLFunctionTemplate(StandardBasicTypes.STRING,
						"trim(?1, ?2)");
			}

			protected SQLFunction resolveLeadingTrimFunction() {
				return new SQLFunctionTemplate(StandardBasicTypes.STRING,
						"ltrim(?1, ?2)");
			}

			protected SQLFunction resolveTrailingTrimFunction() {
				return new SQLFunctionTemplate(StandardBasicTypes.STRING,
						"rtrim(?1, ?2)");
			}
		});

		uniqueDelegate = new SQLiteUniqueDelegate(this);
	}
	// IDENTITY support ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	/*
	@Override
	  	public IdentityColumnSupport getIdentityColumnSupport() {
	      	return new SQLiteDialectIdentityColumnSupport(this);
	  	}
	*/

	public boolean supportsIdentityColumns() {
		return true;
	}

	/*
	public boolean supportsInsertSelectIdentity() {
	  return true; // As specify in NHibernate dialect
	}
	*/

	public boolean hasDataTypeInIdentityColumn() {
		return false; // As specify in NHibernate dialect
	}

	/*
	public String appendIdentitySelectToInsert(String insertString) {
	  return new StringBuffer(insertString.length()+30). // As specify in NHibernate dialect
	    append(insertString).
	    append("; ").append(getIdentitySelectString()).
	    toString();
	}
	*/

	public String getIdentityColumnString() {
		// return "integer primary key autoincrement";
		return "integer";
	}

	public String getIdentitySelectString() {
		return "select last_insert_rowid()";
	}

	@Override
	public boolean supportsLimit() {
		return true;
	}

	protected String getLimitString(String query, boolean hasOffset) {
		return new StringBuffer(query.length() + 20).append(query)
				.append(hasOffset ? " limit ? offset ?" : " limit ?").toString();
	}

	public boolean supportsTemporaryTables() {
		return true;
	}

	public String getCreateTemporaryTableString() {
		return "create temporary table if not exists";
	}

	// @Override
	public boolean dropTemporaryTableAfterUse() {
		return false;
	}

	// current timestamp support ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	@Override
	public boolean supportsCurrentTimestampSelection() {
		return true;
	}

	public boolean isCurrentTimestampSelectStringCallable() {
		return false;
	}

	@Override
	public String getCurrentTimestampSelectString() {
		return "select current_timestamp";
	}

	// SQLException support ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	private static final int SQLITE_BUSY = 5;
	private static final int SQLITE_LOCKED = 6;
	private static final int SQLITE_IOERR = 10;
	private static final int SQLITE_CORRUPT = 11;
	private static final int SQLITE_NOTFOUND = 12;
	private static final int SQLITE_FULL = 13;
	private static final int SQLITE_CANTOPEN = 14;
	private static final int SQLITE_PROTOCOL = 15;
	private static final int SQLITE_TOOBIG = 18;
	private static final int SQLITE_CONSTRAINT = 19;
	private static final int SQLITE_MISMATCH = 20;
	private static final int SQLITE_NOTADB = 26;

	// https://docs.jboss.org/hibernate/core/4.2/javadocs/org/hibernate/exception/spi/SQLExceptionConversionDelegate.html
	@Override
	public SQLExceptionConversionDelegate buildSQLExceptionConversionDelegate() {
		return new SQLExceptionConversionDelegate() {
			// @Override
			public JDBCException convert(SQLException sqlException, String message,
					String sql) {
				final int errorCode = JdbcExceptionHelper
						.extractErrorCode(sqlException);
				if (errorCode == SQLITE_TOOBIG || errorCode == SQLITE_MISMATCH) {
					return new DataException(message, sqlException, sql);
				} else if (errorCode == SQLITE_BUSY || errorCode == SQLITE_LOCKED) {
					return new LockAcquisitionException(message, sqlException, sql);
				} else if ((errorCode >= SQLITE_IOERR && errorCode <= SQLITE_PROTOCOL)
						|| errorCode == SQLITE_NOTADB) {
					return new JDBCConnectionException(message, sqlException, sql);
				}

				// returning null allows other delegates to operate
				return null;
			}
		};
	}

	public ViolatedConstraintNameExtracter getViolatedConstraintNameExtracter() {
		return EXTRACTER;
	}

	private static final ViolatedConstraintNameExtracter EXTRACTER = new TemplatedViolatedConstraintNameExtracter() {
		// @Override
		protected String doExtractConstraintName(SQLException sqle)
				throws NumberFormatException {
			final int errorCode = JdbcExceptionHelper.extractErrorCode(sqle);
			if (errorCode == SQLITE_CONSTRAINT) {
				return extractUsingTemplate("constraint ", " failed",
						sqle.getMessage());
			}
			return null;
		}

		public String extractConstraintName(SQLException arg0) {
			// TODO Auto-generated method stub
			return null;
		}
	};

	// lock acquisition support ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	@Override
	public boolean supportsLockTimeouts() {
		// may be http://sqlite.org/c3ref/db_mutex.html ?
		return false;
	}

	// union subclass support ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	public boolean supportsUnionAll() {
		return true;
	}

	// DDL support ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	// @Override
	public boolean canCreateSchema() {
		return true;
	}

	@Override
	public boolean hasAlterTable() {
		return false; // As specify in NHibernate dialect
	}

	@Override
	public boolean qualifyIndexName() {
		return false;
	}

	@Override
	public boolean dropConstraints() {
		return false;
	}

	@Override
	public String getAddColumnString() {
		return "add column";
	}

	@Override
	public String getSelectGUIDString() {
		return "select hex(randomblob(16))";
	}

	@Override
	public UniqueDelegate getUniqueDelegate() {
		return uniqueDelegate;
	}

	private static class SQLiteUniqueDelegate extends DefaultUniqueDelegate {
		public SQLiteUniqueDelegate(Dialect dialect) {
			super(dialect);
		}

		@Override
		public String getColumnDefinitionUniquenessFragment(Column column) {
			return " unique";
		}
	}

	@Override
	public String getForUpdateString() {
		return "";
	}

	@Override
	public boolean supportsOuterJoinForUpdate() {
		return false;
	}

	@Override
	public String getDropForeignKeyString() {
		throw new UnsupportedOperationException(
				"No drop foreign key syntax supported by SQLiteDialect class");
	}

	@Override
	public String getAddForeignKeyConstraintString(String constraintName,
			String[] foreignKey, String referencedTable, String[] primaryKey,
			boolean referencesPrimaryKey) {
		throw new UnsupportedOperationException(
				"No add foreign key syntax supported by SQLiteDialect class");
	}

	@Override
	public String getAddPrimaryKeyConstraintString(String constraintName) {
		throw new UnsupportedOperationException(
				"No add primary key syntax supported by SQLiteDialect");
	}

	@Override
	public boolean supportsCommentOn() {
		return true;
	}

	@Override
	public boolean supportsIfExistsBeforeTableName() {
		return true;
	}

	@Override
	public boolean supportsCascadeDelete() {
		return false;
	}

	@Override
	public ScrollMode defaultScrollMode() {
		return ScrollMode.FORWARD_ONLY;
	}
}