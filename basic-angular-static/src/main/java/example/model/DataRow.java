package example.model;

public class DataRow {

	public static class DataRowBuilder {
		private String column1;
		private String column2;
		private String column3;

		public DataRowBuilder() {

		}

		public DataRowBuilder withColumn1(String data) {
			column1 = data;
			return this;
		}

		public DataRowBuilder withColumn3(String data) {
			column3 = data;
			return this;
		}

		public DataRowBuilder withColumn2(String data) {
			column2 = data;
			return this;
		}

		public DataRow build() {
			return new DataRow(this);
		}

	}

	private String column1;
	private String column2;
	private String column3;

	private DataRow(DataRowBuilder builder) {
		// implicly calls new DataRow();
		column1 = builder.column1;
		column2 = builder.column2;
		column3 = builder.column3;
	}

	public String getColumn1() {
		return column1;
	}

	public void setColumn1(String data) {
		column1 = data;
	}

	public String getColumn2() {
		return column2;
	}

	public void setColumn2(String data) {
		column2 = data;
	}

	public String getColumn3() {
		return column3;
	}

	public void setColumn3(String data) {
		column3 = data;
	}

	public DataRow(String column1) {
		this.column1 = column1;
	}

	private DataRow() {
	}

}
