package example;

public class App {

	private String value = Utils.name();
	private Helper helper = Utils.getHelper();

	public String getValue() {
		return value;
	}

	public String getHelperOperation() {
		return helper.operation();
	}

	public String getUtilsOperation() {
		Utils utils = Utils.getInstance();
		return utils.operation();
	}

	public String getUtilsInstanceHelperOperation() {
		Utils utils = Utils.getInstance();
		Helper helper = utils.getInstanceHelper();
		return helper.operation();
	}
}
