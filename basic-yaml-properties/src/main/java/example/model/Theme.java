package example.model;

public class Theme {

	private String defaultFolder;

	public String getDefaultFolder() {
		return defaultFolder;
	}

	public void setDefaultFolder(String data) {
		defaultFolder = data;
	}

	@Override
	public String toString() {
		return this.getClass().getSimpleName() + "{" + "defaultFolder='"
				+ defaultFolder + '\'' + '}';
	}
}
