package example;

import java.io.File;
import org.apache.pivot.wtk.DesktopApplicationContext;
import org.apache.pivot.wtk.Application;
import org.apache.pivot.wtk.Window;
// https://javadoc.io/doc/org.apache.pivot/pivot-wtk/latest/index.html
import org.apache.pivot.wtk.FileBrowserSheet;
import org.apache.pivot.wtk.DialogCloseListener;
import org.apache.pivot.wtk.Button;
import org.apache.pivot.wtk.Component.UserDataDictionary;
import org.apache.pivot.wtk.PushButton;
import org.apache.pivot.wtk.Sheet;
import org.apache.pivot.wtk.SheetCloseListener;
import org.apache.pivot.wtk.BoxPane;
import org.apache.pivot.wtk.Orientation;
import org.apache.pivot.wtk.Label;
import org.apache.pivot.wtk.MessageType;
import org.apache.pivot.wtk.Alert;
import org.apache.pivot.collections.Map;
import org.apache.pivot.util.Filter;

public class App implements Application {
	private Window window = null;
	private Label selectedFileLabel = null;
	private UserDataDictionary userDataDictionary = null; 
	@Override
	public void startup(org.apache.pivot.wtk.Display display, Map<String, String> properties) throws Exception {
		BoxPane pane = new BoxPane(Orientation.VERTICAL);
		pane.getStyles().put("padding", 10);
		pane.getStyles().put("spacing", 10);

		selectedFileLabel = new Label("No file selected.");
		userDataDictionary = selectedFileLabel.getUserData();
		userDataDictionary.put("file", null);
		pane.add(selectedFileLabel);

		PushButton openButton = new PushButton("Open File Dialog");
		// see also:
		// https://pivot.apache.org/2.0.5/docs/api/org/apache/pivot/wtk/ButtonPressListener.html
		openButton.getButtonPressListeners().add(new org.apache.pivot.wtk.ButtonPressListener() {
			@Override
			public void buttonPressed(Button button) {
				FileBrowserSheet fileBrowserSheet = new FileBrowserSheet();
				// Set mode to OPEN for a file open dialog
				fileBrowserSheet.setMode(FileBrowserSheet.Mode.OPEN);
				// Optional: set a file filter (e.g., only show XML files)
				fileBrowserSheet.setDisabledFileFilter(new Filter<File>() {
					@Override
					public boolean include(File file) {
						return file.isDirectory() || file.getName().toLowerCase().endsWith(".xml");
					}
				});
				fileBrowserSheet.open(window, new SheetCloseListener() {
					@Override
					public void sheetClosed(Sheet sheet) {

						File selectedFile = fileBrowserSheet.getSelectedFile();
						if (selectedFile != null) {
							selectedFileLabel.setText("Selected file: " + selectedFile.getAbsolutePath());
							userDataDictionary.put("file", selectedFile.getAbsolutePath());
						} else {
							selectedFileLabel.setText("Dialog cancelled.");
						}
					}
				});
			}
		});

		pane.add(openButton);

		window = new Window();
		window.setContent(pane);
		window.setTitle("File Open Example");
		window.setMaximized(true);
		window.open(display);
	}

	@Override
	public boolean shutdown(boolean optional) {
		if (window != null) {
			window.close();
		}
		System.err.println("Selected file: " + selectedFileLabel.getUserData().get("file"));
		return false;
	}

	@Override
	public void suspend() {
	}

	@Override
	public void resume() {
	}

	public static void main(String[] args) {
		DesktopApplicationContext.main(App.class, args);
	}
}
