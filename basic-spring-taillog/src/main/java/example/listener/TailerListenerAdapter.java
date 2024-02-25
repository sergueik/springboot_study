package example.listener;

import org.apache.commons.io.input.Tailer;
import org.apache.commons.io.input.TailerListener;

public interface TailerListenerAdapter extends TailerListener {

	String getStatus();

	boolean isRunning(final Tailer tailer);

}
