package example.notification;

import lombok.AllArgsConstructor;
import lombok.Data;

@Data
@AllArgsConstructor
public class Notification {

	private String word;
	private String line;
	private String file;
}
