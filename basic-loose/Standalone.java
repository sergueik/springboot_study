import java.util.Map;
public class Standalone {
	public static void main(String[] args) {

		System.out.println("Hello Java!");
printEnvironment();
	}
public static void printEnvironment() {
        Map<String, String> env = System.getenv();
        for (String key : env.keySet()) {
            System.out.println(key.toString() + ":\n\t\"" + env.get(key)
                    + "\"");
        }
    }
}
