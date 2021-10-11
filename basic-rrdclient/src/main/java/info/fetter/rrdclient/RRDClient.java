package info.fetter.rrdclient;

import java.util.Arrays;
import java.util.List;

/**
 * Class containing main method to be called from the command line.
 * 
 * @author Didier Fetter
 *
 */
public class RRDClient {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		List<String> remainingArgs = Arrays.asList(args);
		String rrdServer[] = remainingArgs.remove(0).split(":");
		String command = remainingArgs.remove(0);
		RRDCommand rrdCommand;
		
		if(command.equals("graph")) {
			@SuppressWarnings("unused")
			String fileName = remainingArgs.remove(0);
			rrdCommand = new GraphCommand(remainingArgs.toArray(new String[0]));
		} else if(command.equals("fetch")) {
			String fileName = remainingArgs.remove(0);
			String consolidationFunction = remainingArgs.remove(0);
			rrdCommand = new FetchCommand(fileName, consolidationFunction, remainingArgs.toArray(new String[0]));
		} else {
			throw new IllegalArgumentException("RRD command unknown : " + command);
		}
		
		rrdCommand.execute(System.out, rrdServer[0], Integer.parseInt(rrdServer[1]));
	}

	
	
}
