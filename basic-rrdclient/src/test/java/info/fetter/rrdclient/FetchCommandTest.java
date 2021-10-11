package info.fetter.rrdclient;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import info.fetter.rrdclient.util.FetchServer;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.spi.RootLogger;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

public class FetchCommandTest {
	private static final Logger logger = LoggerFactory.getLogger(FetchCommandTest.class);

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		// BasicConfigurator.configure();
		// RootLogger.getRootLogger().setLevel(DEBUG);
	}

	@AfterClass
	public static void tearDownAfterClass() throws Exception {
		// BasicConfigurator.resetConfiguration();
	}

	@Test
	public void testFetchCommandOK() {
		try {
			String fileName = "toto.rrd";
			String CF = "AVERAGE";
			String[] args = new String[] {"--start" , "-1m", "--resolution", "86400"};
			FetchCommand command = new FetchCommand(fileName, CF, args);
			@SuppressWarnings("unused")
			FetchServer server = new FetchServer(13900,new File(FetchServer.class.getClassLoader().getResource("FetchResponse1.txt").toURI()));
			//command.execute("localhost", 13900);
			command.setServerAddress("localhost");
			command.setServerPort(13900);
			command.execute();
			System.out.println(command.getDataTable().toJSON());

		} catch(Exception e) {
			if(logger.isDebugEnabled())
				e.printStackTrace();
		}
	}

	@Test(expected = RRDToolError.class)
	public void testFetchCommandKO() throws IOException, URISyntaxException {
		String fileName = "toto.rrd";
		String CF = "AVERAGE";
		String[] args = new String[] {"--start" , "-1m", "--resolution", "86400"};
		FetchCommand command = new FetchCommand(fileName, CF, args);
		@SuppressWarnings("unused")
		FetchServer server = new FetchServer(13902,new File(FetchServer.class.getClassLoader().getResource("FetchResponse2.txt").toURI()));
		command.execute("localhost", 13902);
	}

	@Test
	public void dummyTest() {}
}
