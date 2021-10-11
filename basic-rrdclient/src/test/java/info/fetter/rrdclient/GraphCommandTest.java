package info.fetter.rrdclient;

import static org.apache.log4j.Level.*;
import info.fetter.rrdclient.util.FetchServer;
import info.fetter.rrdclient.util.GraphServer;

import java.io.File;

import javax.imageio.ImageIO;

import org.apache.log4j.BasicConfigurator;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.apache.log4j.spi.RootLogger;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

public class GraphCommandTest {
	private static final Logger logger = LoggerFactory.getLogger(GraphCommandTest.class);

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
	public void testGraphCommandPseudo() {
		try {
			String[] args = new String[] {"-", "--imgformat=PNG", "--start=-86400", "--end=-300", "--title='toto'", "--base=1000", "--height=120", "--width=500", "--alt-autoscale-max", "--lower-limit=0", "--vertical-label='processes'", "--slope-mode", "--font", "TITLE:12:", "--font", "AXIS:8:", "--font", "LEGEND:10:", "--font", "UNIT:8:", "DEF:a=\"toto.rrd\":ucd_hrSystemProcess:AVERAGE AREA:a#F51D30FF:\"Running Processes\"", "GPRINT:a:LAST:\"Current\\:%8.0lf\"", "GPRINT:a:AVERAGE:\"Average\\:%8.0lf\"", "GPRINT:a:MAX:\"Maximum\\:%8.0lf\""};
			GraphCommand command = new GraphCommand(args);
			new GraphServer(13901, new File(FetchServer.class.getClassLoader().getResource("GraphResponse1.png").toURI()));
			command.execute("localhost", 13901);

			ImageIO.write(command.getImage(), "png", new File("target/test2.png"));
			
			command = new GraphCommand(args);
			command.execute("localhost", 13901);
			
			ImageIO.write(command.getImage(), "gif", new File("target/test2.gif"));
			
		} catch(Exception e) {
			if(logger.isDebugEnabled())
				e.printStackTrace();
		}
	}
	
	@Test
	public void dummyTest() {}
}
