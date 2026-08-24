package example;

/**
 * Copyright 2026 Serguei Kouzmine
 */

import javax.imageio.ImageIO;

// import com.google.gson.Gson;
// import com.google.gson.GsonBuilder;

// import example.utils.Generator;

import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.nio.file.Paths;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Runner {

	private static boolean debug = false;
	// private static Gson gson = new
	// GsonBuilder().setPrettyPrinting().serializeNulls().create();
	static final int COLS = 80;
	static final int ROWS = 24;

	static final int FONT_SIZE = 24;

	static final int LEFT = 30;
	static final int TOP = 30;

	public static void main(String[] args) throws Exception {

		Map<String, String> cli = parseArgs(args);

		String outputFile = "console.png";
		String fontPath = null;
		// reserved for future
		String labelFile = "console.json";
		String screenfile = null;
		String page = "cp037"; // EBCDIC
		Long words = 1L;
		Long length = 1L;
		String foregroundColor = null;
		String backgroundColor = null;
		String degraded = null;

		if (cli.containsKey("debug")) {
			debug = true;
		}

		if (debug)
			System.err.println(cli.keySet());

		if (cli.containsKey("help") || !cli.containsKey("outputfile") || !cli.containsKey("screenfile")) {
			System.err.println(
					String.format("Usage: jar " + "-screenfile <filename> -outputfile <filename> -font <font>\r\n"));
			System.err.println(cli.keySet());
			return;
		}
		if (cli.containsKey("outputfile"))
			outputFile = cli.get("outputfile");
		if (cli.containsKey("screenfile"))
			screenfile = cli.get("screenfile");
		if (cli.containsKey("words"))
			words = Long.parseLong(cli.get("words"));

		if (cli.containsKey("page"))
			page = cli.get("page");
		if (cli.containsKey("font"))
			fontPath = cli.get("font");
		if (cli.containsKey("background"))
			backgroundColor = cli.get("background");
		if (cli.containsKey("foreground"))
			foregroundColor = cli.get("foreground");
		if (cli.containsKey("degraded"))
			degraded = cli.get("degraded");
		if (cli.containsKey("labelfile"))
			labelFile = cli.get("labelfile");
		// new Generator(copybookFile, outputFile, page, maxRows).generate();

		List<String> screenLines = new ArrayList<>();

		Path path = Paths.get(screenfile);

		try {
			screenLines = Files.readAllLines(path);
		} catch (IOException e) {
			e.printStackTrace();
		}
		// To achieve a lookalike screenshot of Blue Prism IBM 3270 mainframe/CICS
		// terminal emulator
		// one may use the open-source TrueType font
		// download
		// https://github.com/rbanffy/3270font

		// Fallback for a standard monospace font
		Font font = new Font(Font.MONOSPACED, Font.PLAIN, FONT_SIZE);
		if (fontPath == null)
			fontPath = System.getenv().containsKey("FONT_PATH") ? System.getenv("FONT_PATH")
					: getOSName().equals("windows")
							? Paths.get(System.getProperty("user.home")).resolve("Downloads")
									.resolve("3270NerdFontMono-Regular.ttf").toAbsolutePath().toString()
							: "/usr/share/fonts/opentype/3270/3270-Regular.otf";

		// String name = "3270 Nerd Font Mono";
		font = Font.createFont(Font.TRUETYPE_FONT, new File(fontPath)).deriveFont((float) FONT_SIZE);

		// create a temporary image to compute FontMetrics.
		BufferedImage metricsImage = new BufferedImage(1, 1, BufferedImage.TYPE_INT_RGB);

		Graphics2D graphics2 = metricsImage.createGraphics();
		graphics2.setFont(font);
		FontMetrics fontMetrics = graphics2.getFontMetrics();

		int cellWidth = fontMetrics.charWidth('M');
		int cellHeight = fontMetrics.getHeight();

		graphics2.dispose();

		int width = LEFT * 2 + COLS * cellWidth;
		int height = TOP * 2 + ROWS * cellHeight;

		BufferedImage bufferedImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);

		Graphics2D graphics = bufferedImage.createGraphics();

		// ---- Terminal appearance -------------------------------------

		graphics.setColor(Color.BLACK);
		graphics.fillRect(0, 0, width, height);

		graphics.setFont(font);
		graphics.setColor(Color.GREEN);

		// Slightly nicer text rendering for the initial clean version.
		graphics.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON);

		// ===============================================================
		// MODE 1: LINE-GRANULAR
		// ===============================================================

		boolean letterGranular = true;

		if (!letterGranular) {

			for (int row = 0; row < screenLines.size(); row++) {

				String line = screenLines.get(row);

				int x = LEFT;
				int y = TOP + fontMetrics.getAscent() + row * cellHeight;

				graphics.drawString(line, x, y);
			}
		}

		// ===============================================================
		// MODE 2: CHARACTER-GRANULAR
		// ===============================================================

		if (letterGranular) {

			for (int row = 0; row < screenLines.size(); row++) {

				String line = screenLines.get(row);

				for (int col = 0; col < line.length(); col++) {

					String letter = String.valueOf(line.charAt(col));

					int x = LEFT + col * cellWidth;
					int y = TOP + fontMetrics.getAscent() + row * cellHeight;

					/*
					 * Future deliberate imperfection hooks:
					 *
					 * x += randomOffset(-1, 1); y += randomOffset(-1, 1);
					 *
					 * change brightness per character omit a pixel blur selected cells slightly
					 * alter spacing
					 */

					graphics.drawString(letter, x, y);
				}
			}
		}

		graphics.dispose();

		ImageIO.write(bufferedImage, "png", new File(outputFile));

		System.out.println(String.format("Wrote %s", outputFile));

	}

	private static String osName;

	public static String getOSName() {
		if (osName == null) {
			osName = System.getProperty("os.name").toLowerCase();
			if (osName.startsWith("windows")) {
				osName = "windows";
			}
		}
		return osName;
	}

	// Extremely simple CLI parser: -key value
	private static Map<String, String> parseArgs(String[] args) {
		if (String.join("", args).indexOf("debug") != -1) {
			System.err.println("xxx");
		}

		Map<String, String> map = new HashMap<>();
		for (int i = 0; i < args.length - 1; i++) {
			if (args[i].startsWith("-")) {
				map.put(args[i].substring(1), args[i + 1]);
				i++;
			}
		}
		return map;
	}
}
