package example;

import javax.imageio.ImageIO;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.File;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.Path;
import java.util.List;

public class Runner {

	static final int COLS = 80;
	static final int ROWS = 24;

	static final int FONT_SIZE = 24;

	static final int LEFT = 30;
	static final int TOP = 30;

	public static void main(String[] args) throws Exception {

		List<String> screen = List.of("                         LORE-MF",
				"                                                                ",
				"LOREM:  ___________                         IPSUM: _____________",
				"DOLOR:  __________                 AMET:  _____________",
				"AMET ====> _____________                 CONSECTETUR: _________",
				"ADIPISCING: __________      ELIT: __________",
				"SED: _________                              DO: _______________",
				"ENIM: _____________                  AD: __________",
				"MINIM: _____________       VENIAM: _____________",
				"QUIS: _____________                 NOSTRUD: ___________",
				"COMMODO: _____________       CONSEQUAT: __________", "",
				"PF1=HELP  PF2=SPLIT  PF3=END  PF4=RETURN  PF5=RFIND  PF6=RCHANGE",
				"PF7=UP    PF8=DOWN   PF9=SWAP  PF10=LEFT  PF11=RIGHT  PF12=RETRIEVE");

		// To achieve a lookalike screenshot of Blue Prism IBM 3270 mainframe/CICS
		// terminal emulator
		// one may use the open-source TrueType font
		// download
		// https://github.com/rbanffy/3270font

		// Fallback for a standard monospace font
		Font font = new Font(Font.MONOSPACED, Font.PLAIN, FONT_SIZE);

		String fontPath = System.getenv().containsKey("FONT_PATH") ? System.getenv("FONT_PATH")
				: getOSName().equals("windows")
						? Paths.get(System.getProperty("user.home")).resolve("Downloads")
								.resolve("3270NerdFontMono-Regular.ttf").toAbsolutePath().toString()
						: "/usr/share/fonts/opentype/3270/3270-Regular.otf";

		font = Font.createFont(Font.TRUETYPE_FONT, new File(fontPath)).deriveFont((float) FONT_SIZE);

		// String name = "3270 Nerd Font Mono";
		font = Font.createFont(Font.TRUETYPE_FONT, new File(fontPath)).deriveFont((float) FONT_SIZE);

		// create a temporary image to compute FontMetrics.
		BufferedImage metricsImage = new BufferedImage(1, 1, BufferedImage.TYPE_INT_RGB);

		Graphics2D mg = metricsImage.createGraphics();
		mg.setFont(font);
		FontMetrics fm = mg.getFontMetrics();

		int cellWidth = fm.charWidth('M');
		int cellHeight = fm.getHeight();

		mg.dispose();

		int width = LEFT * 2 + COLS * cellWidth;
		int height = TOP * 2 + ROWS * cellHeight;

		BufferedImage image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);

		Graphics2D g = image.createGraphics();

		// ---- Terminal appearance -------------------------------------

		g.setColor(Color.BLACK);
		g.fillRect(0, 0, width, height);

		g.setFont(font);
		g.setColor(Color.GREEN);

		// Slightly nicer text rendering for the initial clean version.
		g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON);

		// ===============================================================
		// MODE 1: LINE-GRANULAR
		// ===============================================================

		boolean characterGranular = true;

		if (!characterGranular) {

			for (int row = 0; row < screen.size(); row++) {

				String line = screen.get(row);

				int x = LEFT;
				int y = TOP + fm.getAscent() + row * cellHeight;

				g.drawString(line, x, y);
			}
		}

		// ===============================================================
		// MODE 2: CHARACTER-GRANULAR
		// ===============================================================

		if (characterGranular) {

			for (int row = 0; row < screen.size(); row++) {

				String line = screen.get(row);

				for (int col = 0; col < line.length(); col++) {

					char ch = line.charAt(col);

					int x = LEFT + col * cellWidth;
					int y = TOP + fm.getAscent() + row * cellHeight;

					/*
					 * Future deliberate imperfection hooks:
					 *
					 * x += randomOffset(-1, 1); y += randomOffset(-1, 1);
					 *
					 * change brightness per character omit a pixel blur selected cells slightly
					 * alter spacing
					 */

					g.drawString(String.valueOf(ch), x, y);
				}
			}
		}

		g.dispose();

		ImageIO.write(image, "png", new File("console.png"));

		System.out.println("Wrote console.png");
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

}
