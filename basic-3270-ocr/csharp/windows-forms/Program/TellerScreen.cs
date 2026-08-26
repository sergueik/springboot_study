using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System;
using System.IO;
using System.Drawing;
using System.Drawing.Text;
using System.Drawing.Imaging;
using System.Windows.Forms;
using System.Reflection;
using Utils;

namespace Program {
	public partial class TellerScreen {

		private static string outputFile = "console.png";
		private static string textFile = null;
		private static string screenFile = null;
		private static string foreground = null;
		private static bool debug = false;
		public static bool Debug { set { debug = value; } }
		private static bool antialias = false;
		private static string fontPath = null;
		private static string backgroundPath = null;
		private static int rows = 24;
		private static int cols = 80;
		private static int width = 0;
		private static int height = 0;
		private static int clientTop = 0;
		private static int clientLeft = 0;
		static readonly int left = 30;
		static readonly int top = 30;
		static readonly Dictionary<string, Color> ColorAliases =
			new Dictionary<string, Color>(StringComparer.OrdinalIgnoreCase) {
				{ "lime", Color.Lime },
				{ "brown", Color.Brown }
			};
		[STAThread]
		public static void Main() {

			var parseArgs = new ParseArgs(System.Environment.CommandLine);
			// NOTE: have to set debug with value true, switch arguments are not supported

			if (parseArgs.GetMacro("debug") != String.Empty)
				debug = true;
			// debug = Boolean.Parse(parseArgs.GetMacro("debug"));

			if (parseArgs.GetMacro("outputfile") != String.Empty)
				outputFile = parseArgs.GetMacro("outputfile");

			if (parseArgs.GetMacro("font") != String.Empty)
				fontPath = parseArgs.GetMacro("font");

			if (parseArgs.GetMacro("background") != String.Empty)
				backgroundPath = parseArgs.GetMacro("background");

			if (parseArgs.GetMacro("version") != String.Empty) {
				Console.Error.WriteLine("version: " + Assembly.GetExecutingAssembly().GetName().Version.ToString());
				Environment.Exit(0);
				// https://stackoverflow.com/questions/12977924/how-do-i-properly-exit-a-c-sharp-application
				// Application.Exit();
			}
			if ((parseArgs.GetMacro("screenfile") == String.Empty) || false) {
				Console.Error.WriteLine("Usage: " + Assembly.GetExecutingAssembly().GetName().Name + " -screenfile=<filename> [-outputFile=<filename>] [-font=<font>] [-antialias] [-debug]");
				Environment.Exit(0);
			}
			if (parseArgs.GetMacro("top") != String.Empty)
				clientTop = Convert.ToInt32(parseArgs.GetMacro("top"));

			if (parseArgs.GetMacro("left") != String.Empty)
				clientLeft = Convert.ToInt32(parseArgs.GetMacro("left"));

			if (parseArgs.GetMacro("screenfile") != String.Empty)
				screenFile = parseArgs.GetMacro("screenfile");
			if (parseArgs.GetMacro("textfile") != String.Empty)
				textFile = parseArgs.GetMacro("textfile");
			if (parseArgs.GetMacro("foreground") != String.Empty)
				foreground = parseArgs.GetMacro("foreground");

			if (parseArgs.GetMacro("antialias") != String.Empty)
				antialias = true;
			// antialias = Boolean.Parse(parseArgs.GetMacro("antialias"));

			var screenLines = new List<string>(File.ReadAllLines(screenFile));

			if (fontPath == null) {
				string basePath = Environment.GetEnvironmentVariable("USERPROFILE");
				string folder = "Downloads";
				string filename = "3270NerdFontMono-Regular.ttf";
				fontPath = (Environment.GetEnvironmentVariables().Contains("FONT_PATH")) ?
					Environment.GetEnvironmentVariable("FONT_PATH") : (Environment.GetEnvironmentVariables().Contains("WINDIR")) ?
					Path.Combine(new string[] { basePath, folder, filename })
					: "/usr/share/fonts/opentype/3270/3270-Regular.otf";
			}
			if (debug)
				Console.WriteLine(String.Format("Using font \"{0}\"", fontPath));
			var privateFontCollection = new PrivateFontCollection();
			Font font = null;
			try {
				if (File.Exists(fontPath) == false)
					throw new FileNotFoundException("Font file does not exist: " + fontPath, fontPath);
				privateFontCollection.AddFontFile(fontPath);
				font = new Font(privateFontCollection.Families[0], 24, FontStyle.Regular, GraphicsUnit.Pixel);
			} catch (Exception e) {
				if (debug)
					Console.WriteLine(String.Format("Exception loading \"{0}\":{1}", fontPath, e.ToString()));
				System.Diagnostics.Debug.WriteLine("Exception :" + e.ToString());
				font = new Font(FontFamily.GenericMonospace, 24);
			}
			Size textSize = TextRenderer.MeasureText("M", font);
			int cellWidth = textSize.Width;

			int cellHeight = textSize.Height;

			width = left * 2 + cols * cellWidth;
			height = top * 2 + rows * cellHeight;

			// https://learn.microsoft.com/en-us/dotnet/api/system.drawing.graphics.fromimage?view=netframework-4.5
			Graphics graphics = null;
			Bitmap bitmap = null;
			try {

				// https://learn.microsoft.com/en-us/dotnet/api/system.drawing.graphics.fromimage?view=netframework-4.5
				bitmap = (backgroundPath == null) ? new Bitmap(width, height) : new Bitmap(backgroundPath);

				if (debug)
					Console.WriteLine("Using: " + ((backgroundPath == null) ? "default background" : "custom background: " + backgroundPath));
				graphics = Graphics.FromImage(bitmap);
				if (backgroundPath == null) {
					graphics.Clear(Color.Black);
					clientTop = 0;
				} else {
					if (clientTop == 0)
						clientTop = 250;
				}

			} catch (ArgumentException e) {
				// Unhandled Exception: System.ArgumentException: Parameter is not valid
				// if width, height is zero
				throw e;
			}


			// https://learn.microsoft.com/en-us/dotnet/api/system.windows.forms.textrenderer?view=netframework-4.5
			// https://learn.microsoft.com/en-us/dotnet/api/system.drawing.text.textrenderinghint?view=netframework-4.5
			// graphics.TextRenderingHint = TextRenderingHint.SingleBitPerPixelGridFit;
			// graphics.TextRenderingHint = TextRenderingHint.AntiAlias;
			graphics.TextRenderingHint = antialias ? TextRenderingHint.AntiAliasGridFit : TextRenderingHint.SingleBitPerPixelGridFit;

			// foreground
			var brush = new SolidBrush((foreground != null && ColorAliases.ContainsKey(foreground)) ? ColorAliases[foreground] : Color.White);

			for (int row = 0; row < screenLines.Count; row++) {
				string line = screenLines[row];

				for (int col = 0; col < line.Length; col++) {
					var letter = line[col].ToString();

					float x = left + col * cellWidth;
					float y = top + row * cellHeight + clientTop ;

					// Future:
					// x += jitterX;
					// y += jitterY;

					graphics.DrawString(letter, font, brush, x, y);
				}
			}
			// https://learn.microsoft.com/en-us/dotnet/api/system.drawing.bitmap?view=netframework-4.5
			// https://learn.microsoft.com/en-us/dotnet/api/system.drawing.imaging.imageformat?view=netframework-4.5
			bitmap.Save(outputFile, ImageFormat.Png);
			Console.WriteLine(String.Format("Wrote \"{0}\"", outputFile));
		}
	}
}
