using System;
using System.Collections;
using Xwt;
using Xwt.Drawing;

using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.IO;

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

		private static int rows = 24;
		private static int cols = 80;
		private static int width = 0;
		private static int height = 0;

		static readonly int left = 30;
		static readonly int top = 30;

		const int Rows = 24;
		const int Cols = 80;

		const int Left = 30;
		const int Top = 30;

		const int FontSize = 24;

		[STAThread]
		public static void Main() {
			
			var parseArgs = new ParseArgs(System.Environment.CommandLine);

			// NOTE: have to set debug with value true, switch arguments are not supported
			if (parseArgs.GetMacro("debug") != String.Empty)
				debug = true;
			// debug = Boolean.Parse(parseArgs.GetMacro("debug"));

			if (parseArgs.GetMacro("version") != String.Empty) {
				Console.Error.WriteLine("version: " + Assembly.GetExecutingAssembly().GetName().Version.ToString());
				Environment.Exit(0);
				// https://stackoverflow.com/questions/12977924/how-do-i-properly-exit-a-c-sharp-application 
				// Application.Exit();
			}
			if ((parseArgs.GetMacro("screenfile") == String.Empty ) || false)  {
				Console.Error.WriteLine("Usage: " + Assembly.GetExecutingAssembly().GetName().Name + " -screenfile=<filename> [-outputfile=<filename>] [-font=<font>] [-antialias] [-debug]");
				Environment.Exit(0);
			}

			if (parseArgs.GetMacro("outputfile") != String.Empty)
				outputFile = parseArgs.GetMacro("outputfile");
			if (parseArgs.GetMacro("font") != String.Empty)
				fontPath = parseArgs.GetMacro("font");
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
			screenLines.Add(".net (GTK)");

			Application.Initialize(ToolkitType.Gtk);

			if (fontPath == null) {
				string basePath = Environment.GetEnvironmentVariable("USERPROFILE");
				string folder = "Downloads";
				string filename = "3270NerdFontMono-Regular.ttf";
				fontPath = (Environment.GetEnvironmentVariables().Contains("FONT_PATH")) ?
					Environment.GetEnvironmentVariable("FONT_PATH") : (Environment.GetEnvironmentVariables().Contains("WINDIR")) ? 
					Path.Combine(new string[] {basePath, folder, filename})
					: "/usr/share/fonts/opentype/3270/3270-Regular.otf";
			}
			Font font = null;
			try {
				// very old Xwt 0.2.251
				// The 'Xwt.Drawing.Font' does not contain a definition for 'FromFile' (CS0117)
				// font = Font.FromFile(fontPath, FontSize);
				// TODO: try
				font = Font.FromName("Courier New").WithSize( FontSize);
			} catch (Exception e ) {
				System.Diagnostics.Debug.WriteLine("Exception :" + e.ToString());
				font = Font.SystemMonospaceFont.WithSize(FontSize);
			}
			using (var textLayout = new TextLayout()) {
				textLayout.Font = font;
				textLayout.Text = "M";

				Size cellSize = textLayout.GetSize();

				int cellWidth = (int)Math.Ceiling(cellSize.Width);
				int cellHeight = (int)Math.Ceiling(cellSize.Height);

				int width = Left * 2 + Cols * cellWidth;
				int height = Top * 2 + Rows * cellHeight;

				using (var builder = new ImageBuilder(width, height))
				using (Context context = builder.Context) {
					// background
					context.SetColor(Colors.Black);
					context.Rectangle(0, 0, width, height);
					context.Fill();

					// text setup
					context.SetColor(Colors.Lime);

					textLayout.Font = font;

					for (int row = 0; row < screenLines.Count; row++) {
						string line = screenLines[row];

						for (int col = 0; col < line.Length; col++) {
							textLayout.Text = line[col].ToString();
							double x = Left + col * cellWidth;
							double y = Top + row * cellHeight;
							context.DrawTextLayout(textLayout, x, y);
						}
					}

					Image image = builder.ToBitmap();
					image.Save(outputFile, ImageFileType.Png);
					Console.WriteLine(String.Format("Wrote \"{0}\"", outputFile));

				}
			}
		}
	}
}
