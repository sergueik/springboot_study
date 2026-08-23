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

		private static string outputfile = "console.png";
		private static string textfile = null;
		private static string screenfile = null;
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

			if (parseArgs.GetMacro("outputfile") != String.Empty)
				outputfile = parseArgs.GetMacro("outputfile");

			if (parseArgs.GetMacro("font") != String.Empty)
				fontPath = parseArgs.GetMacro("font");

			if (parseArgs.GetMacro("version") != String.Empty) {
				var versionObj = Assembly.GetExecutingAssembly().GetName().Version;
				Console.Error.WriteLine("version: " + versionObj.ToString());
				Environment.Exit(0);
				// https://stackoverflow.com/questions/12977924/how-do-i-properly-exit-a-c-sharp-application 
				// Application.Exit();
			}
			if (parseArgs.GetMacro("screenfile") != String.Empty)
				screenfile = parseArgs.GetMacro("screenfile");
			if (parseArgs.GetMacro("textfile") != String.Empty)
				textfile = parseArgs.GetMacro("textfile");
			if (parseArgs.GetMacro("foreground") != String.Empty)
				foreground = parseArgs.GetMacro("foreground");

			if (parseArgs.GetMacro("antialias") != String.Empty)
				antialias = true;
				// antialias = Boolean.Parse(parseArgs.GetMacro("antialias"));

			var screenlines = new List<string>(File.ReadAllLines(screenfile));
			
			Application.Initialize(ToolkitType.Gtk);

			string[] screenLines = File.ReadAllLines("example.txt");

			// using (var font = Font.FromName("3270", FontSize))
			var	font = Xwt.Drawing.Font.SystemMonospaceFont.WithSize(FontSize);

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

					for (int row = 0; row < screenLines.Length; row++) {
						string line = screenLines[row];

						for (int col = 0; col < line.Length; col++) {
							textLayout.Text = line[col].ToString();
							double x = Left + col * cellWidth;
							double y = Top + row * cellHeight;
							context.DrawTextLayout(textLayout, x, y);
						}
					}

					Image image = builder.ToBitmap();
					image.Save(outputfile, ImageFileType.Png);
				}
			}
		}
	}
}
