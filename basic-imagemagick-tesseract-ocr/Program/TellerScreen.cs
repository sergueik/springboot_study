using System.Collections.Generic;
using System.Collections.Specialized;
using System.Configuration;
using System.Diagnostics;
using System.ComponentModel;
using System.Linq;
using System;
using System.Drawing;
using System.Drawing.Text;
using System.Drawing.Imaging;
using System.Windows.Forms;
using System.Threading;
using System.Timers;


namespace Program {
	public partial class Form1 {

		private static int rows = 24;
		private static int cols = 80;	
		private static int width = 0;
		private static int height = 0;

		static readonly int left = 30;
		static readonly int top = 30;

		[STAThread]
		public static void Main()
		{
			string[] screenArray = { 
				"                                                                                ",
				"                    MOCK MAINFRAME LOGIN SCREEN                                 ",
				"                                                                                ",
				" USER ID  ===> __________                                                       ",
				" PASSWORD ===> __________                                                       ",
				"                                                                                ",
				"                                                                                ",
				"                                                                                ",
				"                                                                                ",
				" PF3=EXIT                                  ENTER=CONTINUE                       "

			};
			List<String> screen = new List<String>(screenArray);
  

			var fonts = new PrivateFontCollection();
			fonts.AddFontFile(String.Format("{0}\\Downloads\\3270NerdFontMono-Regular.ttf", Environment.GetEnvironmentVariable("USERPROFILE")));

			var font = new Font(fonts.Families[0], 24, FontStyle.Regular, GraphicsUnit.Pixel);

			Size textSize = TextRenderer.MeasureText("M", font);
			int cellWidth = textSize.Width;

			int cellHeight = textSize.Height;

			width = left * 2 + cols * cellWidth;
			height = top * 2 + rows * cellHeight;
			
			var bitmap = new Bitmap(width, height);
			var g = Graphics.FromImage(bitmap);

			g.Clear(Color.Black);

			g.TextRenderingHint = System.Drawing.Text.TextRenderingHint.SingleBitPerPixelGridFit;

			var brush = new SolidBrush(Color.Lime);

			g.DrawString("USER ID  ===> __________", font, brush, 30, 30);

			for (int row = 0; row < screen.Count; row++) {
				string line = screen[row];

				for (int col = 0; col < line.Length; col++) {
					char ch = line[col];
	
					float x = left + col * cellWidth;
					float y = top + row * cellHeight;

					// Future:
					// x += jitterX;
					// y += jitterY;

					g.DrawString(ch.ToString(), font, brush, x, y);
				}
			}
			// https://learn.microsoft.com/en-us/dotnet/api/system.drawing.bitmap?view=netframework-4.5
			// https://learn.microsoft.com/en-us/dotnet/api/system.drawing.imaging.imageformat?view=netframework-4.5
			bitmap.Save("console.png", ImageFormat.Png);
		}
	}
}