using System;
using Xwt;
using Xwt.Drawing;

// https://github.com/mono/xwt/tree/master/TestApps/Samples

namespace UI
{

public class RadioButtonSample: VBox
	{
		public RadioButtonSample ()
		{
			var b1 = new RadioButton ("Item 1");
			var b2 = new RadioButton ("Item 2 (red background)");
			b2.BackgroundColor = Xwt.Drawing.Colors.Red;
			var b3 = new RadioButton ("Item 3");
			b2.Group = b3.Group = b1.Group;
			PackStart (b1);
			PackStart (b2);
			PackStart (b3);

			var la = new Label ();
			la.Hide ();
			b1.Group.ActiveRadioButtonChanged += delegate {
				la.Show ();
				la.Text = "Active: " + b1.Group.ActiveRadioButton.Label;
			};
			PackStart (la);

			PackStart (new HSeparator ());

			var box = new VBox ();
			box.PackStart (new Label ("First Option"));
			box.PackStart (new Label ("Second line"));

			var b4 = new RadioButton (box);
			var b5 = new RadioButton ("Second Option");
			var b6 = new RadioButton ("Disabled Option") { Sensitive = false };
			PackStart (b4);
			PackStart (b5);
			PackStart (b6);
			b4.Group = b5.Group = b6.Group;
		}
	}
}