using System;
using Xwt;
using Xwt.Drawing;

// https://github.com/mono/xwt/tree/master/TestApps/Samples
namespace UI
{
	public class Checkboxes: VBox
	{
		public Checkboxes ()
		{
			var a = new CheckBox ("Normal checkbox");
			var b = new CheckBox ("Disabled") { Sensitive = false };
			var c = new CheckBox ("Allows mixed (with red background)") { AllowMixed = true };
			c.BackgroundColor = Xwt.Drawing.Colors.Red;

			a.Toggled += (sender, e) => b.Sensitive = a.Active;

			PackStart (a);
			PackStart (b);

			PackStart (new CheckBox ("Mixed to start") { AllowMixed = true, State = CheckBoxState.Mixed });
			PackStart (c);
			
			int clicks = 0, toggles = 0;
			Label la = new Label ();
			PackStart (la);
			
			c.Clicked += delegate {
				clicks++;
				la.Text = string.Format ("state:{0}, clicks:{1}, toggles:{2}", c.State, clicks, toggles);
			};
			
			c.Toggled += delegate {
				toggles++;
				la.Text = string.Format ("state:{0}, clicks:{1}, toggles:{2}", c.State, clicks, toggles);
			};
		}
	}
}