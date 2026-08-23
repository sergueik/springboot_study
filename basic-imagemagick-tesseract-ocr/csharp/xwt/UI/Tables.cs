using System;
using Xwt;
using Xwt.Drawing;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using NUnit.Framework;
// https://github.com/mono/xwt/tree/master/TestApps/Samples
namespace UI
{

	public class Tables : VBox
	{
		public Tables()
		{
			Table t = new Table();

			SimpleBox b = new SimpleBox(200, 20);
			t.Add(b, 0, 0);

			b = new SimpleBox(5, 20);
			t.Add(b, 1, 0);

			b = new SimpleBox(250, 20);
			t.Add(b, 0, 1, colspan: 2, hexpand: true, vexpand: true);

			b = new SimpleBox(300, 20);
			t.Add(b, 1, 2, colspan: 2);

			b = new SimpleBox(100, 20);
			t.Add(b, 2, 3);

			b = new SimpleBox(450, 20);
			t.Add(b, 0, 4, colspan: 3);

			PackStart(t);

			HBox box = new HBox();
			PackStart(box);
			t = new Table();
			t.Add(new Label("One:"), 0, 0);
			t.Add(new TextEntry(), 1, 0);
			t.Add(new Label("Two:"), 0, 1);
			t.Add(new TextEntry(), 1, 1);
			t.Add(new Label("Three:"), 0, 2);
			t.Add(new TextEntry(), 1, 2);
			t.InsertRow(1, 2);
			t.Add(new Label("One-and-a-half"), 0, 1);
			t.Add(new TextEntry() { PlaceholderText = "Just inserted" }, 1, 1);
			t.InsertRow(1, 2);
			t.Add(new SimpleBox(300, 20), 0, 1, colspan: 2);
			box.PackStart(t);
		}
	}


}
