using System;
using Xwt;
using Xwt.Drawing;
using System.Xml;
// https://github.com/mono/xwt/tree/master/TestApps/Samples
namespace UI
{
	public class MainWindow : Window
	{
		TreeView samplesTree;
		TreeStore store;
		Image icon;
		VBox sampleBox;
		Label title;
		Widget currentSample;

		DataField<string> nameCol = new DataField<string>();
		DataField<Sample> widgetCol = new DataField<Sample>();
		DataField<Image> iconCol = new DataField<Image>();

		StatusIcon statusIcon;

		public MainWindow()
		{
			Title = "Xwt Demo Application";
			Width = 500;
			Height = 400;

			try
			{
				statusIcon = Application.CreateStatusIcon();
				statusIcon.Menu = new Menu();
				statusIcon.Menu.Items.Add(new MenuItem("Test"));
				statusIcon.Image = Image.FromResource(GetType(), "package.png");
			}
			catch
			{
				Console.WriteLine("Status icon could not be shown");
			}

			Menu menu = new Menu();

			var file = new MenuItem("_File");
			file.SubMenu = new Menu();
			file.SubMenu.Items.Add(new MenuItem("_Open"));
			file.SubMenu.Items.Add(new MenuItem("_New"));
			MenuItem mi = new MenuItem("_Close");
			mi.Clicked += delegate
			{
				Application.Exit();
			};
			file.SubMenu.Items.Add(mi);
			menu.Items.Add(file);

			var edit = new MenuItem("_Edit");
			edit.SubMenu = new Menu();
			edit.SubMenu.Items.Add(new MenuItem("_Copy"));
			edit.SubMenu.Items.Add(new MenuItem("Cu_t"));
			edit.SubMenu.Items.Add(new MenuItem("_Paste"));
			menu.Items.Add(edit);

			MainMenu = menu;


			HPaned box = new HPaned();

			icon = Image.FromResource(typeof(App), "document-generic.png");

			store = new TreeStore(nameCol, iconCol, widgetCol);
			samplesTree = new TreeView();
			samplesTree.Columns.Add("Name", iconCol, nameCol);

			var w = AddSample(null, "Widgets", null);

			samplesTree.DataSource = store;

			box.Panel1.Content = samplesTree;

			sampleBox = new VBox();
			title = new Label("Sample:");
			sampleBox.PackStart(title);

			box.Panel2.Content = sampleBox;
			box.Panel2.Resize = true;
			box.Position = 160;

			Content = box;

			samplesTree.SelectionChanged += HandleSamplesTreeSelectionChanged;

			CloseRequested += HandleCloseRequested;
		}

		void HandleCloseRequested(object sender, CloseRequestedEventArgs args)
		{
			args.AllowClose = MessageDialog.Confirm("Samples will be closed", Command.Ok);
			if (args.AllowClose)
				Application.Exit();
		}

		protected override void Dispose(bool disposing)
		{
			base.Dispose(disposing);

			if (statusIcon != null)
			{
				statusIcon.Dispose();
			}
		}

		void HandleSamplesTreeSelectionChanged(object sender, EventArgs e)
		{
			if (samplesTree.SelectedRow != null)
			{
				if (currentSample != null)
					sampleBox.Remove(currentSample);
				Sample s = store.GetNavigatorAt(samplesTree.SelectedRow).GetValue(widgetCol);
				if (s.Type != null)
				{
					if (s.Widget == null)
						s.Widget = (Widget)Activator.CreateInstance(s.Type);
					sampleBox.PackStart(s.Widget, true);
				}

				//	Console.WriteLine (System.Xaml.XamlServices.Save (s.Widget));
				currentSample = s.Widget;
				Dump(currentSample, 0);
			}
		}

		void Dump(IWidgetSurface w, int ind)
		{
			if (w == null)
				return;
			var s = w.GetPreferredSize();
			Console.WriteLine(new string(' ', ind * 2) + " " + w.GetType().Name + " " + s.Width + " " + s.Height);
			foreach (var c in w.Children)
				Dump(c, ind + 1);
		}

		TreePosition AddSample(TreePosition pos, string name, Type sampleType)
		{
			//if (page != null)
			//	page.Margin.SetAll (5);
			return store.AddNode(pos).SetValue(nameCol, name).SetValue(iconCol, icon).SetValue(widgetCol, new Sample(sampleType)).CurrentPosition;
		}
	}

	class Sample
	{
		public Sample(Type type)
		{
			Type = type;
		}

		public Type Type;
		public Widget Widget;
	}
}

