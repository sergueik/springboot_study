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

    public class ListBoxSample : VBox
    {
        DataField<string> name = new DataField<string>();
        DataField<Image> icon = new DataField<Image>();

        public ListBoxSample()
        {
            // Default list box

            ListBox list = new ListBox();

            for (int n = 0; n < 100; n++)
                list.Items.Add("Value " + n);


            list.KeyPressed += (sender, e) =>
            {
                if (e.Key == Key.Insert)
                {
                    int r = list.SelectedRow + 1;
                    list.Items.Insert(r, "Value " + list.Items.Count + 1);
                    list.ScrollToRow(r);
                    list.SelectRow(r);
                    list.FocusedRow = r;
                }
            };

            PackStart(list, true);

            // Custom list box

            ListBox customList = new ListBox();
            customList.GridLinesVisible = true;
            ListStore store = new ListStore(name, icon);
            customList.DataSource = store;
            customList.Views.Add(new ImageCellView(icon));
            customList.Views.Add(new TextCellView(name));

            var png = Image.FromResource(typeof(Application), "class.png");

            for (int n = 0; n < 100; n++)
            {
                var r = store.AddRow();
                store.SetValue(r, icon, png);
                store.SetValue(r, name, "Value " + n);
            }

            customList.KeyPressed += (sender, e) =>
            {
                if (e.Key == Key.Insert)
                {
                    var r = store.InsertRowAfter(customList.SelectedRow < 0 ? 0 : customList.SelectedRow);
                    store.SetValue(r, icon, png);
                    store.SetValue(r, name, "Value " + (store.RowCount + 1));
                    customList.ScrollToRow(r);
                    customList.SelectRow(r);
                    customList.FocusedRow = r;
                }
            };

            PackStart(customList, true);

            var spnValue = new SpinButton();
            spnValue.MinimumValue = 0;
            spnValue.MaximumValue = 99;
            spnValue.IncrementValue = 1;
            spnValue.Digits = 0;
            var btnScroll = new Button("Go!");
            btnScroll.Clicked += (sender, e) => customList.ScrollToRow((int)spnValue.Value);

            HBox scrollActBox = new HBox();
            scrollActBox.PackStart(new Label("Scroll to Value: "));
            scrollActBox.PackStart(spnValue);
            scrollActBox.PackStart(btnScroll);
            PackStart(scrollActBox);
        }
    }

}
