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
    class MyTestWidget : VBox
    {
        public MyTestWidget()
        {
            PackStart(new TextEntry() { PlaceholderText = "Placeholder Test" });
            PackStart(new Label("Scrollable Test:"));

            VBox ContentData = new VBox()
            {
                ExpandHorizontal = true,
                ExpandVertical = true
            };

            ScrollView ContentScroll = new ScrollView()
            {
                Content = ContentData,
                ExpandHorizontal = true,
                ExpandVertical = true
            };
            PackStart(ContentScroll, true, true);

            ContentData.PackStart(new TextEntry() { PlaceholderText = "Placeholder Test" }, true, true);
            ContentData.PackStart(new TextEntry(), true, true);
            ContentData.PackStart(new TextEntry() { PlaceholderText = "Placeholder Test" }, true, true);
            ContentData.PackStart(new TextEntry(), true, true);
            ContentData.PackStart(new TextEntry() { PlaceholderText = "Placeholder Test" }, true, true);
            ContentData.PackStart(new MyWidget(), true, true);
            ContentData.PackStart(new TextEntry() { PlaceholderText = "Placeholder Test" }, true, true);
            ContentData.PackStart(new TextEntry(), true, true);
            ContentData.PackStart(new TextEntry() { PlaceholderText = "Placeholder Test" }, true, true);
            ContentData.PackStart(new TextEntry(), true, true);
            ContentData.PackStart(new TextEntry() { PlaceholderText = "Placeholder Test" }, true, true);
            ContentData.PackStart(new TextEntry(), true, true);
        }
    }

    class MyWidget : Canvas
    {
        public MyWidget()
        {
            MinWidth = 210;
            MinHeight = 110;
        }

        protected override void OnDraw(Context ctx, Rectangle dirtyRect)
        {
            ctx.SetLineWidth(5);
            ctx.SetColor(new Color(1.0f, 0f, 0.5f));
            ctx.Rectangle(5, 5, 200, 100);
            ctx.FillPreserve();
            ctx.SetColor(new Color(0f, 0f, 1f));
            ctx.Stroke();
        }
    }

}
