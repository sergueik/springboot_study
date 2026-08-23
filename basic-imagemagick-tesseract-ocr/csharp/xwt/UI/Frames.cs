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

    public class Frames : VBox
    {
    	
        public Frames()
        {
            Frame f = new Frame();
            f.Label = "Simple widget box";
            f.Content = new SimpleBox(50);
            	
            PackStart(f);

            f = new Frame();
            f.Content = new Label("No label");
            PackStart(f);

            var fb = new FrameBox();
            fb.BorderWidthLeft = 1;
            fb.BorderWidthTop = 2;
            fb.BorderWidthRight = 3;
            fb.BorderWidthBottom = 4;
            fb.BorderColor = new Color(0, 0, 1);
            fb.Content = new Label("Custom");
            PackStart(fb);

            fb = new FrameBox();
            fb.BorderWidth = 2;
            fb.PaddingLeft = 10;
            fb.PaddingTop = 20;
            fb.PaddingRight = 30;
            fb.PaddingBottom = 40;
            fb.Content = new SimpleBox(50);
            PackStart(fb);

            fb = new FrameBox();
            fb.BorderWidth = 2;
            fb.Padding = 10;
            fb.Content = new Label("With red background");
            fb.BackgroundColor = new Color(1, 0, 0);
            PackStart(fb);
        }
    }




}
