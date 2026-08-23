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

    class SimpleBox : Canvas
    {
        Size coreSize;
        double margin = 1;
        bool highlight;

        public Color Color { get; set; }

        public SimpleBox(double coreSize)
        {
            Color = new Color(0.5, 0.5, 1);
            this.coreSize = new Size(coreSize, coreSize);
            MinWidth = MinHeight = coreSize + margin * 2;
        }

        public SimpleBox(double coreWidth, double coreHeight)
        {
            Color = new Color(0.5, 0.5, 1);
            this.coreSize = new Size(coreWidth, coreHeight);
            MinWidth = coreSize.Width + margin * 2;
            MinHeight = coreSize.Height + margin * 2;
        }

        protected override void OnMouseEntered(EventArgs args)
        {
            base.OnMouseEntered(args);
            highlight = true;
            QueueDraw();
        }

        protected override void OnMouseExited(EventArgs args)
        {
            base.OnMouseExited(args);
            QueueDraw();
            highlight = false;
        }

        protected override void OnDraw(Context ctx, Rectangle dirtyRect)
        {
            ctx.SetColor(new Color(0.5, 0.5, 0.5));
            ctx.Rectangle(Bounds);
            ctx.Fill();
            ctx.SetColor(new Color(0.8, 0.8, 0.8));
            ctx.Rectangle(Bounds.Inflate(-margin, -margin));
            ctx.Fill();
            ctx.SetColor(highlight ? Color.BlendWith(Xwt.Drawing.Colors.White, 0.5) : Color);
            ctx.Rectangle(Bounds.Width / 2 - coreSize.Width / 2, Bounds.Height / 2 - coreSize.Height / 2, coreSize.Width, coreSize.Height);
            ctx.Fill();
        }
    }


}
