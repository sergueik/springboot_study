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

    public class NotebookSample : VBox
    {
        public NotebookSample()
        {
            Notebook nb = new Notebook();
            nb.Add(new ListView2(), "Browser Settings");
            nb.Add(new Tables(), "Locators");
            nb.Add(new TreeViews(), "Page Objects");
            nb.Add(new RadioButtonSample(), "Locators");
            nb.Add(new MyTestWidget(), "Source Code");
            nb.Add(new Frames(), "HTML Source");
            nb.Add(new ScrollWindowSample(), "ScrollWindow");
                        nb.Add(new TextEntries (), "Playground");
            
            nb.TabOrientation = NotebookTabOrientation.Top;
            PackStart(nb, true);
        }
    }

}
