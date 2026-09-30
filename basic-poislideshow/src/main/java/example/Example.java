package example;

import java.io.FileInputStream;
import java.io.IOException;
import java.util.List;

import org.apache.poi.xslf.usermodel.XMLSlideShow;
import org.apache.poi.xslf.usermodel.XSLFNotes;
import org.apache.poi.xslf.usermodel.XSLFShape;
import org.apache.poi.xslf.usermodel.XSLFSlide;
import org.apache.poi.xslf.usermodel.XSLFTextParagraph;
import org.apache.poi.xslf.usermodel.XSLFTextShape;
import java.io.FileInputStream;

import org.apache.poi.xslf.usermodel.XMLSlideShow;
import org.apache.poi.xslf.usermodel.XSLFShape;
import org.apache.poi.xslf.usermodel.XSLFSlide;
import org.apache.poi.xslf.usermodel.XSLFTextShape;

public class Example {

    public static void main(String[] args) throws Exception {

        if (args.length != 1) {
            System.err.println("Usage: java PptxTextExtractor <presentation.pptx>");
            System.exit(1);
        }

        String filename = args[0];

        try (FileInputStream fis = new FileInputStream(filename)) {

            XMLSlideShow ppt = new XMLSlideShow(fis);

            int slideNumber = 1;

            for (XSLFSlide slide : ppt.getSlides()) {

                System.out.println();
                System.out.println("===== SLIDE " + slideNumber + " =====");

                for (XSLFShape shape : slide.getShapes()) {

                    if (shape instanceof XSLFTextShape) {

                        XSLFTextShape textShape =
                                (XSLFTextShape) shape;

                        String text = textShape.getText();

                        if (text != null && !text.trim().isEmpty()) {
                            System.out.println(text.trim());
                        }
                    }
                }

                slideNumber++;
            }

            ppt.close();
        }
    }
}
