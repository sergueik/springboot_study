package org.rrd4j.graph;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

/**
 * Class to represent successfully created Rrd4j graph. Objects of this class are created by method
 * {@link org.rrd4j.graph.RrdGraph#getRrdGraphInfo()}.
 */
public class RrdGraphInfo {
    String filename;
    int width, height;
    InputStream stream;
    String imgInfo;
    private List<String> printLines = new ArrayList<String>();

    RrdGraphInfo() {
        // cannot instantiate this class
    }

    void addPrintLine(String printLine) {
        printLines.add(printLine);
    }

    /**
     * Returns filename of the graph
     *
     * @return filename of the graph. '-' denotes in-memory graph (no file created)
     */
    public String getFilename() {
        return filename;
    }

    /**
     * Returns total graph width
     *
     * @return total graph width
     */
    public int getWidth() {
        return width;
    }

    /**
     * Returns total graph height
     *
     * @return total graph height
     */
    public int getHeight() {
        return height;
    }

    /**
     * Returns graph bytes
     *
     * @return Graph bytes
     * @throws IllegalStateException if the images bytes are unavailable or can't be read
     */
    public byte[] getBytes() {
        try {
            byte[] content = new byte[stream.available()];
            int read = stream.read(content);
            if (read != content.length) {
                throw new IllegalStateException("Unable to read image buffer");
            }
            return content;
        } catch (IOException e) {
            throw new IllegalStateException("Unable to read image bytes", e);
        }
    }

    /**
     * Returns PRINT lines requested by {@link org.rrd4j.graph.RrdGraphDef#print(String, org.rrd4j.ConsolFun, String)} method.
     *
     * @return An array of formatted PRINT lines
     */
    public String[] getPrintLines() {
        return printLines.toArray(new String[printLines.size()]);
    }

    /**
     * Returns image information requested by {@link org.rrd4j.graph.RrdGraphDef#setImageInfo(String)} method
     *
     * @return Image information
     */
    public String getImgInfo() {
        return imgInfo;
    }

    /**
     * Returns the number of bytes in the graph file
     *
     * @return Length of the graph file
     * @throws IllegalStateException if the images bytes are unavailable
     */
    public int getByteCount() {
        try {
            return stream.available();
        } catch (IOException e) {
            throw new IllegalStateException("Unable to read image bytes", e);
        }
    }

    /**
     * Dumps complete graph information. Useful for debugging purposes.
     *
     * @return String containing complete graph information
     */
    public String dump() {
        StringBuilder b = new StringBuilder();
        b.append("filename = \"").append(getFilename()).append("\"\n");
        b.append("width = ").append(getWidth()).append(", height = ").append(getHeight()).append("\n");
        b.append("byteCount = ").append(getByteCount()).append("\n");
        b.append("imginfo = \"").append(getImgInfo()).append("\"\n");
        String[] plines = getPrintLines();
        if (plines.length == 0) {
            b.append("No print lines found\n");
        }
        else {
            for (int i = 0; i < plines.length; i++) {
                b.append("print[").append(i).append("] = \"").append(plines[i]).append("\"\n");
            }
        }
        return b.toString();
    }
}
