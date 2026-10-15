import { useEffect, useState } from "react";
import mermaid from "mermaid";

import pdfkit from "pdfkit";
import umd from "umd";
import blobStream from "blob-stream";
import blobStreamBrowserify from "blob-stream-browserify";
import * as svgExport from "svg-exportjs-browser";
import svgPdfkit from "svg-to-pdfkit";

export default function App() {

  const [diagram, setDiagram] = useState(`
graph TD
 A[Browser] --> B[React]
 B --> C[Mermaid]
 C --> D[SVG]
`);

  const [svg, setSvg] = useState("");

  useEffect(() => {

    mermaid.initialize({
      startOnLoad: false
    });

    mermaid.render(
      "diagram",
      diagram
    ).then(result => {
      setSvg(result.svg);
    });

  }, [diagram]);


  return (
    <>
      <textarea
        rows="10"
        cols="50"
        value={diagram}
        onChange={
          e => setDiagram(e.target.value)
        }
      />

      <div
        dangerouslySetInnerHTML={{
          __html: svg
        }}
      />
    </>
  );
}
