import { useEffect, useState } from "react";
import mermaid from "mermaid";
import umd from "umd";
import blobStreamBrowserify from "blob-stream-browserify";
import * as svgExport from "svg-exportjs-browser";

import "./App.css";

function MermaidVersion() {
  return <span>v{__MERMAID_VERSION__}</span>;
}

let scale = 1;

window.zoomIn = function() {
    scale += 0.1;
    window.applyZoom();
};

window.zoomOut = function() {
    scale = Math.max(0.2, scale - 0.1);
    window.applyZoom();
};

window.applyZoom = function() {

    const svg = document.querySelector('svg#output');

    if (svg) {
        svg.style.transform = 'scale(' + scale + ')';
        svg.style.transformOrigin = '0 0';
    }
};

window.prepareSvgForExport = function(sourceSvg) {
  var data = sourceSvg.outerHTML;

  console.log( 'prepareSvgForExport original source length:', data.length);

  console.log( 'prepareSvgForExport original source fragment:', data.substring(0, 200));

  const svg = sourceSvg.cloneNode(true);

  data = svg.outerHTML;

  console.log( 'prepareSvgForExport clone source length:', data.length);

  console.log( 'prepareSvgForExport clone source fragment:', data.substring(0, 200));

  console.log( 'prepareSvgForExport clone source SVG metadata:');

  console.dir({
    width: svg.getAttribute('width'),
    height: svg.getAttribute('height'),
    viewBox: svg.getAttribute('viewBox'),
    xmlns: svg.getAttribute('xmlns')
  });

  const viewBox = svg.viewBox.baseVal;

  console.log( 'prepareSvgForExport clone viewBox:');
  console.dir( viewBox, { depth: null });
  console.log( 'prepareSvgForExport MERMAID SVG:');

  console.dir({
    viewBox: svg.getAttribute('viewBox'),
    width: svg.getAttribute('width'),
    height: svg.getAttribute('height'),
    clientWidth: svg.clientWidth,
    clientHeight: svg.clientHeight
  });

  if (viewBox && viewBox.width > 0 && viewBox.height > 0) {

      svg.setAttribute( 'width', viewBox.width);
      svg.setAttribute( 'height', viewBox.height);
      svg.style.width = viewBox.width + 'px';
      svg.style.height = viewBox.height + 'px';
      svg.style.maxWidth = 'none';
      svg.style.transform = 'none';
  }

  const sourceElements = sourceSvg.querySelectorAll('*');
  const targetElements = svg.querySelectorAll('*');

  const properties = [
      'fill',
      'fill-opacity',
      'stroke',
      'stroke-width',
      'stroke-opacity',
      'stroke-dasharray',
      'stroke-dashoffset',
      'stroke-linecap',
      'stroke-linejoin',
      'opacity',
      'font-family',
      'font-size',
      'font-weight',
      'font-style',
      'text-anchor',
      'dominant-baseline'
  ];

  for (let i = 0; i < sourceElements.length; i++) {
      const source = sourceElements[i];
      const target = targetElements[i];
      if (!target) {
        continue;
      }

      const computed = window.getComputedStyle(source);
      for (const property of properties) {
          const value = computed.getPropertyValue(property);
          if (value) {
              target.style.setProperty( property, value);
          }
      }
  }

  const styles =
      svg.querySelectorAll('style');

  for (const style of styles) {
      style.parentNode.removeChild(style);
  }

  const foreignObjects = svg.querySelectorAll('foreignObject');

  for (const foreignObject of foreignObjects) {

    const sourceForeignObject =
        sourceSvg.querySelectorAll('foreignObject')[
            Array.prototype.indexOf.call(
                foreignObjects,
                foreignObject
            )
        ];

    let textContent = '';

    if (sourceForeignObject) {

        textContent =
            sourceForeignObject.textContent.trim();

    } else {

        textContent =
            foreignObject.textContent.trim();
    }

    if (!textContent) {
        continue;
    }

    const parent =
        foreignObject.parentNode;

    const text =
        document.createElementNS(
            'http://www.w3.org/2000/svg',
            'text'
        );

    text.textContent =
        textContent;

    const computed =
        window.getComputedStyle(
            sourceForeignObject ||
            foreignObject
        );

    text.setAttribute(
        'font-family',
        computed.fontFamily || 'Arial'
    );

    text.setAttribute(
        'font-size',
        computed.fontSize || '16px'
    );

    text.setAttribute(
        'fill',
        computed.color || '#333'
    );

    text.setAttribute(
        'text-anchor',
        'middle'
    );

    const width =
        parseFloat(
            foreignObject.getAttribute('width')
        ) || 0;

    const height =
        parseFloat(
            foreignObject.getAttribute('height')
        ) || 19;

    text.setAttribute(
        'x',
        width / 2
    );

    text.setAttribute(
        'y',
        height * 0.75
    );

    parent.replaceChild(
        text,
        foreignObject
    );
  }

  return svg;
};

window.normalizeSvgViewBoxToIntegers = function(svg) {
  const viewBox = svg.viewBox.baseVal;
  const original = svg.getAttribute('viewBox');
  const x = Math.round(viewBox.x);
  const y = Math.round(viewBox.y);
  const width = Math.round(viewBox.width);
  const height = Math.round(viewBox.height);
  const normalized = x + ' ' + y + ' ' + width + ' ' + height;
  console.log( '  original:' + original +  '  normalized:' + normalized);
  svg.setAttribute( 'viewBox', normalized);
};

window.exportSvg = async function(sourceSvg) {
  sourceSvg = sourceSvg ||
      document.querySelector('svg#output');

  if (!sourceSvg) {
      console.warn(
          'No Mermaid SVG is currently rendered.'
      );
      return;
  }

  const svg = window.prepareSvgForExport(sourceSvg);
  window.normalizeSvgViewBoxToIntegers(svg);

  await svgExport.downloadSvg(
    svg,
    'Mermaid diagram'
  );
};

export default function App() {
  const [diagram, setDiagram] = useState(`
  swimlane-beta LR
  subgraph Customer
    Browse[Browse catalogue]
    Pay[Pay]
  end
  subgraph Warehouse
    Pick[Pick items]
    Ship[Ship order]
  end
  subgraph Finance
    Invoice[Raise invoice]
  end
  Browse --> Pay
  Pay --> Pick
  Pick --> Ship
  Pay --> Invoice
`);

  const [svg, setSvg] = useState("");
  const [rendered, setRendered] = useState(false);
	
  useEffect(() => {

    mermaid.initialize({
      startOnLoad: false
    });

    mermaid.render(
      "output",
      diagram
    ).then(result => {
      setSvg(result.svg);
      setRendered(true);
    });

  }, [diagram]);

  return (
    <div className="app-shell">

      <div className="titlebar">
        <span>Ultra Basic Mermaid Renderer</span>
        <span className="titlebar-meta">
          Mermaid React <MermaidVersion />
        </span>
      </div>

      <div className="split">

        <div className="pane-input">

          <div className="panel-label">
            Mermaid Input
          </div>

          <textarea
            className="diagram-input"
            value={diagram}
            spellCheck={false}
            onChange={e => setDiagram(e.target.value)}
          />

          <div className="toolbar">

            <button
              disabled={!rendered}
              onClick={() => window.zoomIn()}
            >
              +
            </button>

            <button
              disabled={!rendered}
              onClick={() => window.zoomOut()}
            >
              -
            </button>
            <button
              disabled={!rendered}
              onClick={() => window.exportSvg()}
            >
              Export SVG
            </button>
          </div>

        </div>

        <div className="pane-output">

          <div className="panel-label">
            Rendered Diagram
          </div>

          <div
            id="diagramOutput"
            className="diagram-output"
            dangerouslySetInnerHTML={{
              __html: svg
            }}
          />
        </div>
      </div>
    </div>
  );
}
