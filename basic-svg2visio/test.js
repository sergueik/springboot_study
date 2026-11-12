// @ts-check
/**
 * Node.js test script for the SVG parser.
 * Tests parsing of sample SVGs to verify shape/connector extraction.
 */

const fs = require('fs');
const { load } = require('./tools/load.js');

// The sources are loaded exactly as index.html loads them
const { SceneModel, SceneLayout, SvgTransform, SvgStyleResolver,
        SvgParser, DrawioParser, VsdxBuilder, domParser } = load();

let passed = 0;
let failed = 0;

function assert(condition, msg) {
    if (condition) {
        console.log(`  PASS: ${msg}`);
        passed++;
    } else {
        console.log(`  FAIL: ${msg}`);
        failed++;
    }
}

// Test 1: Flowchart
console.log('\n--- Test 1: Flowchart SVG ---');
const flowchartSvg = fs.readFileSync('./test-samples/flowchart.svg', 'utf8');
const parser1 = new SvgParser(flowchartSvg);
const result1 = parser1.parse();
const stats1 = parser1.getStats();

assert(stats1.shapes >= 5, `Should have at least 5 shapes (got ${stats1.shapes})`);
assert(stats1.connectors >= 6, `Should have at least 6 connectors (got ${stats1.connectors})`);

// Check shape types
const shapeTypes1 = result1.shapes.map(s => s.type);
assert(shapeTypes1.includes('ellipse'), 'Should have ellipse shapes (start/end)');
assert(shapeTypes1.includes('rect'), 'Should have rect shapes (process boxes)');
assert(shapeTypes1.includes('diamond') || shapeTypes1.includes('polygon'),
    'Should have diamond/polygon shape (decision)');

// Check text association
const shapesWithText = result1.shapes.filter(s => s.text);
assert(shapesWithText.length >= 4, `At least 4 shapes should have text (got ${shapesWithText.length})`);

// Check arrow detection
const arrowConnectors = result1.connectors.filter(c => c.arrowEnd || c.arrowStart);
assert(arrowConnectors.length >= 6, `At least 6 connectors should have arrows (got ${arrowConnectors.length})`);

// Test 2: Architecture diagram
console.log('\n--- Test 2: Architecture SVG ---');
const archSvg = fs.readFileSync('./test-samples/architecture.svg', 'utf8');
const parser2 = new SvgParser(archSvg);
const result2 = parser2.parse();
const stats2 = parser2.getStats();

assert(stats2.shapes >= 10, `Should have at least 10 shapes (got ${stats2.shapes})`);
assert(stats2.connectors >= 5, `Should have at least 5 connectors (got ${stats2.connectors})`);

// Check that connectors reference shapes
const linkedConnectors = result2.connectors.filter(c => c.fromShape !== null || c.toShape !== null);
assert(linkedConnectors.length > 0, `Some connectors should be linked to shapes (got ${linkedConnectors.length})`);

// Test 3: Edge cases
console.log('\n--- Test 3: Edge cases ---');

// Minimal SVG
const minSvg = '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 100 100"><rect x="10" y="10" width="80" height="80" fill="blue"/></svg>';
const parser3 = new SvgParser(minSvg);
const result3 = parser3.parse();
assert(result3.shapes.length === 1, 'Minimal SVG: should have 1 shape');
assert(result3.shapes[0].style.fill === 'blue', 'Fill color should be blue');

// SVG with path connectors
const pathSvg = `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 400 200">
  <rect x="10" y="50" width="100" height="60" fill="#eee" stroke="#333"/>
  <rect x="250" y="50" width="100" height="60" fill="#eee" stroke="#333"/>
  <path d="M 110 80 C 150 80 200 80 250 80" stroke="#333" stroke-width="2" fill="none" marker-end="url(#a)"/>
</svg>`;
const parser4 = new SvgParser(pathSvg);
const result4 = parser4.parse();
assert(result4.shapes.length === 2, `Path SVG: should have 2 shapes (got ${result4.shapes.length})`);
assert(result4.connectors.length === 1, `Path SVG: should have 1 connector (got ${result4.connectors.length})`);

// Test 4: Draw.io flowchart
console.log('\n--- Test 4: Draw.io Flowchart ---');
const drawioXml = fs.readFileSync('./test-samples/flowchart.drawio', 'utf8');
const drawioParser = new DrawioParser(drawioXml);
const drawioResult = drawioParser.parse();
const drawioStats = drawioParser.getStats();

assert(drawioStats.shapes >= 6, `Should have at least 6 shapes (got ${drawioStats.shapes})`);
assert(drawioStats.connectors >= 5, `Should have at least 5 connectors (got ${drawioStats.connectors})`);

// Check shape types
const drawioTypes = drawioResult.shapes.map(s => s.type);
assert(drawioTypes.includes('ellipse'), 'Should have ellipse shapes (Start/End)');
assert(drawioTypes.includes('rect'), 'Should have rect shapes (process boxes)');
assert(drawioTypes.includes('diamond'), 'Should have diamond shape (decision)');

// Check text extraction
const drawioShapesWithText = drawioResult.shapes.filter(s => s.text);
assert(drawioShapesWithText.length >= 6, `All 6 shapes should have text (got ${drawioShapesWithText.length})`);

// Check connector linking
const drawioLinked = drawioResult.connectors.filter(c => c.fromShape !== null && c.toShape !== null);
assert(drawioLinked.length >= 5, `At least 5 connectors should be linked (got ${drawioLinked.length})`);

// Check arrows
const drawioArrows = drawioResult.connectors.filter(c => c.arrowEnd || c.arrowStart);
assert(drawioArrows.length >= 5, `At least 5 connectors should have arrows (got ${drawioArrows.length})`);

// Check edge labels become texts
const edgeLabels = drawioResult.texts.filter(t => t.text === 'Yes' || t.text === 'No');
assert(edgeLabels.length === 2, `Should have 2 edge labels (got ${edgeLabels.length})`);

// Glue to the pin (shape centre) makes Visio draw the route through the box.
// Endpoints sit on the bounding box, the way the SVG/draw.io edge is drawn.
const startBox = drawioResult.shapes.find(s => s.text === 'Start');
const startEdge = drawioResult.connectors.find(c => c.fromShape === drawioResult.shapes.indexOf(startBox));
const onBottom = startBox && startEdge &&
    Math.abs(startEdge.points[0].x - (startBox.x + startBox.width / 2)) < 1 &&
    Math.abs(startEdge.points[0].y - (startBox.y + startBox.height)) < 1;
assert(onBottom, 'a draw.io edge leaves the box at its edge, not its centre');

// Test 5: Draw.io raw mxGraphModel (without mxfile wrapper)
console.log('\n--- Test 5: Draw.io raw mxGraphModel ---');
const rawDrawio = `<mxGraphModel>
  <root>
    <mxCell id="0"/>
    <mxCell id="1" parent="0"/>
    <mxCell id="2" value="Box A" style="rounded=1;fillColor=#dae8fc;strokeColor=#6c8ebf;" vertex="1" parent="1">
      <mxGeometry x="50" y="50" width="120" height="60" as="geometry"/>
    </mxCell>
    <mxCell id="3" value="Box B" style="fillColor=#d5e8d4;strokeColor=#82b366;" vertex="1" parent="1">
      <mxGeometry x="250" y="50" width="120" height="60" as="geometry"/>
    </mxCell>
    <mxCell id="4" style="endArrow=classic;" edge="1" source="2" target="3" parent="1">
      <mxGeometry relative="1" as="geometry"/>
    </mxCell>
  </root>
</mxGraphModel>`;
const rawParser = new DrawioParser(rawDrawio);
const rawResult = rawParser.parse();
assert(rawResult.shapes.length === 2, `Should have 2 shapes (got ${rawResult.shapes.length})`);
assert(rawResult.connectors.length === 1, `Should have 1 connector (got ${rawResult.connectors.length})`);
assert(rawResult.shapes[0].style.rx > 0, 'First shape should be rounded');
assert(rawResult.connectors[0].arrowEnd, 'Connector should have arrow');

// Test 6: Real-world Draw.io (Köln BGP network diagram)
console.log('\n--- Test 6: Real-world Draw.io (BGP network) ---');
const bgpXml = fs.readFileSync('./test-samples/koeln-bgp.drawio', 'utf8');
const bgpParser = new DrawioParser(bgpXml);
const bgpResult = bgpParser.parse();
const bgpStats = bgpParser.getStats();

assert(bgpStats.shapes === 12, `Should have 12 shapes (got ${bgpStats.shapes})`);
assert(bgpStats.connectors === 10, `Should have 10 connectors (got ${bgpStats.connectors})`);
assert(bgpStats.texts === 2, `Should have 2 edge labels (got ${bgpStats.texts})`);

// Check &#xa; newline decoding in labels
const koelnShape = bgpResult.shapes.find(s => s.text && s.text.includes('Köln'));
assert(koelnShape && koelnShape.text.includes('\n'), 'Should decode &#xa; as newline in labels');

// Check all connectors are linked
const allLinked = bgpResult.connectors.every(c => c.fromShape !== null && c.toShape !== null);
assert(allLinked, 'All connectors should be linked to shapes');

// Check waypoints parsed for HA connectors
const haConn = bgpResult.connectors.find(c => c.points.length > 2);
assert(haConn && haConn.points.length === 4, `HA connector should have 4 points (got ${haConn ? haConn.points.length : 0})`);

// Test 7: CSS classes, <style> blocks, group transforms, nested frames
console.log('\n--- Test 7: Class-styled SVG ---');
const cssSvg = fs.readFileSync('./test-samples/css-classes.svg', 'utf8');
const result7 = new SvgParser(cssSvg).parse();

const frame = result7.shapes.find(s => s.width === 500);
const nodeA = result7.shapes.find(s => s.x === 80);
const nodeB = result7.shapes.find(s => s.x === 340);

assert(nodeA && nodeA.style.fill === '#FFFFFF', `.box fill from <style> (got ${nodeA && nodeA.style.fill})`);
assert(nodeA && nodeA.style.stroke === '#333333', `.box stroke from <style> (got ${nodeA && nodeA.style.stroke})`);
assert(nodeA && nodeA.style.strokeWidth === 1.5, `.box stroke-width from <style> (got ${nodeA && nodeA.style.strokeWidth})`);
assert(frame && frame.style.strokeDasharray === '8 5', 'frame keeps its dash pattern');
assert(frame && frame.isContainer === true, 'enclosing rect is detected as a container');
assert(frame && !frame.text, `container must not swallow inner labels (got ${JSON.stringify(frame && frame.text)})`);
assert(nodeA && nodeA.text === 'Node A\ndetail line', `inner box keeps both its lines (got ${JSON.stringify(nodeA && nodeA.text)})`);
assert(nodeA && nodeA.textRuns && nodeA.textRuns.length === 2, 'inner box keeps one style run per line');
assert(nodeA && nodeA.textRuns[0].style.fontSize === 18 && nodeA.textRuns[1].style.fontSize === 11,
    'heading and detail line keep their own font sizes');
assert(nodeA && nodeA.y === 150, `group transform is applied (got y=${nodeA && nodeA.y})`);

const title = result7.texts.find(t => t.text.indexOf('Title') === 0);
assert(title && title.style.textAnchor === 'start', `text-anchor defaults to start (got ${title && title.style.textAnchor})`);
assert(title && title.style.textColor === '#009640', `text colour comes from the class (got ${title && title.style.textColor})`);
assert(title && title.style.fontSize === 18, `class font-size applies (got ${title && title.style.fontSize})`);

const link = result7.connectors.find(c => c.arrowEnd || c.arrowStart);
assert(link && link.style.stroke === '#009640', `connector stroke from class (got ${link && link.style.stroke})`);
const aIdx = result7.shapes.indexOf(nodeA), bIdx = result7.shapes.indexOf(nodeB);
assert(link && link.fromShape === aIdx && link.toShape === bIdx,
    `connector glues to the inner boxes, not the frame (got ${link && link.fromShape}->${link && link.toShape})`);

// Test 8: the styles survive into the VSDX page
console.log('\n--- Test 8: VSDX output ---');
const pageXml = new VsdxBuilder(result7)._page1();

assert(pageXml.indexOf('N="FillForegnd" V="#FFFFFF"') !== -1, 'fill colour reaches the VSDX');
assert(pageXml.indexOf('N="LineColor" V="#333333"') !== -1, 'stroke colour reaches the VSDX');
assert(pageXml.indexOf('N="LineColor" V="#009640"') !== -1, 'connector colour reaches the VSDX');
assert(/<Text>Node A<\/Text>/.test(pageXml) && /<Text>detail line<\/Text>/.test(pageXml),
    'each source line is its own Visio text shape');
assert((pageXml.match(/<pp IX=/g) || []).length === 0,
    'no <pp> marker, which Visio draws as a box and then drops the rest');
assert(pageXml.indexOf('N="HorzAlign" V="0"') !== -1, 'left-aligned text is not force-centred');

const twoLineDrawio = `<mxGraphModel>
  <root>
    <mxCell id="0"/>
    <mxCell id="1" parent="0"/>
    <mxCell id="2" value="First&#xa;Second" vertex="1" parent="1">
      <mxGeometry x="10" y="10" width="120" height="60" as="geometry"/>
    </mxCell>
  </root>
</mxGraphModel>`;
const floatingXml = new VsdxBuilder(new DrawioParser(twoLineDrawio).parse())._page1();
assert(/<Text>First<\/Text>/.test(floatingXml) && /<Text>Second<\/Text>/.test(floatingXml),
    'a draw.io label with &#xa; becomes two Visio text shapes, not a newline glyph');
const twoLineDoc = domParser.parseFromString(floatingXml, 'application/xml');
const twoLineBox = Array.from(twoLineDoc.querySelectorAll('Shape')).find(s => {
    const cell = Array.from(s.querySelectorAll('Cell')).find(c => c.getAttribute('N') === 'Width');
    const w = cell && parseFloat(cell.getAttribute('V'));
    return w && Math.abs(w - 120 / 96) < 1e-6;
});
assert(twoLineBox && twoLineBox.getAttribute('Type') === 'Shape',
    'a labelled tile is a shape, not a nested group of its own lines');
assert(!/N="PinX"[^>]*F="Sheet\./.test(floatingXml),
    'extra label lines are not formula-glued to the tile, so they can be dragged out');

// Test 9: transforms, references, paint servers, markers
console.log('\n--- Test 9: Transforms and references ---');
const trSvg = fs.readFileSync('./test-samples/transforms-and-refs.svg', 'utf8');
const trParser = new SvgParser(trSvg);
const r9 = trParser.parse();

const nested = r9.shapes.find(s => s.width === 60);
assert(nested && nested.x === 130 && nested.y === 140,
    `nested translates compose (got ${nested && nested.x}/${nested && nested.y}, want 130/140)`);

const scaled = r9.shapes.find(s => s.width === 60 * 1 && s.height === 30);
assert(scaled && scaled.x === 10 && scaled.width === 60,
    `scale() resizes and repositions (got x=${scaled && scaled.x} w=${scaled && scaled.width})`);
assert(scaled && scaled.style.strokeWidth === 3,
    `stroke width scales with the transform (got ${scaled && scaled.style.strokeWidth}, want 3)`);

const grad = r9.shapes.find(s => s.x === 200 && s.width === 50);
assert(grad && grad.style.fill === '#800080',
    `gradient flattens to a blend, not black (got ${grad && grad.style.fill})`);

const used = r9.shapes.find(s => s.width === 40 && s.height === 20 && s.style.fill === '#eeeeee');
assert(used && used.x === 250 && used.y === 120,
    `<use> is expanded at its offset (got ${used && used.x}/${used && used.y})`);

const startArrow = r9.connectors.find(c => c.arrowStart);
assert(startArrow && startArrow.arrowStart === true && startArrow.arrowEnd === false,
    'marker-start is reported on the start, not the end');

const first = r9.texts.find(t => t.text === 'First');
const second = r9.texts.find(t => t.text === 'Second');
assert(first && second, 'text before a positioned <tspan> is kept, and the tspan becomes its own line');
assert(second && second.y === 120, `positioned tspan keeps its own y (got ${second && second.y})`);
assert(r9.texts.some(t => t.text === 'Bare bold'), 'an unpositioned tspan stays inline');

assert(trParser.warnings.some(w => w.indexOf('<image>') === 0), 'unsupported <image> is reported');
assert(trParser.warnings.some(w => w.indexOf('gradient') !== -1), 'gradient flattening is reported');
const glueRadius = SceneLayout.glueRadius(r9, SceneLayout.TUNING);
assert(glueRadius > 0 && glueRadius < 30,
    `glue radius scales with the drawing (got ${glueRadius})`);

// Test 10: those features reach the VSDX
console.log('\n--- Test 10: VSDX for transforms and references ---');
const xml9 = new VsdxBuilder(r9)._page1();

const pinXs = (xml9.match(/N="PinX" V="([-\d.]+)"/g) || [])
    .map(m => parseFloat(/([-\d.]+)"$/.exec(m)[1]));
assert(pinXs.length > 0 && pinXs.every(v => v >= 0),
    'viewBox origin is applied, so nothing lands at a negative X');
assert(xml9.indexOf('N="BeginArrow" V="5"') !== -1, 'marker-start becomes a BeginArrow');
assert(xml9.indexOf('N="LinePattern" V="3"') !== -1, 'a fine dash array becomes a dotted pattern');
assert(xml9.indexOf('N="LinePattern" V="4"') !== -1, 'a four-value dash array becomes dash-dot');
assert(xml9.indexOf('N="LineColorTrans" V="0.75"') !== -1, 'stroke-opacity becomes line transparency');
assert(xml9.indexOf('V="url(') === -1, 'no raw url() paint leaks into the VSDX');

// Test 11: rotation is carried as an angle rather than baked away
console.log('\n--- Test 11: Rotation ---');
const rotSvg = '<svg viewBox="0 0 200 200"><g transform="rotate(90,50,50)">' +
               '<rect x="40" y="20" width="20" height="60" fill="#fff" stroke="#000"/></g></svg>';
const r11 = new SvgParser(rotSvg).parse();
assert(Math.abs(r11.shapes[0].angle + Math.PI / 2) < 1e-6,
    `rotate(90) becomes a -90 deg Visio angle (got ${r11.shapes[0].angle})`);
assert(new VsdxBuilder(r11)._page1().indexOf('N="Angle" V="-1.57') !== -1,
    'the angle reaches the VSDX');

// Test 13: the extracted modules, tested directly
console.log('\n--- Test 13: SvgTransform (pure) ---');
const T = SvgTransform;
const near = (a, b, eps) => Math.abs(a - b) < (eps || 1e-9);

const composed = T.combine(T.parseAttr('translate(100,100)'), 'translate(20,30)');
const p13 = T.apply(10, 10, composed);
assert(p13.x === 130 && p13.y === 140, `nested translates compose (got ${p13.x}/${p13.y})`);

assert(T.parseAttr('') === null, 'an empty transform is no transform');
assert(T.apply(5, 7, null).x === 5, 'a null matrix is the identity');

const rot = T.parseAttr('rotate(90)');
const rp = T.apply(1, 0, rot);
assert(near(rp.x, 0) && near(rp.y, 1), `rotate(90) maps (1,0) to (0,1) (got ${rp.x}/${rp.y})`);

const about = T.apply(50, 50, T.parseAttr('rotate(37,50,50)'));
assert(near(about.x, 50) && near(about.y, 50), 'rotating about a point leaves that point fixed');

assert(T.scaleOf(T.parseAttr('scale(3)')) === 3, 'uniform scale is reported');
assert(near(T.scaleOf(T.parseAttr('scale(2,8)')), 4), 'non-uniform scale reports the geometric mean');
assert(T.scaleOf(T.parseAttr('rotate(45)')) === 1, 'a pure rotation does not scale');

const box = T.rect(10, 20, 30, 40, T.parseAttr('scale(2)'));
assert(box.x === 20 && box.width === 60 && box.angle === 0, 'scale maps a box without rotating it');

const chained = T.combine(T.parseAttr('translate(10,0)'), 'scale(2)');
const cp = T.apply(5, 0, chained);
assert(cp.x === 20, `order matters: translate then scale gives 20 (got ${cp.x})`);

const warned = [];
T.parseAttr('wobble(3)', (m) => warned.push(m));
assert(warned.length === 1 && warned[0].indexOf('wobble') !== -1,
    'an unknown transform function is reported, not silently applied');

console.log('\n--- Test 13b: SceneModel ---');
assert(SceneModel.validate(null).length > 0, 'a non-scene is rejected');
assert(SceneModel.validate({}).length > 0, 'a scene missing its arrays is rejected');

const filled = SceneModel.normalise({
    viewBox: { width: 100, height: 100 },
    shapes: [{ type: 'rect', x: 0, y: 0, width: 10, height: 10 }],
    connectors: [{ type: 'line', points: [{ x: 0, y: 0 }, { x: 5, y: 5 }], hasArrow: true }],
    texts: [{ x: 1, y: 1, text: 'hi' }]
});
assert(SceneModel.validate(filled).length === 0,
    `normalise fills every required field (${SceneModel.validate(filled).join('; ')})`);
assert(filled.shapes[0].isContainer === false && filled.shapes[0].angle === 0,
    'shape defaults are applied');
assert(filled.connectors[0].arrowEnd === true && filled.connectors[0].arrowStart === false,
    'a legacy hasArrow becomes an end arrow');
assert(filled.texts[0].standalone === false, 'texts default to being layout-assignable');

const broken = SceneModel.normalise({
    viewBox: { width: 10, height: 10 },
    shapes: [], connectors: [{ type: 'line', points: [{ x: 0, y: 0 }, { x: 1, y: 1 }], toShape: 7 }], texts: []
});
assert(SceneModel.validate(broken).some(p => p.indexOf('toShape') !== -1),
    'a connector pointing at a shape that does not exist is caught');

console.log('\n--- Test 13c: SvgStyleResolver cascade ---');
const cascadeDoc = domParser.parseFromString(
    '<svg xmlns="http://www.w3.org/2000/svg" font-family="Georgia">' +
    '<style>.a{fill:#111111;font-size:20px} rect{stroke:#222222}</style>' +
    '<rect id="r" class="a" fill="#999999" style="stroke-width:4"/></svg>', 'image/svg+xml');
const cascadeSvg = cascadeDoc.querySelector('svg');
const resolver = new SvgStyleResolver(cascadeSvg, () => {});
const resolved = resolver.resolve(cascadeSvg.querySelector('rect'));

assert(resolved.fill === '#111111', `a class beats a presentation attribute (got ${resolved.fill})`);
assert(resolved.stroke === '#222222', 'a type selector applies');
assert(resolved.strokeWidth === 4, 'an inline style beats everything');
assert(resolved.fontFamily === 'Georgia', 'font-family is inherited from the root');
assert(resolved.fontSize === 20, 'font-size comes from the class');
assert(resolver.resolve(cascadeSvg.querySelector('rect')) !== null, 'a second resolve hits the cache');

// Test 12: the generated package is a well-formed .vsdx
console.log('\n--- Test 12: Generated .vsdx package ---');

function xmlError(xml) {
    const doc = domParser.parseFromString(xml, 'application/xml');
    const err = doc.querySelector('parsererror');
    return err ? err.textContent.slice(0, 120) : null;
}

// Text that would break the XML if it were not escaped
const nastySvg = '<svg viewBox="0 0 200 120">' +
    '<rect x="10" y="10" width="120" height="50" fill="#ffffff" stroke="#000000"/>' +
    '<text x="20" y="30">a &lt; b &amp; c "q" &apos;x&apos;</text>' +
    '<text x="20" y="50">2 &gt; 1 &amp;&amp; 3 &lt; 4</text></svg>';
const nastyParsed = new SvgParser(nastySvg).parse();
const nastyBuilder = new VsdxBuilder(nastyParsed);

const parts = {
    'page1.xml': nastyBuilder._page1(),
    'pages.xml': nastyBuilder._pages(),
    'document.xml': nastyBuilder._document(),
    '[Content_Types].xml': nastyBuilder._contentTypes(),
    'app.xml': nastyBuilder._appProps(),
    'core.xml': nastyBuilder._coreProps(),
    'windows.xml': nastyBuilder._windows()
};
for (const name of Object.keys(parts)) {
    const err = xmlError(parts[name]);
    assert(err === null, `${name} is well-formed XML${err ? ' (' + err + ')' : ''}`);
}
assert(parts['page1.xml'].indexOf('a &lt; b &amp; c') !== -1, 'shape text is XML-escaped');

// Test 15: the output stays editable - shapes that resize, text that fits
console.log('\n--- Test 15: Editable output ---');
const editableSvg = '<svg viewBox="0 0 400 200"><style>.lbl{font-size:13px}</style>' +
    '<rect x="10" y="10" width="120" height="30" rx="8" fill="#fff" stroke="#000"/>' +
    '<text x="15" y="30" class="lbl">A label far wider than its own box</text>' +
    '<rect x="10" y="80" width="180" height="30" rx="8" fill="#fff" stroke="#000"/>' +
    '<text x="15" y="100" class="lbl">Fits here</text>' +
    '<text x="200" y="20" class="lbl">Free floating</text>' +
    '<ellipse cx="300" cy="100" rx="60" ry="30" fill="#fff" stroke="#000"/>' +
    '<polygon points="20,150 80,150 50,190" fill="#eee" stroke="#000"/></svg>';
const editableScene = new SvgParser(editableSvg).parse();
const editableXml = new VsdxBuilder(editableScene)._page1();
const editableDoc = domParser.parseFromString(editableXml, 'application/xml');
const cellV = (shape, name) => {
    const c = shape && shape.querySelector(`Cell[N="${name}"]`);
    return c ? parseFloat(c.getAttribute('V')) : null;
};
const cellF = (shape, name) => {
    const c = shape && shape.querySelector(`Cell[N="${name}"]`);
    return c ? String(c.getAttribute('F')) : '';
};

// Visio resizes a shape by rewriting Width and Height and re-evaluating the
// geometry. A coordinate held as a plain number has nothing to re-evaluate, so
// the outline stays its original size while the selection frame grows - which
// is what "I cannot drag this tile any bigger" looks like.
let frozenCells = 0;
for (const row of Array.from(editableDoc.querySelectorAll('Section[N="Geometry"] Row'))) {
    if (String(row.getAttribute('T')).indexOf('Rel') === 0) continue;
    for (const geomCell of Array.from(row.querySelectorAll('Cell'))) {
        if (!geomCell.getAttribute('F')) frozenCells++;
    }
}
assert(frozenCells === 0,
    `absolute geometry scales with Width/Height (got ${frozenCells} frozen cells)`);

// A font size is in user units like every other length, so it converts with
// the same scale. Treating it as points made every label 96/72 too large.
const fontSizes = (editableXml.match(/N="Size" V="([\d.]+)"/g) || [])
    .map(m => parseFloat(/V="([\d.]+)"/.exec(m)[1]));
assert(fontSizes.length >= 2 && fontSizes.every(v => Math.abs(v - 13 / 96) < 1e-9),
    `13px text becomes ${13 / 96} in, not ${13 / 72} in (got ${fontSizes.join(', ')})`);

// Visio wraps shape text at the shape width. A line that the SVG let overflow
// the tile must stay a free-floating line, not get stuffed in and wrapped.
assert(editableScene.texts.some(t => t.text.indexOf('far wider') !== -1),
    'a label wider than its tile stays a free-floating line, as in the SVG');
assert(!editableScene.shapes.some(s => s.text && s.text.indexOf('far wider') !== -1),
    'it is not folded into the tile');

const labelled = Array.from(editableDoc.querySelectorAll('Shape')).find(s => {
    const t = s.querySelector('Text');
    return t && t.textContent.indexOf('Fits here') !== -1;
});
assert(labelled && Math.abs(cellV(labelled, 'LeftMargin') - 4 / 96) < 1e-9,
    'text margins are in drawing units, not Visio\'s unscaled 4pt default');
assert(cellF(labelled, 'TxtWidth') === 'Width' && cellF(labelled, 'TxtHeight') === 'Height',
    'the text block follows the tile, so shrinking the box wraps the label');
// Rounding alone would be shorter, but only Visio acts on it: libvisio,
// which every Linux viewer uses, draws such a shape square. So the corners
// are real arcs - and they still have to scale, which the frozen-cell check
// above covers for every ArcTo row emitted here.
const arcs = Array.from((labelled && labelled.querySelectorAll('Row[T="ArcTo"]')) || []);
assert(labelled && cellV(labelled, 'Rounding') > 0 && arcs.length === 4,
    `a rounded rect is drawn with four real arcs (got ${arcs.length})`);
assert(arcs.every(r => Array.from(r.querySelectorAll('Cell')).every(c => c.getAttribute('F'))),
    'the arcs are formulas, so the corners survive a resize');
assert(arcs.every(r => /MIN\(Rounding/.test(String(
    (r.querySelector('Cell[N="A"]') || { getAttribute: () => '' }).getAttribute('F')))),
    'the arc bow follows the Rounding cell, so the radius stays editable');

// The width estimate decides where Visio wraps, so it has to follow the
// characters rather than count them. A flat per-character average
// under-measures short uppercase text - the shape a diagram's labels take -
// and an under-measured line is one Visio wraps inside a box that had room.
const metrics = new VsdxBuilder(new SvgParser('<svg viewBox="0 0 10 10"/>').parse());
const inch = 1 / 96;
assert(metrics._lineWidthInches('MMM', 13 * inch, false) >
       metrics._lineWidthInches('iii', 13 * inch, false) * 3,
    'the width estimate follows the characters, not just how many there are');
// Arial draws HA1 at 0.722 + 0.667 + 0.556 = 1.945 em
assert(metrics._lineWidthInches('HA1', 13 * inch, false) > 1.945 * 13 * inch,
    'a run of capitals is estimated at least as wide as Arial draws it');
assert(metrics._lineWidthInches('Hamburg', 13 * inch, true) >
       metrics._lineWidthInches('Hamburg', 13 * inch, false),
    'bold is estimated wider than regular in the same face');
// Aspose/Visio wrapped the last letter of this 18px bold title when the
// estimate was 135px. The box is only as wide as the estimate, so the
// estimate has to clear the painted width.
assert(metrics._lineWidthInches('Hauptstandort', 18 * inch, true) > 150 * inch,
    'a bold title is not estimated so short that Visio wraps the last letter');
assert(metrics._lineWidthInches('vnet-hub-connectivity', 18 * inch, true) > 210 * inch,
    'a long bold heading keeps one line');
assert(metrics._advance('\u2192') >= metrics._advance('M'),
    'a diagram arrow is at least an em wide, not a digit');

const overflowSvg = '<svg viewBox="0 0 400 80">' +
    '<rect x="10" y="10" width="260" height="30" fill="#fff" stroke="#000"/>' +
    '<text x="20" y="30" font-size="13px">Telekom IntraSelect (MPLS) \u2192 ExpressRoute Circuit</text></svg>';
const overflowScene = new SvgParser(overflowSvg).parse();
assert(overflowScene.texts.some(t => t.text.indexOf('Telekom IntraSelect') !== -1),
    'a label longer than its tile stays a free-floating line');
assert(!overflowScene.shapes.some(s => s.text && s.text.indexOf('Telekom') !== -1),
    'so Visio cannot wrap it inside the 30px-tall tile');

const clipSvg = '<svg viewBox="0 0 400 200">' +
    '<rect x="10" y="10" width="200" height="150" fill="none" stroke="#000"/>' +
    '<rect x="20" y="40" width="120" height="30" fill="#fff" stroke="#000"/>' +
    '<text x="25" y="60" font-size="13px">Telekom IntraSelect (MPLS) to ExpressRoute Circuit extra</text></svg>';
const clipScene = new SvgParser(clipSvg).parse();
const clipText = clipScene.texts.find(t => t.text.indexOf('Telekom IntraSelect') !== -1);
assert(clipText && clipText.parentShape === null,
    'an overflowing line is not nested into a frame narrower than the line');

// Test 16: frames hold their contents, connectors hold on to their shapes
console.log('\n--- Test 16: Groups and 1-D connectors ---');
const nestSvg = '<svg viewBox="0 0 400 300">' +
    '<rect x="10" y="10" width="200" height="200" fill="none" stroke="#000"/>' +
    '<rect x="30" y="40" width="60" height="30" fill="#fff" stroke="#000"/>' +
    '<text x="20" y="25">Frame title</text>' +
    '<rect x="300" y="10" width="50" height="50" fill="#fff" stroke="#000"/>' +
    '<path d="M90 55 L300 55 L300 35" stroke="#000" fill="none"/></svg>';
const nestScene = new SvgParser(nestSvg).parse();
const nestXml = new VsdxBuilder(nestScene)._page1();
const nestDoc = domParser.parseFromString(nestXml, 'application/xml');

// Null-tolerant on purpose: when a regression means a group is not there at
// all, every assertion below has to report FAIL rather than abort the suite.
const own = (el, tag, name) => (el && el.children
    ? Array.from(el.children).find(c => c.nodeName === tag && c.getAttribute('N') === name) || null
    : null);
const kidsOf = (el, tag) => (el && el.children
    ? Array.from(el.children).filter(c => c.nodeName === tag) : []);
const cellNum = (el, name) => {
    const c = own(el, 'Cell', name);
    return c ? parseFloat(c.getAttribute('V')) : null;
};
const formulaOf = (el, name) => {
    const c = own(el, 'Cell', name);
    return c ? String(c.getAttribute('F')) : '';
};
// A shape's own box, in absolute page inches, from the origin of its group
const boxOf = (el, ox, oy) => ({
    left: ox + cellNum(el, 'PinX') - cellNum(el, 'LocPinX'),
    bottom: oy + cellNum(el, 'PinY') - cellNum(el, 'LocPinY')
});
const IN = 1 / 96;
const nestPageH = 300 * IN;
const roots = kidsOf(kidsOf(nestDoc.documentElement, 'Shapes')[0], 'Shape');

const nestFrame = roots.find(s => s.getAttribute('Type') === 'Group');
assert(nestFrame !== undefined, 'a frame with contents becomes a group, so dragging it takes them along');
const framed = kidsOf(kidsOf(nestFrame, 'Shapes')[0], 'Shape');
assert(framed.length === 2, `the frame holds both its box and its title (got ${framed.length})`);
assert(own(nestFrame, 'Section', 'User') !== null, 'the group declares itself a Visio container');
assert(cellNum(nestFrame, 'DontMoveChildren') === 1,
    'resizing a group reflows children via formulas instead of stretching them');
const bannerScene = new DrawioParser('<mxGraphModel><root>' +
    '<mxCell id="0"/><mxCell id="1" parent="0"/>' +
    '<mxCell id="2" value="Banner" style="verticalAlign=top;" vertex="1" parent="1">' +
    '<mxGeometry x="0" y="0" width="900" height="700" as="geometry"/></mxCell>' +
    '<mxCell id="3" value="Child" vertex="1" parent="1">' +
    '<mxGeometry x="20" y="80" width="80" height="30" as="geometry"/></mxCell>' +
    '</root></mxGraphModel>').parse();
const bannerXml = new VsdxBuilder(bannerScene)._page1();
const bannerDoc = domParser.parseFromString(bannerXml, 'application/xml');
const bannerGroup = Array.from(bannerDoc.querySelectorAll('Shape')).find(s =>
    s.getAttribute('Type') === 'Group');
assert(bannerGroup && cellV(bannerGroup, 'TxtHeight') < cellV(bannerGroup, 'Height') / 4,
    'a frame title is not stretched to the height of the frame');
// Without DisplayMode Visio (and Aspose) draw only the frame and hide every
// member - which is the empty-box drawing we shipped. 1 = group behind members.
assert(cellNum(nestFrame, 'DisplayMode') === 1,
    'a group draws its members in front of the frame (DisplayMode=1)');

// Nesting is only useful if it does not move anything: a child's coordinates
// are relative to its group, so an error here shifts a whole frame's worth.
const frameBox = boxOf(nestFrame, 0, 0);
const tile = framed.find(s => cellNum(s, 'FillPattern') === 1);
const nestTitle = framed.find(s => cellNum(s, 'FillPattern') === 0);
const tileBox = boxOf(tile, frameBox.left, frameBox.bottom);
assert(Math.abs(tileBox.left - 30 * IN) < 1e-9 &&
       Math.abs(tileBox.bottom - (nestPageH - 70 * IN)) < 1e-9,
    `a nested box still lands where the SVG put it (got ${tileBox.left}/${tileBox.bottom})`);
assert(Math.abs(boxOf(nestTitle, frameBox.left, frameBox.bottom).left - 20 * IN) < 1e-9,
    'a frame title is nested too, instead of being left behind on the page');

const lone = roots.find(s => s.getAttribute('Type') === 'Shape' &&
    own(s, 'Cell', 'OneD') === null && cellNum(s, 'FillPattern') === 1);
assert(lone !== undefined && kidsOf(lone, 'Shapes').length === 0,
    'a box with nothing inside stays an ordinary shape rather than a group of one');

// Visio only honours the glue in <Connects> for a 1-D shape. Without OneD the
// arrows sat unattached and stayed behind when a box moved.
const connector = roots.find(s => own(s, 'Cell', 'OneD') !== null);
const nestRootsHasConnector = connector !== undefined;
assert(connector !== undefined && cellNum(connector, 'OneD') === 1,
    'a connector is a 1-D shape, which is what makes its glue real');
// Visio derives a 1-D shape's transform from its endpoints itself, and will
// not open a file that tries to do the same job with formulas. So the cells
// carry values - which means they have to be right, since nothing recomputes
// them on load.
assert(['PinX', 'PinY', 'Width', 'LocPinX', 'LocPinY', 'Angle']
    .every(n => formulaOf(connector, n) === 'null'),
    'the connector transform is written as values, not formulas');
const beginX = cellNum(connector, 'BeginX');
const endX = cellNum(connector, 'EndX');
const beginY = cellNum(connector, 'BeginY');
const endY = cellNum(connector, 'EndY');
assert(Math.abs(cellNum(connector, 'PinX') - (beginX + endX) / 2) < 1e-9 &&
       Math.abs(cellNum(connector, 'PinY') - (beginY + endY) / 2) < 1e-9,
    'the connector pin sits midway between its endpoints');
assert(Math.abs(cellNum(connector, 'Width') - Math.hypot(endX - beginX, endY - beginY)) < 1e-9,
    'its width is the distance between its endpoints');
assert(cellNum(connector, 'Height') > 0.01,
    'a straight connector still has height, so Visio can select and glue it');
assert(cellNum(connector, 'ObjType') === 2,
    'ObjType 2 makes the 1-D line a routable connector');
assert(String(formulaOf(connector, 'BegTrigger') || '').indexOf('XFTRIGGER') !== -1,
    'BegTrigger watches the glued shape, so the line moves with it');
assert(Math.abs(cellNum(connector, 'Angle') - Math.atan2(endY - beginY, endX - beginX)) < 1e-9,
    'its angle points from begin to end');
assert(Math.abs(cellNum(connector, 'BeginX') - 90 * IN) < 1e-9 &&
       Math.abs(cellNum(connector, 'BeginY') - (nestPageH - 55 * IN)) < 1e-9,
    'the begin point keeps the coordinate the SVG gave it');
assert(kidsOf(own(connector, 'Section', 'Geometry'), 'Row').length === 3,
    'the elbow survives the move into the 1-D frame instead of collapsing');

const nestConnects = Array.from(nestDoc.querySelectorAll('Connect'));
const tileId = tile ? tile.getAttribute('ID') : null;
assert(tileId !== null && nestConnects.some(c => c.getAttribute('ToSheet') === tileId),
    'glue reaches a shape nested inside a group');

// A rotated frame would turn its contents with it, and their coordinates are
// axis-aligned, so it must not adopt them.
const rotScene = new SvgParser('<svg viewBox="0 0 200 200">' +
    '<g transform="rotate(10,100,100)"><rect x="10" y="10" width="150" height="150" fill="none" stroke="#000"/></g>' +
    '<rect x="40" y="40" width="30" height="30" fill="#fff" stroke="#000"/></svg>').parse();
assert(rotScene.shapes.some(s => s.isContainer) &&
       rotScene.shapes.every(s => s.parentShape === null),
    'a rotated frame is still a frame but adopts nothing');

// A connector joins two things, so it can only belong to a frame that holds
// both. This one crosses the frame's edge and has to stay on the page.
assert(nestRootsHasConnector, 'a connector crossing a frame boundary stays on the page');

// One that runs between two boxes inside the same frame travels with it -
// glue alone would drag one end and stretch the route.
const insideSvg = '<svg viewBox="0 0 200 200">' +
    '<rect x="10" y="10" width="180" height="120" fill="none" stroke="#000"/>' +
    '<rect x="30" y="40" width="40" height="30" fill="#fff" stroke="#000"/>' +
    '<rect x="120" y="40" width="40" height="30" fill="#fff" stroke="#000"/>' +
    '<line x1="70" y1="55" x2="120" y2="55" stroke="#000"/></svg>';
const insideScene = new SvgParser(insideSvg).parse();
assert(insideScene.connectors[0].parentShape !== null,
    'a connector wholly inside a frame is nested into it');
const insideDoc = domParser.parseFromString(
    new VsdxBuilder(insideScene)._page1(), 'application/xml');
const insideRoots = kidsOf(kidsOf(insideDoc.documentElement, 'Shapes')[0], 'Shape');
const insideFrame = insideRoots.find(s => s.getAttribute('Type') === 'Group');
const insideConn = kidsOf(kidsOf(insideFrame, 'Shapes')[0], 'Shape')
    .find(s => own(s, 'Cell', 'OneD') !== null);
assert(insideConn !== undefined, 'the nested connector is emitted inside the group');
// Nesting must not move it: its endpoint is relative to the group now.
const insideFrameBox = boxOf(insideFrame, 0, 0);
assert(insideConn !== undefined &&
       Math.abs(insideFrameBox.left + cellNum(insideConn, 'BeginX') - 70 * IN) < 1e-9,
    'a nested connector still starts where the SVG put it');

// Building twice must not renumber anything, or <Connects> would drift
const twice = new VsdxBuilder(nestScene);
assert(twice._page1() === twice._page1(), 'building the page twice gives the same IDs');

// Test 17: the typeface the drawing asked for
console.log('\n--- Test 17: Fonts ---');
const fontSvg = '<svg viewBox="0 0 300 100" font-family="Arial, Helvetica, sans-serif">' +
    '<rect x="10" y="10" width="120" height="40" fill="#fff" stroke="#000"/>' +
    '<text x="20" y="35">Arial label</text>' +
    '<text x="160" y="35" font-family="&quot;Courier New&quot;, monospace">mono</text></svg>';
const fontBuilder = new VsdxBuilder(new SvgParser(fontSvg).parse());
const fontPage = fontBuilder._page1();
const fontDoc = fontBuilder._document();
const faces = {};
for (const m of fontDoc.matchAll(/<FaceName ID="(\d+)" Name="([^"]+)"/g)) faces[m[1]] = m[2];
const usedFonts = [...new Set([...fontPage.matchAll(/N="Font" V="(\d+)"/g)].map(m => m[1]))];

// A page that names a face the document never declares leaves Visio guessing.
assert(usedFonts.length > 0 && usedFonts.every(id => faces[id] !== undefined),
    `every face the page uses is declared (uses ${usedFonts.join(',')}, declared ${Object.keys(faces).join(',')})`);
assert(Object.values(faces).indexOf('Arial') !== -1,
    `an Arial drawing converts to Arial, not the default (declared: ${Object.values(faces).join(', ')})`);
assert(Object.values(faces).indexOf('Courier New') !== -1,
    'a second typeface gets its own face entry');
assert(faces['1'] === 'Calibri',
    'face 1 stays Calibri, which the No Style stylesheet names as its default');
// A CSS stack names fallbacks; Visio takes one name, and Helvetica is not it
assert(Object.values(faces).indexOf('Helvetica') === -1,
    'Helvetica is not offered to Visio, which has no such font');

// Full package, built the same way the browser builds it
(async () => {
    try {
        const blob = await new VsdxBuilder(nastyParsed).build();
        // JSZip emits a Blob; loadAsync wants bytes
        const zip = await JSZip.loadAsync(await blob.arrayBuffer());
        const required = ['[Content_Types].xml', '_rels/.rels', 'visio/document.xml',
                          'visio/pages/pages.xml', 'visio/pages/page1.xml'];
        for (const path of required) {
            assert(zip.file(path) !== null, `package contains ${path}`);
        }
        for (const path of Object.keys(zip.files)) {
            if (!/\.(xml|rels)$/.test(path)) continue;
            const err = xmlError(await zip.file(path).async('string'));
            assert(err === null, `${path} in the package is well-formed${err ? ' (' + err + ')' : ''}`);
        }
    } catch (e) {
        assert(false, `building the .vsdx package threw: ${e.message}`);
    }

    console.log(`\n=== Results: ${passed} passed, ${failed} failed ===`);
    process.exit(failed > 0 ? 1 : 0);
})();

