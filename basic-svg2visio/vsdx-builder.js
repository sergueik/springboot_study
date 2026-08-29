/**
 * VSDX Builder - Generates Visio .vsdx files from parsed SVG data.
 *
 * VSDX is a ZIP-based Open Packaging Convention (OPC) format containing:
 * - [Content_Types].xml  - MIME types for package parts
 * - _rels/.rels          - Root relationships
 * - visio/document.xml   - Document properties
 * - visio/pages/pages.xml - Page index
 * - visio/pages/page1.xml - Page content (shapes)
 * - docProps/app.xml     - Application properties
 *
 * Coordinates: Visio uses inches with origin at bottom-left.
 * SVG uses pixels with origin at top-left.
 *
 * Uses the official Cell N="..." V="..." attribute format per MS-VSDX spec.
 * Connectors use <Connects> elements to glue to source/target shapes.
 */

class VsdxBuilder {
    constructor(parsedSvg) {
        this.data = parsedSvg;
        this.shapeIdCounter = 1;
        this.pageWidthInches = this.data.viewBox.width / 96;
        this.pageHeightInches = this.data.viewBox.height / 96;

        // Ensure minimum page size
        if (this.pageWidthInches < 1) this.pageWidthInches = 8.5;
        if (this.pageHeightInches < 1) this.pageHeightInches = 11;

        this.scale = 1 / 96; // px to inches

        // Track shape IDs for connector gluing
        this.shapeIds = []; // index = data.shapes index, value = Visio shape ID
        this.connectorLinks = []; // { connectorId, fromShapeId, toShapeId }
    }

    async build() {
        const zip = new JSZip();

        // Add required VSDX structure
        zip.file('[Content_Types].xml', this._contentTypes());
        zip.file('_rels/.rels', this._rootRels());
        zip.file('visio/document.xml', this._document());
        zip.file('visio/_rels/document.xml.rels', this._documentRels());
        zip.file('visio/pages/pages.xml', this._pages());
        zip.file('visio/pages/_rels/pages.xml.rels', this._pagesRels());
        zip.file('visio/pages/page1.xml', this._page1());
        zip.file('docProps/app.xml', this._appProps());
        zip.file('docProps/core.xml', this._coreProps());
        zip.file('visio/windows.xml', this._windows());

        const blob = await zip.generateAsync({
            type: 'blob',
            mimeType: 'application/vnd.ms-visio.drawing',
            compression: 'DEFLATE',
            compressionOptions: { level: 6 }
        });

        return blob;
    }

    _nextId() {
        return this.shapeIdCounter++;
    }

    // Convert SVG Y (top-down) to Visio Y (bottom-up) in inches
    _svgToVisioY(svgY) {
        return this.pageHeightInches - (svgY * this.scale);
    }

    _svgToVisioX(svgX) {
        return svgX * this.scale;
    }

    _colorToRGB(color) {
        if (!color || color === 'none' || color === 'transparent') return null;

        // Named colors
        const named = {
            'white': '#FFFFFF', 'black': '#000000', 'red': '#FF0000',
            'green': '#008000', 'blue': '#0000FF', 'yellow': '#FFFF00',
            'orange': '#FFA500', 'purple': '#800080', 'gray': '#808080',
            'grey': '#808080', 'lightgray': '#D3D3D3', 'lightgrey': '#D3D3D3',
            'darkgray': '#A9A9A9', 'darkgrey': '#A9A9A9', 'navy': '#000080',
            'teal': '#008080', 'maroon': '#800000', 'olive': '#808000',
            'lime': '#00FF00', 'aqua': '#00FFFF', 'fuchsia': '#FF00FF',
            'silver': '#C0C0C0', 'coral': '#FF7F50', 'salmon': '#FA8072',
            'tomato': '#FF6347', 'gold': '#FFD700', 'khaki': '#F0E68C',
            'pink': '#FFC0CB', 'plum': '#DDA0DD', 'violet': '#EE82EE',
            'indigo': '#4B0082', 'cyan': '#00FFFF', 'magenta': '#FF00FF',
            'crimson': '#DC143C', 'chocolate': '#D2691E', 'sienna': '#A0522D',
            'tan': '#D2B48C', 'wheat': '#F5DEB3', 'ivory': '#FFFFF0',
            'beige': '#F5F5DC', 'linen': '#FAF0E6', 'lavender': '#E6E6FA',
            'steelblue': '#4682B4', 'royalblue': '#4169E1', 'cornflowerblue': '#6495ED',
            'dodgerblue': '#1E90FF', 'deepskyblue': '#00BFFF', 'lightskyblue': '#87CEFA',
            'lightblue': '#ADD8E6', 'powderblue': '#B0E0E6', 'cadetblue': '#5F9EA0',
            'darkblue': '#00008B', 'midnightblue': '#191970', 'slateblue': '#6A5ACD',
            'darkslateblue': '#483D8B', 'mediumslateblue': '#7B68EE',
            'forestgreen': '#228B22', 'darkgreen': '#006400', 'limegreen': '#32CD32',
            'lightgreen': '#90EE90', 'seagreen': '#2E8B57', 'mediumseagreen': '#3CB371',
            'springgreen': '#00FF7F', 'yellowgreen': '#9ACD32', 'olivedrab': '#6B8E23',
            'darkolivegreen': '#556B2F', 'darkred': '#8B0000', 'firebrick': '#B22222',
            'indianred': '#CD5C5C', 'lightcoral': '#F08080', 'darkorange': '#FF8C00',
            'orangered': '#FF4500', 'darkviolet': '#9400D3', 'mediumpurple': '#9370DB',
            'darkorchid': '#9932CC', 'mediumorchid': '#BA55D3', 'orchid': '#DA70D6',
            'rosybrown': '#BC8F8F', 'sandybrown': '#F4A460', 'peru': '#CD853F',
            'saddlebrown': '#8B4513', 'burlywood': '#DEB887', 'darkgoldenrod': '#B8860B',
            'goldenrod': '#DAA520', 'palegoldenrod': '#EEE8AA',
            'darkkhaki': '#BDB76B', 'darkseagreen': '#8FBC8F', 'palegreen': '#98FB98',
            'mediumaquamarine': '#66CDAA', 'mediumturquoise': '#48D1CC',
            'darkturquoise': '#00CED1', 'lightseagreen': '#20B2AA',
            'darkcyan': '#008B8B', 'paleturquoise': '#AFEEEE',
            'aliceblue': '#F0F8FF', 'azure': '#F0FFFF', 'mintcream': '#F5FFFA',
            'honeydew': '#F0FFF0', 'ghostwhite': '#F8F8FF', 'whitesmoke': '#F5F5F5',
            'floralwhite': '#FFFAF0', 'oldlace': '#FDF5E6', 'antiquewhite': '#FAEBD7',
            'papayawhip': '#FFEFD5', 'blanchedalmond': '#FFEBCD',
            'bisque': '#FFE4C4', 'peachpuff': '#FFDAB9', 'navajowhite': '#FFDEAD',
            'moccasin': '#FFE4B5', 'cornsilk': '#FFF8DC', 'lemonchiffon': '#FFFACD',
            'lightyellow': '#FFFFE0', 'lightgoldenrodyellow': '#FAFAD2',
            'mistyrose': '#FFE4E1', 'lavenderblush': '#FFF0F5', 'seashell': '#FFF5EE',
            'snow': '#FFFAFA', 'dimgray': '#696969', 'dimgrey': '#696969',
            'darkslategray': '#2F4F4F', 'darkslategrey': '#2F4F4F',
            'slategray': '#708090', 'slategrey': '#708090',
            'lightslategray': '#778899', 'lightslategrey': '#778899',
            'gainsboro': '#DCDCDC'
        };

        const lower = color.toLowerCase().trim();
        if (named[lower]) return named[lower];

        // Handle rgb() and rgba()
        const rgbMatch = color.match(/rgba?\(\s*(\d+)\s*,\s*(\d+)\s*,\s*(\d+)/);
        if (rgbMatch) {
            const r = parseInt(rgbMatch[1]).toString(16).padStart(2, '0');
            const g = parseInt(rgbMatch[2]).toString(16).padStart(2, '0');
            const b = parseInt(rgbMatch[3]).toString(16).padStart(2, '0');
            return '#' + r + g + b;
        }

        // Already hex
        if (color.startsWith('#')) {
            if (color.length === 4) {
                return '#' + color[1] + color[1] + color[2] + color[2] + color[3] + color[3];
            }
            return color.toUpperCase();
        }

        return '#000000';
    }

    _xmlEscape(str) {
        return str
            .replace(/&/g, '&amp;')
            .replace(/</g, '&lt;')
            .replace(/>/g, '&gt;')
            .replace(/"/g, '&quot;')
            .replace(/'/g, '&apos;');
    }

    _contentTypes() {
        return `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">
  <Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>
  <Default Extension="xml" ContentType="application/xml"/>
  <Override PartName="/visio/document.xml" ContentType="application/vnd.ms-visio.drawing.main+xml"/>
  <Override PartName="/visio/pages/pages.xml" ContentType="application/vnd.ms-visio.pages+xml"/>
  <Override PartName="/visio/pages/page1.xml" ContentType="application/vnd.ms-visio.page+xml"/>
  <Override PartName="/docProps/app.xml" ContentType="application/vnd.openxmlformats-officedocument.extended-properties+xml"/>
  <Override PartName="/docProps/core.xml" ContentType="application/vnd.openxmlformats-package.core-properties+xml"/>
  <Override PartName="/visio/windows.xml" ContentType="application/vnd.ms-visio.windows+xml"/>
</Types>`;
    }

    _rootRels() {
        return `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">
  <Relationship Id="rId1" Type="http://schemas.microsoft.com/visio/2010/relationships/document" Target="visio/document.xml"/>
  <Relationship Id="rId2" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/extended-properties" Target="docProps/app.xml"/>
  <Relationship Id="rId3" Type="http://schemas.openxmlformats.org/package/2006/relationships/metadata/core-properties" Target="docProps/core.xml"/>
</Relationships>`;
    }

    _document() {
        return `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<VisioDocument xmlns="http://schemas.microsoft.com/office/visio/2012/main"
               xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
               xml:space="preserve">
  <DocumentSettings TopPage="0" DefaultTextStyle="0" DefaultLineStyle="0" DefaultFillStyle="0">
    <GlueSettings>9</GlueSettings>
    <SnapSettings>65847</SnapSettings>
    <SnapExtensions>34</SnapExtensions>
  </DocumentSettings>
  <FaceNames>
    <FaceName ID="1" Name="Calibri" UnicodeRanges="-536870145 1073786111 0 0" CharSets="536871327 0" Panos="2 15 5 2 2 2 4 3 2 4"/>
    <FaceName ID="2" Name="Arial" UnicodeRanges="-536870145 1073786111 0 0" CharSets="536871327 0" Panos="2 11 6 4 2 2 2 2 2 4"/>
  </FaceNames>
  <StyleSheets>
    <StyleSheet ID="0" Name="No Style" NameU="No Style">
      <Cell N="LineWeight" V="0.01041666666666667"/>
      <Cell N="LineColor" V="#000000"/>
      <Cell N="LinePattern" V="1"/>
      <Cell N="LineCap" V="0"/>
      <Cell N="BeginArrow" V="0"/>
      <Cell N="EndArrow" V="0"/>
      <Cell N="BeginArrowSize" V="2"/>
      <Cell N="EndArrowSize" V="2"/>
      <Cell N="FillForegnd" V="#FFFFFF"/>
      <Cell N="FillBkgnd" V="#000000"/>
      <Cell N="FillPattern" V="1"/>
      <Cell N="ShdwForegnd" V="#D8D8D8"/>
      <Cell N="ShdwPattern" V="0"/>
      <Section N="Character">
        <Row IX="0">
          <Cell N="Font" V="1"/>
          <Cell N="Color" V="#000000"/>
          <Cell N="Size" V="0.1111111111111111"/>
        </Row>
      </Section>
      <Section N="Paragraph">
        <Row IX="0">
          <Cell N="HorzAlign" V="1"/>
        </Row>
      </Section>
    </StyleSheet>
  </StyleSheets>
</VisioDocument>`;
    }

    _documentRels() {
        return `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">
  <Relationship Id="rId1" Type="http://schemas.microsoft.com/visio/2010/relationships/pages" Target="pages/pages.xml"/>
  <Relationship Id="rId2" Type="http://schemas.microsoft.com/visio/2010/relationships/windows" Target="windows.xml"/>
</Relationships>`;
    }

    _pages() {
        return `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<Pages xmlns="http://schemas.microsoft.com/office/visio/2012/main"
       xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships">
  <Page ID="0" Name="Page-1" NameU="Page-1">
    <PageSheet>
      <Cell N="PageWidth" V="${this.pageWidthInches}"/>
      <Cell N="PageHeight" V="${this.pageHeightInches}"/>
      <Cell N="PageScale" V="1"/>
      <Cell N="DrawingScale" V="1"/>
      <Cell N="DrawingSizeType" V="1"/>
      <Cell N="DrawingScaleType" V="0"/>
    </PageSheet>
    <Rel r:id="rId1"/>
  </Page>
</Pages>`;
    }

    _pagesRels() {
        return `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">
  <Relationship Id="rId1" Type="http://schemas.microsoft.com/visio/2010/relationships/page" Target="page1.xml"/>
</Relationships>`;
    }

    _appProps() {
        return `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<Properties xmlns="http://schemas.openxmlformats.org/officeDocument/2006/extended-properties">
  <Application>SVG to Visio Converter</Application>
  <AppVersion>15.00</AppVersion>
</Properties>`;
    }

    _coreProps() {
        const now = new Date().toISOString();
        return `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<cp:coreProperties xmlns:cp="http://schemas.openxmlformats.org/package/2006/metadata/core-properties"
                   xmlns:dc="http://purl.org/dc/elements/1.1/"
                   xmlns:dcterms="http://purl.org/dc/terms/"
                   xmlns:dcmitype="http://purl.org/dc/dcmitype/"
                   xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
  <dc:creator>SVG to Visio Converter</dc:creator>
  <dcterms:created xsi:type="dcterms:W3CDTF">${now}</dcterms:created>
  <dcterms:modified xsi:type="dcterms:W3CDTF">${now}</dcterms:modified>
</cp:coreProperties>`;
    }

    _windows() {
        return `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<Windows xmlns="http://schemas.microsoft.com/office/visio/2012/main"
         xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships">
  <Window ID="0" WindowType="Drawing" WindowState="1073741824"
          WindowLeft="-1" WindowTop="-1" WindowWidth="1024" WindowHeight="768"
          Page="0">
    <ShowGrid>0</ShowGrid>
    <ShowGuides>0</ShowGuides>
    <ShowConnectionPoints>0</ShowConnectionPoints>
    <ShowPageBreaks>0</ShowPageBreaks>
    <TabSplitterPos>0.5</TabSplitterPos>
  </Window>
</Windows>`;
    }

    _page1() {
        let shapesXml = '';
        this.shapeIds = [];
        this.connectorLinks = [];

        // Render shapes first (so we have their IDs for connectors)
        for (let i = 0; i < this.data.shapes.length; i++) {
            const shape = this.data.shapes[i];
            const id = this._nextId();
            this.shapeIds[i] = id;
            shapesXml += this._buildShape(shape, id);
        }

        // Render standalone texts
        for (const text of this.data.texts) {
            shapesXml += this._buildTextShape(text);
        }

        // Render connectors
        for (const conn of this.data.connectors) {
            const id = this._nextId();
            shapesXml += this._buildConnector(conn, id);

            // Track connections for <Connects> section
            if (conn.fromShape !== null) {
                this.connectorLinks.push({
                    connectorId: id,
                    cell: 'BeginX',
                    fromPart: 9,
                    targetId: this.shapeIds[conn.fromShape],
                    toPart: 3
                });
            }
            if (conn.toShape !== null) {
                this.connectorLinks.push({
                    connectorId: id,
                    cell: 'EndX',
                    fromPart: 12,
                    targetId: this.shapeIds[conn.toShape],
                    toPart: 3
                });
            }
        }

        // Build <Connects> section
        let connectsXml = '';
        if (this.connectorLinks.length > 0) {
            connectsXml = '\n  <Connects>';
            for (const link of this.connectorLinks) {
                connectsXml += `\n    <Connect FromSheet="${link.connectorId}" FromCell="${link.cell}" FromPart="${link.fromPart}" ToSheet="${link.targetId}" ToCell="PinX" ToPart="${link.toPart}"/>`;
            }
            connectsXml += '\n  </Connects>';
        }

        return `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<PageContents xmlns="http://schemas.microsoft.com/office/visio/2012/main"
              xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
              xml:space="preserve">
  <Shapes>
${shapesXml}
  </Shapes>${connectsXml}
</PageContents>`;
    }

    _buildShape(shape, id) {
        // Center position in Visio coordinates (inches, bottom-left origin)
        const w = shape.width * this.scale;
        const h = shape.height * this.scale;
        const pinX = this._svgToVisioX(shape.x) + w / 2;
        const pinY = this._svgToVisioY(shape.y + shape.height) + h / 2;

        const fillColor = this._colorToRGB(shape.style.fill);
        const lineColor = this._colorToRGB(shape.style.stroke);
        const lineWeight = (shape.style.strokeWidth || 1) * this.scale;

        let cellsXml = '';
        let sectionsXml = '';
        let textXml = '';

        // XForm cells
        cellsXml += `      <Cell N="PinX" V="${pinX}"/>
      <Cell N="PinY" V="${pinY}"/>
      <Cell N="Width" V="${w}"/>
      <Cell N="Height" V="${h}"/>
      <Cell N="LocPinX" V="${w / 2}"/>
      <Cell N="LocPinY" V="${h / 2}"/>
      <Cell N="Angle" V="0"/>
`;

        // Fill cells
        if (fillColor && shape.style.fill !== 'none') {
            const trans = 1 - (shape.style.fillOpacity || 1) * (shape.style.opacity || 1);
            cellsXml += `      <Cell N="FillForegnd" V="${fillColor}"/>
      <Cell N="FillPattern" V="1"/>
      <Cell N="FillForegndTrans" V="${trans}"/>
`;
        } else {
            cellsXml += `      <Cell N="FillPattern" V="0"/>
`;
        }

        // Line cells
        if (lineColor && shape.style.stroke !== 'none') {
            const dashPattern = shape.style.strokeDasharray ? '2' : '1';
            cellsXml += `      <Cell N="LineWeight" V="${lineWeight}"/>
      <Cell N="LineColor" V="${lineColor}"/>
      <Cell N="LinePattern" V="${dashPattern}"/>
`;
        } else {
            cellsXml += `      <Cell N="LinePattern" V="0"/>
`;
        }

        // Rounding for rects
        if (shape.type === 'rect' && shape.style.rx > 0) {
            cellsXml += `      <Cell N="Rounding" V="${shape.style.rx * this.scale}"/>
`;
        }

        // Geometry section
        switch (shape.type) {
            case 'rect':
                sectionsXml += this._rectGeom(w, h, shape.style.rx * this.scale);
                break;
            case 'circle':
            case 'ellipse':
                sectionsXml += this._ellipseGeom(w, h);
                break;
            case 'diamond':
                sectionsXml += this._diamondGeom(w, h);
                break;
            case 'polygon':
            case 'path-shape':
                sectionsXml += this._polygonGeom(shape, w, h);
                break;
            default:
                sectionsXml += this._rectGeom(w, h, 0);
        }

        // Text + character formatting
        if (shape.text) {
            const textColor = (shape.textStyle && shape.textStyle.textColor) ?
                this._colorToRGB(shape.textStyle.textColor) : '#000000';
            const fontSize = ((shape.textStyle && shape.textStyle.fontSize) || 14) / 72;
            const isBold = (shape.textStyle && shape.textStyle.fontWeight === 'bold');

            sectionsXml += `
      <Section N="Character">
        <Row IX="0">
          <Cell N="Font" V="1"/>
          <Cell N="Color" V="${textColor || '#000000'}"/>
          <Cell N="Size" V="${fontSize}"/>
          <Cell N="Style" V="${isBold ? '1' : '0'}"/>
        </Row>
      </Section>
      <Section N="Paragraph">
        <Row IX="0">
          <Cell N="HorzAlign" V="1"/>
        </Row>
      </Section>`;

            textXml = `
      <Text>${this._xmlEscape(shape.text)}</Text>`;
        }

        return `    <Shape ID="${id}" Type="Shape" LineStyle="0" FillStyle="0" TextStyle="0">
${cellsXml}${sectionsXml}${textXml}
    </Shape>
`;
    }

    _rectGeom(w, h, rx) {
        if (rx > 0) {
            return `
      <Section N="Geometry" IX="0">
        <Cell N="NoFill" V="0"/>
        <Cell N="NoLine" V="0"/>
        <Row T="MoveTo" IX="1"><Cell N="X" V="0"/><Cell N="Y" V="${rx}"/></Row>
        <Row T="ArcTo" IX="2"><Cell N="X" V="${rx}"/><Cell N="Y" V="0"/><Cell N="A" V="${rx * 0.4142}"/></Row>
        <Row T="LineTo" IX="3"><Cell N="X" V="${w - rx}"/><Cell N="Y" V="0"/></Row>
        <Row T="ArcTo" IX="4"><Cell N="X" V="${w}"/><Cell N="Y" V="${rx}"/><Cell N="A" V="${rx * 0.4142}"/></Row>
        <Row T="LineTo" IX="5"><Cell N="X" V="${w}"/><Cell N="Y" V="${h - rx}"/></Row>
        <Row T="ArcTo" IX="6"><Cell N="X" V="${w - rx}"/><Cell N="Y" V="${h}"/><Cell N="A" V="${rx * 0.4142}"/></Row>
        <Row T="LineTo" IX="7"><Cell N="X" V="${rx}"/><Cell N="Y" V="${h}"/></Row>
        <Row T="ArcTo" IX="8"><Cell N="X" V="0"/><Cell N="Y" V="${h - rx}"/><Cell N="A" V="${rx * 0.4142}"/></Row>
        <Row T="LineTo" IX="9"><Cell N="X" V="0"/><Cell N="Y" V="${rx}"/></Row>
      </Section>`;
        }

        return `
      <Section N="Geometry" IX="0">
        <Cell N="NoFill" V="0"/>
        <Cell N="NoLine" V="0"/>
        <Row T="RelMoveTo" IX="1"><Cell N="X" V="0"/><Cell N="Y" V="0"/></Row>
        <Row T="RelLineTo" IX="2"><Cell N="X" V="1"/><Cell N="Y" V="0"/></Row>
        <Row T="RelLineTo" IX="3"><Cell N="X" V="1"/><Cell N="Y" V="1"/></Row>
        <Row T="RelLineTo" IX="4"><Cell N="X" V="0"/><Cell N="Y" V="1"/></Row>
        <Row T="RelLineTo" IX="5"><Cell N="X" V="0"/><Cell N="Y" V="0"/></Row>
      </Section>`;
    }

    _ellipseGeom(w, h) {
        return `
      <Section N="Geometry" IX="0">
        <Cell N="NoFill" V="0"/>
        <Cell N="NoLine" V="0"/>
        <Row T="Ellipse" IX="1">
          <Cell N="X" V="${w / 2}"/><Cell N="Y" V="${h / 2}"/>
          <Cell N="A" V="${w}"/><Cell N="B" V="${h / 2}"/>
          <Cell N="C" V="${w / 2}"/><Cell N="D" V="${h}"/>
        </Row>
      </Section>`;
    }

    _diamondGeom(w, h) {
        return `
      <Section N="Geometry" IX="0">
        <Cell N="NoFill" V="0"/>
        <Cell N="NoLine" V="0"/>
        <Row T="RelMoveTo" IX="1"><Cell N="X" V="0.5"/><Cell N="Y" V="0"/></Row>
        <Row T="RelLineTo" IX="2"><Cell N="X" V="1"/><Cell N="Y" V="0.5"/></Row>
        <Row T="RelLineTo" IX="3"><Cell N="X" V="0.5"/><Cell N="Y" V="1"/></Row>
        <Row T="RelLineTo" IX="4"><Cell N="X" V="0"/><Cell N="Y" V="0.5"/></Row>
        <Row T="RelLineTo" IX="5"><Cell N="X" V="0.5"/><Cell N="Y" V="0"/></Row>
      </Section>`;
    }

    _polygonGeom(shape, w, h) {
        if (!shape.points || shape.points.length < 3) {
            return this._rectGeom(w, h, 0);
        }

        let xml = `
      <Section N="Geometry" IX="0">
        <Cell N="NoFill" V="0"/>
        <Cell N="NoLine" V="0"/>`;

        const pts = shape.points;
        for (let i = 0; i < pts.length; i++) {
            const lx = (pts[i].x - shape.x) * this.scale;
            // Flip Y for Visio local coords
            const ly = h - (pts[i].y - shape.y) * this.scale;

            if (i === 0) {
                xml += `
        <Row T="MoveTo" IX="1"><Cell N="X" V="${lx}"/><Cell N="Y" V="${ly}"/></Row>`;
            } else {
                xml += `
        <Row T="LineTo" IX="${i + 1}"><Cell N="X" V="${lx}"/><Cell N="Y" V="${ly}"/></Row>`;
            }
        }

        // Close
        const lx0 = (pts[0].x - shape.x) * this.scale;
        const ly0 = h - (pts[0].y - shape.y) * this.scale;
        xml += `
        <Row T="LineTo" IX="${pts.length + 1}"><Cell N="X" V="${lx0}"/><Cell N="Y" V="${ly0}"/></Row>`;

        xml += `
      </Section>`;

        return xml;
    }

    _buildTextShape(text) {
        const id = this._nextId();

        // Estimate text size
        const fontSize = (text.style.fontSize || 14);
        const estWidth = Math.max(text.text.length * fontSize * 0.6 * this.scale, 1);
        const estHeight = fontSize * 1.5 * this.scale;

        const pinX = this._svgToVisioX(text.x);
        const pinY = this._svgToVisioY(text.y);

        const textColor = this._colorToRGB(text.style.textColor || text.style.fill) || '#000000';
        const fontSizeInches = fontSize / 72;
        const isBold = text.style.fontWeight === 'bold';

        return `    <Shape ID="${id}" Type="Shape" LineStyle="0" FillStyle="0" TextStyle="0">
      <Cell N="PinX" V="${pinX}"/>
      <Cell N="PinY" V="${pinY}"/>
      <Cell N="Width" V="${estWidth}"/>
      <Cell N="Height" V="${estHeight}"/>
      <Cell N="LocPinX" V="${estWidth / 2}"/>
      <Cell N="LocPinY" V="${estHeight / 2}"/>
      <Cell N="Angle" V="0"/>
      <Cell N="FillPattern" V="0"/>
      <Cell N="LinePattern" V="0"/>
      <Section N="Character">
        <Row IX="0">
          <Cell N="Font" V="1"/>
          <Cell N="Color" V="${textColor}"/>
          <Cell N="Size" V="${fontSizeInches}"/>
          <Cell N="Style" V="${isBold ? '1' : '0'}"/>
        </Row>
      </Section>
      <Section N="Paragraph">
        <Row IX="0">
          <Cell N="HorzAlign" V="1"/>
        </Row>
      </Section>
      <Text>${this._xmlEscape(text.text)}</Text>
    </Shape>
`;
    }

    _buildConnector(conn, id) {
        const pts = conn.points;
        if (pts.length < 2) return '';

        const startPt = pts[0];
        const endPt = pts[pts.length - 1];

        // Calculate bounding box
        let minX = Infinity, minY = Infinity, maxX = -Infinity, maxY = -Infinity;
        for (const p of pts) {
            minX = Math.min(minX, p.x);
            minY = Math.min(minY, p.y);
            maxX = Math.max(maxX, p.x);
            maxY = Math.max(maxY, p.y);
        }

        const w = Math.max((maxX - minX) * this.scale, 0.01);
        const h = Math.max((maxY - minY) * this.scale, 0.01);
        const pinX = this._svgToVisioX(minX) + w / 2;
        const pinY = this._svgToVisioY(maxY) + h / 2;

        const lineColor = this._colorToRGB(conn.style.stroke) || '#000000';
        const lineWeight = (conn.style.strokeWidth || 1) * this.scale;
        const dashPattern = conn.style.strokeDasharray ? '2' : '1';

        // Arrow: 5 = filled triangle
        const endArrow = conn.hasArrow ? '5' : '0';

        // Build geometry
        let geomXml = `
      <Section N="Geometry" IX="0">
        <Cell N="NoFill" V="1"/>
        <Cell N="NoLine" V="0"/>`;

        for (let i = 0; i < pts.length; i++) {
            const lx = (pts[i].x - minX) * this.scale;
            const ly = h - (pts[i].y - minY) * this.scale;

            if (i === 0) {
                geomXml += `
        <Row T="MoveTo" IX="1"><Cell N="X" V="${lx}"/><Cell N="Y" V="${ly}"/></Row>`;
            } else {
                geomXml += `
        <Row T="LineTo" IX="${i + 1}"><Cell N="X" V="${lx}"/><Cell N="Y" V="${ly}"/></Row>`;
            }
        }

        geomXml += `
      </Section>`;

        return `    <Shape ID="${id}" Type="Shape" LineStyle="0" FillStyle="0" TextStyle="0">
      <Cell N="PinX" V="${pinX}"/>
      <Cell N="PinY" V="${pinY}"/>
      <Cell N="Width" V="${w}"/>
      <Cell N="Height" V="${h}"/>
      <Cell N="LocPinX" V="${w / 2}"/>
      <Cell N="LocPinY" V="${h / 2}"/>
      <Cell N="Angle" V="0"/>
      <Cell N="BeginX" V="${this._svgToVisioX(startPt.x)}"/>
      <Cell N="BeginY" V="${this._svgToVisioY(startPt.y)}"/>
      <Cell N="EndX" V="${this._svgToVisioX(endPt.x)}"/>
      <Cell N="EndY" V="${this._svgToVisioY(endPt.y)}"/>
      <Cell N="FillPattern" V="0"/>
      <Cell N="LineWeight" V="${lineWeight}"/>
      <Cell N="LineColor" V="${lineColor}"/>
      <Cell N="LinePattern" V="${dashPattern}"/>
      <Cell N="BeginArrow" V="0"/>
      <Cell N="EndArrow" V="${endArrow}"/>
      <Cell N="BeginArrowSize" V="2"/>
      <Cell N="EndArrowSize" V="2"/>${geomXml}
    </Shape>
`;
    }
}
