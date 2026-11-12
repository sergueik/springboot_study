// @ts-check
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
    /** Face 1, named by the No Style stylesheet, and the fallback for a
     *  drawing that names no font at all. */
    static DEFAULT_FACE = 'Calibri';

    /**
     * Advance widths in ems, grouped by width, for the Helvetica metrics that
     * Arial matches.
     *
     * A flat average per character is wrong in the direction that hurts: it
     * under-measures short uppercase text, which is exactly what a diagram's
     * labels are full of. Measured against real Arial, a 0.6 em average
     * under-measured "HA1" by 8% - and an under-measured line is one Visio
     * wraps inside a box that had room for it.
     */
    static ADVANCES = {
        '0.191': "'",
        '0.222': "ijl",
        '0.26': "|",
        '0.278': " !,./:;I[\\]ft",
        '0.333': "()-`{}",
        '0.355': "\"",
        '0.389': "*",
        '0.469': "^",
        '0.5': "Jcksvxz",
        '0.556': "#$0123456789?_abdeghnopqu",
        '0.584': "+<=>",
        '0.611': "FTZ",
        '0.667': "ABEKPRSVXY&",
        '0.722': "CDHNRUw",
        '0.778': "GOQ",
        '0.833': "Mm",
        '0.889': "%",
        '0.944': "W",
        '1.015': "@"
    };

    /** Built once from ADVANCES on first use. @type {Map<string, number>|null} */
    static _advanceMap = null;

    constructor(parsedSvg) {
        this.data = parsedSvg;
        this.shapeIdCounter = 1;
        this.pageWidthInches = this.data.viewBox.width / 96;
        this.pageHeightInches = this.data.viewBox.height / 96;

        // Ensure minimum page size
        if (this.pageWidthInches < 1) this.pageWidthInches = 8.5;
        if (this.pageHeightInches < 1) this.pageHeightInches = 11;

        // A viewBox may start anywhere; without this offset a drawing with a
        // negative origin lands off the page.
        this.viewBoxX = this.data.viewBox.x || 0;
        this.viewBoxY = this.data.viewBox.y || 0;

        this.scale = 1 / 96; // px to inches

        // Text sits this far off the shape's border. Visio's own default is
        // 4pt, which is a different size on every drawing because it does not
        // scale; 4 user units matches the inset the source drawings use.
        this.textMargin = 4 * this.scale;

        // Typefaces, in the order they are first used. Calibri stays first
        // because the No Style stylesheet names face 1 as its default.
        this.faceNames = [VsdxBuilder.DEFAULT_FACE];
        this.faceIds = new Map([[VsdxBuilder.DEFAULT_FACE.toLowerCase(), 1]]);

        // Track shape IDs for connector gluing
        this.shapeIds = []; // index = data.shapes index, value = Visio shape ID
        this.connectorLinks = []; // { connectorId, fromShapeId, toShapeId }
    }

    async build() {
        const zip = new JSZip();

        // The page first: it is what registers the typefaces that
        // document.xml has to declare.
        const pageXml = this._page1();

        // Add required VSDX structure
        zip.file('[Content_Types].xml', this._contentTypes());
        zip.file('_rels/.rels', this._rootRels());
        zip.file('visio/document.xml', this._document());
        zip.file('visio/_rels/document.xml.rels', this._documentRels());
        zip.file('visio/pages/pages.xml', this._pages());
        zip.file('visio/pages/_rels/pages.xml.rels', this._pagesRels());
        zip.file('visio/pages/page1.xml', pageXml);
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
        return this.pageHeightInches - ((svgY - this.viewBoxY) * this.scale);
    }

    _svgToVisioX(svgX) {
        return (svgX - this.viewBoxX) * this.scale;
    }

    /**
     * Map stroke-dasharray onto Visio's line patterns instead of collapsing
     * every dashed style into the same one.
     * 1 = solid, 2 = dashed, 3 = dotted, 4 = dash-dot.
     */
    _linePattern(dasharray) {
        if (!dasharray) return 1;
        const nums = String(dasharray).split(/[\s,]+/)
            .map(parseFloat).filter(n => !isNaN(n) && n >= 0);
        if (nums.length === 0) return 1;

        const dashes = nums.filter((n, i) => i % 2 === 0);
        if (Math.max.apply(null, dashes) <= 2) return 3;
        if (nums.length >= 4) return 4;
        return 2;
    }

    /** Visio transparency (0..1) from the opacity pair on a style. */
    _transparency(a, b) {
        const v = (a === undefined ? 1 : a) * (b === undefined ? 1 : b);
        return Math.min(1, Math.max(0, 1 - v));
    }

    // SVG text-anchor -> Visio HorzAlign (0 = left, 1 = centre, 2 = right)
    _horzAlign(style) {
        const anchor = (style && style.textAnchor) || 'start';
        if (anchor === 'middle') return 1;
        if (anchor === 'end') return 2;
        return 0;
    }

    /**
     * A geometry cell Visio can rescale.
     *
     * Visio resizes a shape by writing its Width and Height cells and then
     * re-evaluating the geometry. A geometry cell holding a plain number has
     * nothing to re-evaluate, so the outline stays put while the selection
     * frame grows - the shape looks like it cannot be resized at all. Every
     * coordinate therefore carries a formula in terms of Width/Height, with
     * the computed value cached in V for readers that do not evaluate
     * formulas.
     *
     * @param {string} name
     * @param {number} value the cached value, in inches
     * @param {string} formula
     */
    _geomCell(name, value, formula) {
        return `<Cell N="${name}" V="${value}" F="${this._xmlEscape(formula)}"/>`;
    }

    /** A geometry coordinate as a fraction of the shape's Width or Height. */
    _relGeomCell(name, value, extent, dimension) {
        const fraction = extent > 0 ? value / extent : 0;
        return this._geomCell(name, value, `${dimension}*${fraction}`);
    }

    /**
     * The typeface a style asks for, as a Visio face name.
     *
     * A CSS font stack names fallbacks the drawing's author was willing to
     * accept; Visio takes one name, so the first is it. The generic families
     * have no meaning in Visio either, so they become the face Windows would
     * have substituted anyway - and Helvetica, which no Windows install has,
     * becomes the Arial it is always substituted with.
     */
    _faceName(style) {
        const stack = (style && style.fontFamily) || '';
        const first = String(stack).split(',')[0].trim().replace(/^["']|["']$/g, '');
        const generic = {
            'sans-serif': 'Arial',
            'serif': 'Times New Roman',
            'monospace': 'Courier New',
            'cursive': 'Comic Sans MS',
            'fantasy': 'Impact',
            'helvetica': 'Arial',
            'system-ui': 'Segoe UI',
            'ui-sans-serif': 'Arial',
            'ui-serif': 'Times New Roman',
            'ui-monospace': 'Courier New'
        };
        if (!first) return VsdxBuilder.DEFAULT_FACE;
        return generic[first.toLowerCase()] || first;
    }

    /**
     * The Font cell value for a style, registering the face if it is new.
     *
     * Every run used to be written as face 1, which is Calibri, whatever the
     * drawing asked for - so an Arial diagram came out of the converter in
     * Calibri. The faces are collected as the page is built and declared in
     * document.xml afterwards.
     */
    _fontId(style) {
        const name = this._faceName(style);
        const key = name.toLowerCase();
        if (!this.faceIds.has(key)) {
            this.faceNames.push(name);
            this.faceIds.set(key, this.faceNames.length);
        }
        return this.faceIds.get(key);
    }

    /**
     * Font size in inches.
     *
     * An SVG font size is in user units - the same units as every coordinate
     * on the drawing - so it converts with the same scale. Treating it as
     * points instead (a plain /72) made every label 96/72 = 33% too large for
     * the box around it, which is why text that fitted in the SVG wrapped and
     * spilled out in Visio.
     */
    _fontSizeInches(style) {
        const size = style && style.fontSize ? style.fontSize : 14;
        return size * this.scale;
    }

    /** Advance width of one character, in ems. */
    _advance(ch) {
        let map = VsdxBuilder._advanceMap;
        if (!map) {
            map = new Map();
            for (const [width, chars] of Object.entries(VsdxBuilder.ADVANCES)) {
                for (const c of chars) map.set(c, parseFloat(width));
            }
            VsdxBuilder._advanceMap = map;
        }
        const w = map.get(ch);
        if (w !== undefined) return w;
        const code = ch.codePointAt(0);
        // Arrows in labels (→ ↔) are closer to an em than to a digit.
        if (code >= 0x2190 && code <= 0x21FF) return 1.0;
        if (code === 0x00B7 || code === 0x2022) return 0.35;
        if (code === 0x2013 || code === 0x2014) return 0.6;
        // Accented letters, CJK: a digit is the common case among them.
        return 0.556;
    }

    /**
     * Rendered width of one line, in inches.
     *
     * Erring wide is cheap - it only widens an invisible text block - while
     * erring narrow makes Visio wrap a line that fitted in the source
     * drawing, so the sum carries a few percent of slack. Bold is wider than
     * regular in the same face by roughly that much again.
     *
     * @param {string} text
     * @param {number} fontSizeInches
     * @param {boolean} [bold]
     */
    _lineWidthInches(text, fontSizeInches, bold) {
        let ems = 0;
        for (const ch of text) ems += this._advance(ch);
        // 1.08/1.15 still under-measured Arial in Visio: "Hauptstandort" at
        // 18px bold wrapped the last letter inside a box sized to the estimate.
        return ems * fontSizeInches * (bold ? 1.30 : 1.18);
    }

    /**
     * Draw.io labels keep line breaks inside one run (`&#xa;`). Split them
     * so each line is its own Visio paragraph.
     *
     * @param {{text: string, style: any}[]} runs
     * @returns {{text: string, style: any}[]}
     */
    _flattenTextRuns(runs) {
        const flat = [];
        for (const run of runs) {
            const parts = String(run.text == null ? '' : run.text).split(/\r?\n/);
            for (const part of parts) {
                flat.push({ text: part, style: run.style || {} });
            }
        }
        return flat;
    }

    /**
     * The text block: margins, and a block at least as large as the text.
     *
     * Visio wraps shape text at the shape's width, so a label wider than its
     * box breaks into extra lines and spills out of a short tile - where the
     * same label in the source SVG simply overflowed on one line. TxtWidth and
     * TxtHeight keep the block as large as the text needs, and the block's pin
     * follows the paragraph alignment so the overflow grows away from the edge
     * the text is anchored to rather than back across it.
     *
     * When lockToShape is set, TxtWidth/TxtHeight follow the box so shrinking
     * the field wraps the label. Values-only TxtWidth kept overflow on one
     * line (matching the SVG) but left the text the original size when the
     * user pulled the handles in.
     *
     * @param {{text: string, style: any}[]} runs
     * @param {number} w shape width in inches
     * @param {number} h shape height in inches
     * @param {number} [vertAlign] 0 top, 1 middle, 2 bottom
     * @param {boolean} [lockToShape]
     */
    _textBlockCells(runs, w, h, vertAlign, lockToShape) {
        const margin = this.textMargin;
        let needW = 0;
        let needH = 2 * margin;
        let overflowAlign = 0;

        for (const run of this._flattenTextRuns(runs)) {
            const size = this._fontSizeInches(run.style);
            const bold = (run.style || {}).fontWeight === 'bold';
            const lineW = this._lineWidthInches(run.text, size, bold) + 2 * margin + size;
            if (lineW > needW) {
                needW = lineW;
                overflowAlign = this._horzAlign(run.style);
            }
            // One source line, plus extra rows if Visio still wraps at Width
            // even though TxtWidth is larger (some viewers do).
            const lines = lineW > w ? Math.max(1, Math.ceil(lineW / Math.max(w, 0.01))) : 1;
            needH += size * 1.5 * lines;
        }

        const va = vertAlign == null ? 1 : vertAlign;
        // A frame is huge; filling it with the text block makes a stray
        // glyph as tall as the frame. Size the block to the text, then pin
        // it to the top of a container or the centre of a tile.
        const txtW = va === 0 ? Math.max(needW, 2 * margin + 0.1) : Math.max(w, needW);
        const txtH = Math.max(needH, 2 * margin + 0.1);
        const pin = overflowAlign === 1 ? 0.5 : (overflowAlign === 2 ? 1 : 0);
        const txtPinY = va === 0 ? h - txtH / 2 : h / 2;
        const pinF = pin === 1 ? 'Width' : (pin === 0.5 ? 'Width*0.5' : '0');
        const locF = pin === 1 ? 'TxtWidth' : (pin === 0.5 ? 'TxtWidth*0.5' : '0');

        let sizeCells;
        if (lockToShape && va !== 0) {
            sizeCells = `      <Cell N="TxtWidth" V="${w}" F="Width"/>
      <Cell N="TxtHeight" V="${h}" F="Height"/>
      <Cell N="TxtPinX" V="${w * 0.5}" F="Width*0.5"/>
      <Cell N="TxtPinY" V="${h * 0.5}" F="Height*0.5"/>
      <Cell N="TxtLocPinX" V="${w * 0.5}" F="TxtWidth*0.5"/>
      <Cell N="TxtLocPinY" V="${h * 0.5}" F="TxtHeight*0.5"/>
`;
        } else if (lockToShape) {
            sizeCells = `      <Cell N="TxtWidth" V="${w}" F="Width"/>
      <Cell N="TxtHeight" V="${txtH}"/>
      <Cell N="TxtPinX" V="${w * pin}" F="${pinF}"/>
      <Cell N="TxtPinY" V="${txtPinY}"/>
      <Cell N="TxtLocPinX" V="${w * pin}" F="${locF}"/>
      <Cell N="TxtLocPinY" V="${txtH * 0.5}"/>
`;
        } else {
            sizeCells = `      <Cell N="TxtWidth" V="${txtW}"/>
      <Cell N="TxtHeight" V="${txtH}"/>
      <Cell N="TxtPinX" V="${w * pin}"/>
      <Cell N="TxtPinY" V="${txtPinY}"/>
      <Cell N="TxtLocPinX" V="${txtW * pin}"/>
      <Cell N="TxtLocPinY" V="${txtH * 0.5}"/>
`;
        }

        return `      <Cell N="LeftMargin" V="${margin}"/>
      <Cell N="RightMargin" V="${margin}"/>
      <Cell N="TopMargin" V="${margin}"/>
      <Cell N="BottomMargin" V="${margin}"/>
      <Cell N="VerticalAlign" V="${va}"/>
${sizeCells}`;
    }

    /**
     * One character row, one paragraph row, and one marker pair per source line.
     *
     * A line feed in <Text> is the paragraph separator, and only immediately
     * before <pp>. Any other newline is drawn as a missing-glyph box - which
     * is what the second line of every tile became. Marker IXs are sequential
     * and not reused: Visio rewrites reused IXs by inventing extra rows, and
     * a .vsdx is less forgiving than the old VDX loader that used to do that
     * for you. cp before pp is the order Visio itself writes.
     *
     * @param {{text: string, style: any}[]} runs
     * @returns {{charRows: string, paraRows: string, body: string}}
     */
    _textRunsXml(runs) {
        const lines = this._flattenTextRuns(runs);
        let charRows = '';
        let paraRows = '';
        let body = '';
        for (let i = 0; i < lines.length; i++) {
            const st = lines[i].style || {};
            const color = this._colorToRGB(st.textColor || st.fill) || '#000000';
            const size = this._fontSizeInches(st);
            const bold = st.fontWeight === 'bold' ? '1' : '0';
            const align = this._horzAlign(st);
            charRows += `
        <Row IX="${i}">
          <Cell N="Font" V="${this._fontId(st)}"/>
          <Cell N="Color" V="${color}"/>
          <Cell N="Size" V="${size}"/>
          <Cell N="Style" V="${bold}"/>
        </Row>`;
            paraRows += `
        <Row IX="${i}">
          <Cell N="HorzAlign" V="${align}"/>
        </Row>`;
            // Never put a line feed in <Text>: Visio draws U+000A as a
            // missing-glyph box, and if the text block is the size of a
            // frame that box is inches tall. A <pp> starts the next
            // paragraph. The first run has no marker - a leading cp/pp
            // ate the first letter of every label.
            body += this._xmlEscape(lines[i].text);
        }
        return { charRows, paraRows, body };
    }

    /**
     * One child shape per source line, local to the parent box. Visio will
     * not render a second paragraph in <Text> without a glyph box.
     *
     * @param {{text: string, style: any}[]} runs
     * @param {number} w parent width in inches
     * @param {number} h parent height in inches
     * @param {number} ox lower-left of the tile in the parent, inches
     * @param {number} oy
     */
    _lineTextShapes(runs, w, h, ox, oy) {
        const margin = this.textMargin;
        let yFromTop = margin;
        let xml = '';
        for (const run of runs) {
            const st = run.style || {};
            const size = this._fontSizeInches(st);
            const bold = st.fontWeight === 'bold';
            const align = this._horzAlign(st);
            const lineW = Math.min(
                w,
                Math.max(this._lineWidthInches(run.text, size, bold) + 2 * margin, 0.2)
            );
            const lineH = 2 * margin + size * 1.2;
            const pinX = ox + (align === 1 ? w / 2 : (align === 2 ? w - margin : margin));
            const locPinX = align === 1 ? lineW / 2 : (align === 2 ? lineW : 0);
            const pinY = oy + h - yFromTop - 0.8 * size;
            const locPinY = lineH / 2 - 0.35 * size;
            yFromTop += size * 1.5;
            const one = this._textRunsXml([run]);
            xml += `    <Shape ID="${this._nextId()}" Type="Shape" LineStyle="0" FillStyle="0" TextStyle="0">
      <Cell N="PinX" V="${pinX}"/>
      <Cell N="PinY" V="${pinY}"/>
      <Cell N="Width" V="${lineW}"/>
      <Cell N="Height" V="${lineH}"/>
      <Cell N="LocPinX" V="${locPinX}"/>
      <Cell N="LocPinY" V="${locPinY}"/>
      <Cell N="Angle" V="0"/>
      <Cell N="FillPattern" V="0"/>
      <Cell N="LinePattern" V="0"/>
${this._textBlockCells([run], lineW, lineH, 1, true)}      <Section N="Character">${one.charRows}
      </Section>
      <Section N="Paragraph">${one.paraRows}
      </Section>
      <Text>${one.body}</Text>
    </Shape>
`;
        }
        return xml;
    }

    _colorToRGB(color) {
        if (!color || color === 'none' || color === 'transparent') return null;
        // Unresolved paint servers stay unpainted rather than turning black
        if (/^url\(/i.test(color.trim())) return null;

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
  <FaceNames>${this._faceNamesXml()}
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

    /**
     * The document's typeface table.
     *
     * Panose describes a face's shape and is what Visio falls back on when the
     * named font is missing, so the two faces whose values are known keep
     * them; for anything else naming the face is all this can honestly say.
     */
    _faceNamesXml() {
        const known = {
            calibri: 'Panos="2 15 5 2 2 2 4 3 2 4"',
            arial: 'Panos="2 11 6 4 2 2 2 2 2 4"'
        };
        return this.faceNames.map((name, i) => {
            const panose = known[name.toLowerCase()];
            return `
    <FaceName ID="${i + 1}" Name="${this._xmlEscape(name)}" ` +
                `UnicodeRanges="-536870145 1073786111 0 0" CharSets="536871327 0"` +
                (panose ? ' ' + panose : '') + '/>';
        }).join('');
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
        this.connectorLinks = [];
        // Building the page twice must produce the same IDs
        this.shapeIdCounter = 1;

        // IDs before geometry: a connector may glue to a box nested three
        // frames deep, and <Connects> needs that ID before the tree is walked.
        this.shapeIds = this.data.shapes.map(() => this._nextId());
        this.textIds = this.data.texts.map(() => this._nextId());
        this.connectorIds = this.data.connectors.map(() => this._nextId());

        this.childShapes = new Map();
        this.childTexts = new Map();
        this.childConnectors = new Map();
        this.data.shapes.forEach((shape, i) => this._addChild(this.childShapes, shape.parentShape, i));
        this.data.texts.forEach((text, i) => this._addChild(this.childTexts, text.parentShape, i));
        this.data.connectors.forEach((conn, i) => this._addChild(this.childConnectors, conn.parentShape, i));

        // The page itself is the outermost coordinate system
        const pageOrigin = { x: 0, y: 0 };
        for (const i of (this.childShapes.get(null) || [])) {
            shapesXml += this._buildShapeTree(i, pageOrigin);
        }
        for (const i of (this.childTexts.get(null) || [])) {
            shapesXml += this._buildTextShape(this.data.texts[i], this.textIds[i], pageOrigin);
        }
        for (const i of (this.childConnectors.get(null) || [])) {
            shapesXml += this._buildConnector(this.data.connectors[i], this.connectorIds[i], pageOrigin);
        }
        for (let i = 0; i < this.data.connectors.length; i++) {
            this._trackGlue(this.data.connectors[i], this.connectorIds[i]);
        }

        // Build <Connects> section
        let connectsXml = '';
        if (this.connectorLinks.length > 0) {
            connectsXml = '\n  <Connects>';
            for (const link of this.connectorLinks) {
                connectsXml += `\n    <Connect FromSheet="${link.connectorId}" FromCell="${link.cell}" FromPart="${link.fromPart}" ToSheet="${link.targetId}" ToCell="${link.toCell}" ToPart="${link.toPart}"/>`;
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

    /**
     * Record a connector's glue for the page's <Connects> section.
     *
     * <Connects> lives on the page whatever depth the shapes sit at, so this
     * runs for every connector, nested or not.
     */
    /**
     * Four edge midpoints, Visio local (origin bottom-left). Row IX 0..3 =
     * top, right, bottom, left; ToPart is 100+IX.
     */
    _connectionSection(w, h) {
        const rows = [
            { x: w / 2, y: h, xf: 'Width*0.5', yf: 'Height', dx: 0, dy: 1 },
            { x: w, y: h / 2, xf: 'Width', yf: 'Height*0.5', dx: 1, dy: 0 },
            { x: w / 2, y: 0, xf: 'Width*0.5', yf: '0', dx: 0, dy: -1 },
            { x: 0, y: h / 2, xf: '0', yf: 'Height*0.5', dx: -1, dy: 0 }
        ];
        let xml = `
      <Section N="Connection">`;
        rows.forEach((r, i) => {
            xml += `
        <Row T="Connection" IX="${i}">
          <Cell N="X" V="${r.x}" F="${r.xf}"/>
          <Cell N="Y" V="${r.y}" F="${r.yf}"/>
          <Cell N="DirX" V="${r.dx}"/>
          <Cell N="DirY" V="${r.dy}"/>
        </Row>`;
        });
        xml += `
      </Section>`;
        return xml;
    }

    /** Closest of the four edge midpoints to a page-space Visio point. */
    _nearestConnection(shape, pagePt) {
        const box = this._shapeBox(shape);
        const cps = [
            { ix: 0, x: box.left + box.w / 2, y: box.bottom + box.h },
            { ix: 1, x: box.left + box.w, y: box.bottom + box.h / 2 },
            { ix: 2, x: box.left + box.w / 2, y: box.bottom },
            { ix: 3, x: box.left, y: box.bottom + box.h / 2 }
        ];
        let best = cps[0];
        let bestD = Infinity;
        for (const c of cps) {
            const d = (c.x - pagePt.x) ** 2 + (c.y - pagePt.y) ** 2;
            if (d < bestD) {
                bestD = d;
                best = c;
            }
        }
        return best;
    }

    _trackGlue(conn, id) {
        const begin = conn.points[0];
        const end = conn.points[conn.points.length - 1];
        const beginV = { x: this._svgToVisioX(begin.x), y: this._svgToVisioY(begin.y) };
        const endV = { x: this._svgToVisioX(end.x), y: this._svgToVisioY(end.y) };
        if (conn.fromShape !== null) {
            const cp = this._nearestConnection(this.data.shapes[conn.fromShape], beginV);
            this.connectorLinks.push({
                connectorId: id,
                cell: 'BeginX',
                fromPart: 9,
                targetId: this.shapeIds[conn.fromShape],
                toPart: 100 + cp.ix,
                toCell: 'Connections.X' + (cp.ix + 1)
            });
        }
        if (conn.toShape !== null) {
            const cp = this._nearestConnection(this.data.shapes[conn.toShape], endV);
            this.connectorLinks.push({
                connectorId: id,
                cell: 'EndX',
                fromPart: 12,
                targetId: this.shapeIds[conn.toShape],
                toPart: 100 + cp.ix,
                toCell: 'Connections.X' + (cp.ix + 1)
            });
        }
    }

    /** Group children by their parent index, with null for the page itself. */
    _addChild(map, parent, index) {
        const key = parent === undefined ? null : parent;
        if (!map.has(key)) map.set(key, []);
        map.get(key).push(index);
    }

    /** A shape's box in absolute Visio inches. */
    _shapeBox(shape) {
        return {
            w: shape.width * this.scale,
            h: shape.height * this.scale,
            left: this._svgToVisioX(shape.x),
            bottom: this._svgToVisioY(shape.y + shape.height)
        };
    }

    /**
     * A shape and everything the layout stage nested inside it.
     *
     * A frame with contents becomes a group, so dragging the frame takes its
     * contents along instead of sliding out from under them. A frame with
     * nothing inside it stays an ordinary shape - a group of one would only
     * make it harder to select.
     */
    _buildShapeTree(index, origin) {
        const shape = this.data.shapes[index];
        const kidShapes = this.childShapes.get(index) || [];
        const kidTexts = this.childTexts.get(index) || [];
        const kidConns = this.childConnectors.get(index) || [];

        if (!kidShapes.length && !kidTexts.length && !kidConns.length) {
            const xml = this._buildShape(shape, this.shapeIds[index], origin, '');
            return xml + (this._afterShapeXml || '');
        }

        // A group's children are placed in the group's own coordinate system,
        // whose origin is the group's lower-left corner.
        const box = this._shapeBox(shape);
        const inner = { x: box.left, y: box.bottom };
        let childrenXml = '';
        for (const i of kidShapes) childrenXml += this._buildShapeTree(i, inner);
        for (const i of kidTexts) {
            childrenXml += this._buildTextShape(this.data.texts[i], this.textIds[i], inner);
        }
        // Last, so a route draws over the boxes it runs between
        for (const i of kidConns) {
            childrenXml += this._buildConnector(this.data.connectors[i], this.connectorIds[i], inner);
        }

        return this._buildShape(shape, this.shapeIds[index], origin, childrenXml);
    }

    /**
     * The cells that make a group a Visio container rather than a plain group.
     *
     * A container moves its members with it and still lets a single click
     * select a member, instead of making you step into the group first. Visio
     * drives that from this user-defined cell; a Visio that does not act on it
     * falls back to ordinary group behaviour, which already fixes a frame
     * dragging out from under its contents. Nothing here changes the layout
     * either way.
     */
    _containerSection() {
        return `
      <Section N="User">
        <Row N="msvStructureType"><Cell N="Value" V="Container"/><Cell N="Prompt" V=""/></Row>
      </Section>`;
    }

    /**
     * @param {any} shape
     * @param {number} id
     * @param {{x: number, y: number}} origin the coordinate system to place it in
     * @param {string} childrenXml nested shapes, or '' for a leaf
     */
    _buildShape(shape, id, origin, childrenXml) {
        // Center position in Visio coordinates (inches, bottom-left origin)
        const box = this._shapeBox(shape);
        const w = box.w;
        const h = box.h;
        const pinX = box.left - origin.x + w / 2;
        const pinY = box.bottom - origin.y + h / 2;

        const fillColor = this._colorToRGB(shape.style.fill);
        const lineColor = this._colorToRGB(shape.style.stroke);
        const lineWeight = (shape.style.strokeWidth || 1) * this.scale;

        this._afterShapeXml = '';
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
      <Cell N="Angle" V="${shape.angle || 0}"/>
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
            const dashPattern = this._linePattern(shape.style.strokeDasharray);
            const lineTrans = this._transparency(shape.style.strokeOpacity, shape.style.opacity);
            cellsXml += `      <Cell N="LineWeight" V="${lineWeight}"/>
      <Cell N="LineColor" V="${lineColor}"/>
      <Cell N="LineColorTrans" V="${lineTrans}"/>
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

        // Perimeter glue points. Gluing to PinX (the centre) makes Visio
        // route the connector through the tile instead of to its edge.
        sectionsXml += this._connectionSection(w, h);

        // Visio cannot host a line feed or a <pp> in <Text> without drawing a
        // missing-glyph box and dropping every line after the first. One
        // source line stays on this shape; extra lines become child text
        // shapes, so the parent is a group.
        let childXml = childrenXml || '';
        if (shape.text) {
            const runs = this._flattenTextRuns(
                (shape.textRuns && shape.textRuns.length)
                    ? shape.textRuns
                    : [{ text: shape.text, style: shape.textStyle || {} }]
            );
            if (runs.length <= 1) {
                const text = this._textRunsXml(runs);
                cellsXml += this._textBlockCells(runs, w, h, shape.isContainer ? 0 : 1, true);
                sectionsXml += `
      <Section N="Character">${text.charRows}
      </Section>
      <Section N="Paragraph">${text.paraRows}
      </Section>`;
                textXml = `
      <Text>${text.body}</Text>`;
            } else if (childXml) {
                // Already a frame: title lines live in the group.
                childXml += this._lineTextShapes(runs, w, h, 0, 0);
            } else {
                // A labelled tile stays a Shape, not a group-of-text. The
                // extra lines sit next to it in the parent, so a click hits
                // the box or a line instead of a nested group.
                const ox = box.left - origin.x;
                const oy = box.bottom - origin.y;
                this._afterShapeXml = this._lineTextShapes(runs, w, h, ox, oy);
            }
        }

        if (childXml) {
            // DisplayMode 1 = draw the frame behind its members. Leaving the
            // cell unset makes Visio (and Aspose) show only the empty frame.
            cellsXml += `      <Cell N="DisplayMode" V="1"/>
      <Cell N="SelectMode" V="1"/>
      <Cell N="DontMoveChildren" V="1"/>
      <Cell N="IsTextEditTarget" V="0"/>
`;
            sectionsXml += this._containerSection();
            return `    <Shape ID="${id}" Type="Group" LineStyle="0" FillStyle="0" TextStyle="0">
${cellsXml}${sectionsXml}${textXml}
      <Shapes>
${childXml}      </Shapes>
    </Shape>
`;
        }

        return `    <Shape ID="${id}" Type="Shape" LineStyle="0" FillStyle="0" TextStyle="0">
${cellsXml}${sectionsXml}${textXml}
    </Shape>
`;
    }

    /**
    /**
     * A rectangle in relative coordinates, so it follows Width and Height.
     *
     * @param {number} w width in inches, for the cached corner values
     * @param {number} h
     * @param {number} rx corner radius in inches, 0 for square corners
     */
    _rectGeom(w, h, rx) {
        if (rx > 0) return this._roundedRectGeom(w, h, rx);

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

    /**
     * A rounded rectangle, with the corners drawn as arcs.
     *
     * The Rounding cell would express this in one line, but it is a Visio
     * refinement that other readers of the format ignore - libvisio, which
     * LibreOffice and every Linux viewer use, draws such a shape with square
     * corners - and a file whose rounded boxes are only round in Visio is not
     * worth the shorter geometry. The arcs are drawn from the Rounding cell,
     * so the radius stays editable in one place.
     *
     * The radius is a constant, because stretching a box should not fatten its
     * corners, and it is clamped through MIN so a box dragged narrower than
     * two radii still closes cleanly instead of turning inside out.
     *
     * A is the arc's bow: the distance from the arc's midpoint to the midpoint
     * of its chord. For a quarter circle of radius r that is r(1 - 1/sqrt 2),
     * and not the r(sqrt 2 - 1) this used to emit - a corner 41% too deep.
     */
    _roundedRectGeom(w, h, rx) {
        const r = Math.min(rx, w / 2, h / 2);
        const R = 'MIN(Rounding,Width/2,Height/2)';
        const BOW = 1 - Math.SQRT1_2;
        const cell = (name, value, formula) => this._geomCell(name, value, formula);
        const arc = (x, fx, y, fy) =>
            cell('X', x, fx) + cell('Y', y, fy) + cell('A', r * BOW, `${R}*${BOW}`);

        return `
      <Section N="Geometry" IX="0">
        <Cell N="NoFill" V="0"/>
        <Cell N="NoLine" V="0"/>
        <Row T="MoveTo" IX="1">${cell('X', 0, '0')}${cell('Y', r, R)}</Row>
        <Row T="ArcTo" IX="2">${arc(r, R, 0, '0')}</Row>
        <Row T="LineTo" IX="3">${cell('X', w - r, `Width-${R}`)}${cell('Y', 0, '0')}</Row>
        <Row T="ArcTo" IX="4">${arc(w, 'Width', r, R)}</Row>
        <Row T="LineTo" IX="5">${cell('X', w, 'Width')}${cell('Y', h - r, `Height-${R}`)}</Row>
        <Row T="ArcTo" IX="6">${arc(w - r, `Width-${R}`, h, 'Height')}</Row>
        <Row T="LineTo" IX="7">${cell('X', r, R)}${cell('Y', h, 'Height')}</Row>
        <Row T="ArcTo" IX="8">${arc(0, '0', h - r, `Height-${R}`)}</Row>
        <Row T="LineTo" IX="9">${cell('X', 0, '0')}${cell('Y', r, R)}</Row>
      </Section>`;
    }

    _ellipseGeom(w, h) {
        return `
      <Section N="Geometry" IX="0">
        <Cell N="NoFill" V="0"/>
        <Cell N="NoLine" V="0"/>
        <Row T="Ellipse" IX="1">
          ${this._geomCell('X', w / 2, 'Width*0.5')}${this._geomCell('Y', h / 2, 'Height*0.5')}
          ${this._geomCell('A', w, 'Width*1')}${this._geomCell('B', h / 2, 'Height*0.5')}
          ${this._geomCell('C', w / 2, 'Width*0.5')}${this._geomCell('D', h, 'Height*1')}
        </Row>
      </Section>`;
    }

    // The diamond is drawn with relative coordinates, so its size is implied
    _diamondGeom(_w, _h) {
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
        <Row T="MoveTo" IX="1">${this._relGeomCell('X', lx, w, 'Width')}${this._relGeomCell('Y', ly, h, 'Height')}</Row>`;
            } else {
                xml += `
        <Row T="LineTo" IX="${i + 1}">${this._relGeomCell('X', lx, w, 'Width')}${this._relGeomCell('Y', ly, h, 'Height')}</Row>`;
            }
        }

        // Close
        const lx0 = (pts[0].x - shape.x) * this.scale;
        const ly0 = h - (pts[0].y - shape.y) * this.scale;
        xml += `
        <Row T="LineTo" IX="${pts.length + 1}">${this._relGeomCell('X', lx0, w, 'Width')}${this._relGeomCell('Y', ly0, h, 'Height')}</Row>`;

        xml += `
      </Section>`;

        return xml;
    }

    /**
     * @param {any} text
     * @param {number} id
     * @param {{x: number, y: number}} origin the coordinate system to place it in
     */
    _buildTextShape(text, id, origin) {
        // Estimate text size from the widest line, not the whole string
        const fontSizeInches = this._fontSizeInches(text.style);
        const lines = text.text.split('\n');
        const runs = lines.map(line => ({ text: line, style: text.style }));
        const isBold = text.style.fontWeight === 'bold';
        const margin = this.textMargin;
        // Widest by measurement, not by character count: a short run of
        // capitals easily beats a longer lowercase one.
        const widest = lines.reduce((a, b) =>
            (this._lineWidthInches(b, fontSizeInches, isBold) >
             this._lineWidthInches(a, fontSizeInches, isBold) ? b : a), '');
        const estWidth = Math.max(
            this._lineWidthInches(widest, fontSizeInches, isBold) +
            2 * margin + fontSizeInches, 1);
        const estHeight = 2 * margin + Math.max(lines.length, 1) * fontSizeInches * 1.15;

        const pinX = this._svgToVisioX(text.x) - origin.x;
        const pinY = this._svgToVisioY(text.y) - origin.y;

        // In SVG, x is the anchor point and y is the baseline. Visio positions a
        // box by its local pin, so move the pin to match the anchor and lift the
        // box off the baseline - otherwise every label drifts left and down.
        const horzAlign = this._horzAlign(text.style);
        const locPinX = horzAlign === 1 ? estWidth / 2 : (horzAlign === 2 ? estWidth : 0);
        const locPinY = estHeight / 2 - 0.35 * fontSizeInches;

        const textXml = this._textRunsXml(runs);

        return `    <Shape ID="${id}" Type="Shape" LineStyle="0" FillStyle="0" TextStyle="0">
      <Cell N="PinX" V="${pinX}"/>
      <Cell N="PinY" V="${pinY}"/>
      <Cell N="Width" V="${estWidth}"/>
      <Cell N="Height" V="${estHeight}"/>
      <Cell N="LocPinX" V="${locPinX}"/>
      <Cell N="LocPinY" V="${locPinY}"/>
      <Cell N="Angle" V="0"/>
      <Cell N="FillPattern" V="0"/>
      <Cell N="LinePattern" V="0"/>
${this._textBlockCells(runs, estWidth, estHeight, 1, true)}      <Section N="Character">${textXml.charRows}
      </Section>
      <Section N="Paragraph">${textXml.paraRows}
      </Section>
      <Text>${textXml.body}</Text>
    </Shape>
`;
    }

    /**
     * A connector, as a Visio 1-D shape.
     *
     * Visio only treats a shape as a connector - and only honours the glue
     * recorded in <Connects> - when OneD is set. Without it the Begin/End
     * cells describe endpoints of a shape that has none, so the arrows sat on
     * the page unattached and stayed behind when a box moved.
     *
     * A 1-D shape lives in the frame its own endpoints define: local X runs
     * from the begin point to the end point, local Y is the perpendicular
     * offset. Elbows therefore survive as drawn.
     *
     * The transform cells are plain values. Visio derives a 1-D shape's
     * transform from its endpoints itself, so writing formulas that do the
     * same job by hand does not help - and Visio will not open a file that
     * has them, which is how this was found: the drawing came up with every
     * labelled box missing.
     *
     * A connector usually stays on the page: page-level glue reaches a shape
     * at any depth, and a route crossing a frame boundary belongs to neither
     * frame. One whose whole route and both ends are inside a single frame is
     * nested into it, so it travels with the frame instead of stretching.
     *
     * @param {any} conn
     * @param {number} id
     * @param {{x: number, y: number}} origin the coordinate system to place it in
     */
    _buildConnector(conn, id, origin) {
        const pts = conn.points;
        if (pts.length < 2) return '';

        // Visio coordinates, y growing upwards, before any local frame
        const vpts = pts.map(p => ({
            x: this._svgToVisioX(p.x) - origin.x,
            y: this._svgToVisioY(p.y) - origin.y
        }));
        // Pull an arrowed end off the box edge so the head is a triangle
        // on the line, not a blob sitting on the tile.
        const arrowIn = 0.12;
        const pull = (from, toward) => {
            const dx = toward.x - from.x;
            const dy = toward.y - from.y;
            const len = Math.hypot(dx, dy);
            if (len < arrowIn * 2) return from;
            return {
                x: from.x + dx / len * arrowIn,
                y: from.y + dy / len * arrowIn
            };
        };
        if (conn.arrowStart && vpts.length >= 2) vpts[0] = pull(vpts[0], vpts[1]);
        if (conn.arrowEnd && vpts.length >= 2) {
            vpts[vpts.length - 1] = pull(vpts[vpts.length - 1], vpts[vpts.length - 2]);
        }

        const begin = vpts[0];
        const end = vpts[vpts.length - 1];

        const spanX = end.x - begin.x;
        const spanY = end.y - begin.y;
        const length = Math.hypot(spanX, spanY);
        // A connector whose ends coincide has no direction to lay a local
        // frame along; keep it visible rather than dividing by zero.
        const w = Math.max(length, 0.01);
        const angle = length > 0 ? Math.atan2(spanY, spanX) : 0;
        const cos = Math.cos(angle);
        const sin = Math.sin(angle);

        const local = vpts.map(p => {
            const dx = p.x - begin.x;
            const dy = p.y - begin.y;
            return { x: dx * cos + dy * sin, y: -dx * sin + dy * cos };
        });

        // Height is the route's perpendicular extent, doubled so the line
        // itself keeps Visio's usual place at half the shape's height.
        // A straight connector with Height 0 cannot be selected or glued.
        const perp = local.reduce((m, p) => Math.max(m, Math.abs(p.y)), 0);
        const h = Math.max(2 * perp, 0.0625);

        const lineColor = this._colorToRGB(conn.style.stroke) || '#000000';
        const lineWeight = (conn.style.strokeWidth || 1) * this.scale;
        const dashPattern = this._linePattern(conn.style.strokeDasharray);
        const lineTrans = this._transparency(conn.style.strokeOpacity, conn.style.opacity);

        // Arrow: 5 = filled triangle
        // SceneModel guarantees both ends are present and boolean
        const beginArrow = conn.arrowStart ? '5' : '0';
        const endArrow = conn.arrowEnd ? '5' : '0';

        let geomXml = `
      <Section N="Geometry" IX="0">
        <Cell N="NoFill" V="1"/>
        <Cell N="NoLine" V="0"/>`;

        local.forEach((p, i) => {
            const ly = h / 2 + p.y;
            const row = i === 0 ? 'MoveTo' : 'LineTo';
            geomXml += `
        <Row T="${row}" IX="${i + 1}">${this._relGeomCell('X', p.x, w, 'Width')}${this._relGeomCell('Y', ly, h, 'Height')}</Row>`;
        });

        geomXml += `
      </Section>`;

        const fromId = conn.fromShape !== null ? this.shapeIds[conn.fromShape] : null;
        const toId = conn.toShape !== null ? this.shapeIds[conn.toShape] : null;
        let glueCells = `      <Cell N="ObjType" V="2"/>
      <Cell N="GlueType" V="2"/>
`;
        if (fromId !== null) {
            glueCells += `      <Cell N="BegTrigger" F="_XFTRIGGER(Sheet.${fromId}!EventXFMod)" V="${fromId}"/>
`;
        }
        if (toId !== null) {
            glueCells += `      <Cell N="EndTrigger" F="_XFTRIGGER(Sheet.${toId}!EventXFMod)" V="${toId}"/>
`;
        }

        // The XForm follows the endpoints, the way Visio's own connectors do,
        // so re-gluing an end moves and re-aims the whole shape.
        return `    <Shape ID="${id}" Type="Shape" LineStyle="0" FillStyle="0" TextStyle="0">
      <Cell N="OneD" V="1"/>
      <Cell N="BeginX" V="${begin.x}"/>
      <Cell N="BeginY" V="${begin.y}"/>
      <Cell N="EndX" V="${end.x}"/>
      <Cell N="EndY" V="${end.y}"/>
      <Cell N="PinX" V="${(begin.x + end.x) / 2}"/>
      <Cell N="PinY" V="${(begin.y + end.y) / 2}"/>
      <Cell N="Width" V="${w}"/>
      <Cell N="Height" V="${h}"/>
      <Cell N="LocPinX" V="${w / 2}"/>
      <Cell N="LocPinY" V="${h / 2}"/>
      <Cell N="Angle" V="${angle}"/>
      <Cell N="FillPattern" V="0"/>
      <Cell N="LineWeight" V="${lineWeight}"/>
      <Cell N="LineColor" V="${lineColor}"/>
      <Cell N="LinePattern" V="${dashPattern}"/>
      <Cell N="BeginArrow" V="${beginArrow}"/>
      <Cell N="EndArrow" V="${endArrow}"/>
      <Cell N="BeginArrowSize" V="3"/>
      <Cell N="EndArrowSize" V="3"/>
      <Cell N="LineColorTrans" V="${lineTrans}"/>
${glueCells}${geomXml}
    </Shape>
`;
    }
}
