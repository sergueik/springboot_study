// @ts-check
/**
 * Native LibreOffice Draw export (flat ODG / .fodg) from the scene model.
 *
 * VSDX import in Draw turns connectors into paths, so they do not glue.
 * Two-point tile edges become draw:connector (glue). Polylines become
 * draw:polyline — LibreOffice ignores svg:d on a glued connector and
 * reroutes around groups. draw:g keeps frames together.
 */

class OdgBuilder {
    /**
     * @param {any} scene
     */
    constructor(scene) {
        this.scene = scene;
        this.vb = scene.viewBox;
        this.pxToCm = 2.54 / 96;
        /** @type {string[]} */
        this.autoStyles = [];
        this.styleSeq = 0;
        /** @type {Map<string, string>} */
        this.styleCache = new Map();
        /** @type {boolean} */
        this.needDash = false;
        /** @type {boolean} */
        this.needArrow = false;
        /** @type {string[]} */
        this.shapeXmlId = [];
        /** @type {Map<number|null, number[]>} */
        this.childShapes = new Map();
        /** @type {Map<number|null, number[]>} */
        this.childTexts = new Map();
    }

    /** @returns {string} */
    build() {
        this.autoStyles = [];
        this.styleSeq = 0;
        this.styleCache = new Map();
        this.needDash = false;
        this.needArrow = false;
        this.shapeXmlId = this.scene.shapes.map((_, i) => `id_s${i}`);
        this.childShapes = new Map();
        this.childTexts = new Map();
        this.scene.shapes.forEach((s, i) => this._add(this.childShapes, s.parentShape, i));
        this.scene.texts.forEach((t, i) => this._add(this.childTexts, t.parentShape, i));

        const body = this._emitTree(null);

        const pageW = this._cm(this.vb.width);
        const pageH = this._cm(this.vb.height);
        const dashDef = this.needDash
            ? `<draw:stroke-dash draw:name="Dash" draw:style="rect" draw:dots1="1" draw:dots1-length="0.2cm" draw:dots2="1" draw:dots2-length="0.2cm" draw:distance="0.15cm"/>`
            : '';
        const arrowDef = this.needArrow
            ? `<draw:marker draw:name="Arrow" draw:display-name="Arrow" svg:viewBox="0 0 20 30" svg:d="M0 0 L20 15 L0 30 z"/>`
            : '';

        return `<?xml version="1.0" encoding="UTF-8"?>
<office:document
 xmlns:office="urn:oasis:names:tc:opendocument:xmlns:office:1.0"
 xmlns:style="urn:oasis:names:tc:opendocument:xmlns:style:1.0"
 xmlns:text="urn:oasis:names:tc:opendocument:xmlns:text:1.0"
 xmlns:draw="urn:oasis:names:tc:opendocument:xmlns:drawing:1.0"
 xmlns:fo="urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0"
 xmlns:svg="urn:oasis:names:tc:opendocument:xmlns:svg-compatible:1.0"
 xmlns:xlink="http://www.w3.org/1999/xlink"
 xmlns:dc="http://purl.org/dc/elements/1.1/"
 xmlns:meta="urn:oasis:names:tc:opendocument:xmlns:meta:1.0"
 office:version="1.3"
 office:mimetype="application/vnd.oasis.opendocument.graphics">
 <office:meta>
  <meta:generator>svgtovisio</meta:generator>
 </office:meta>
 <office:font-face-decls>
  <style:font-face style:name="Arial" svg:font-family="Arial" style:font-family-generic="swiss" style:font-pitch="variable"/>
 </office:font-face-decls>
 <office:styles>
  <draw:layer-set>
   <draw:layer draw:name="layout" draw:display="true" draw:protected="false"/>
   <draw:layer draw:name="background" draw:display="true" draw:protected="false"/>
   <draw:layer draw:name="controls" draw:display="true" draw:protected="false"/>
   <draw:layer draw:name="measurelines" draw:display="true" draw:protected="false"/>
  </draw:layer-set>
  ${dashDef}
  ${arrowDef}
  <style:style style:name="standard" style:family="graphic">
   <style:graphic-properties draw:stroke="solid" svg:stroke-color="#000000" draw:fill="solid" draw:fill-color="#ffffff"/>
  </style:style>
 </office:styles>
 <office:automatic-styles>
  <style:page-layout style:name="PM1">
   <style:page-layout-properties fo:page-width="${pageW}cm" fo:page-height="${pageH}cm" fo:margin-top="0cm" fo:margin-bottom="0cm" fo:margin-left="0cm" fo:margin-right="0cm"/>
  </style:page-layout>
  <style:style style:name="dp1" style:family="drawing-page">
   <style:drawing-page-properties draw:background-size="full" draw:fill="none"/>
  </style:style>
  ${this.autoStyles.join('\n  ')}
 </office:automatic-styles>
 <office:master-styles>
  <style:master-page style:name="Default" style:page-layout-name="PM1"/>
 </office:master-styles>
 <office:body>
  <office:drawing>
   <draw:page draw:name="page1" draw:style-name="dp1" draw:master-page-name="Default">
${body}
   </draw:page>
  </office:drawing>
 </office:body>
</office:document>
`;
    }

    /**
     * @param {Map<number|null, number[]>} map
     * @param {number|null} parent
     * @param {number} index
     */
    _add(map, parent, index) {
        const key = parent == null ? null : parent;
        if (!map.has(key)) map.set(key, []);
        map.get(key).push(index);
    }

    /**
     * @param {number|null} parentIndex
     * @returns {string}
     */
    _emitTree(parentIndex) {
        let xml = '';
        for (const i of (this.childShapes.get(parentIndex) || [])) {
            const shape = this.scene.shapes[i];
            const kids = this._emitTree(i) + this._emitTexts(i);
            if (shape.isContainer) {
                xml += `    <draw:g>\n${this._shape(shape, i)}${kids}    </draw:g>\n`;
            } else {
                xml += this._shape(shape, i) + kids;
            }
        }
        if (parentIndex == null) {
            xml += this._emitTexts(null);
            // Connectors stay on the page. svg:d is page-absolute; inside
            // draw:g LibreOffice treats it as group-local and drops the route.
            this.scene.connectors.forEach((c, i) => {
                xml += this._connector(c, i);
            });
        }
        return xml;
    }

    /**
     * @param {number|null} parentIndex
     * @returns {string}
     */
    _emitTexts(parentIndex) {
        let xml = '';
        for (const i of (this.childTexts.get(parentIndex) || [])) {
            xml += this._text(this.scene.texts[i], i);
        }
        return xml;
    }

    /**
     * @param {any} shape
     * @param {number} i
     * @returns {string}
     */
    _shape(shape, i) {
        const id = this.shapeXmlId[i];
        const style = this._graphic(shape.style, {
            arrowStart: false,
            arrowEnd: false,
            line: false,
            align: shape.isContainer ? 'left' : 'center',
            valign: shape.isContainer ? 'top' : 'middle',
            fitText: !shape.isContainer
        });
        const x = this._cm(shape.x - this.vb.x);
        const y = this._cm(shape.y - this.vb.y);
        const w = this._cm(Math.max(shape.width, 1));
        const h = this._cm(Math.max(shape.height, 1));
        const para = this._para(shape.textStyle || shape.style, shape.isContainer ? 'start' : 'center');
        const textXml = this._textContent(shape, para);
        const common = `draw:style-name="${style}" draw:id="${id}" xml:id="${id}" draw:layer="layout" svg:x="${x}cm" svg:y="${y}cm" svg:width="${w}cm" svg:height="${h}cm"`;

        if (shape.type === 'ellipse' || shape.type === 'circle') {
            return `    <draw:ellipse ${common}>${textXml}</draw:ellipse>\n`;
        }
        if (shape.type === 'diamond') {
            const pts = this._diamondPoints(w, h);
            return `    <draw:polygon ${common} svg:viewBox="0 0 ${this._hmm(shape.width)} ${this._hmm(shape.height)}" draw:points="${pts}">${textXml}</draw:polygon>\n`;
        }
        if ((shape.type === 'polygon' || shape.type === 'path-shape') && shape.points && shape.points.length >= 3) {
            const pts = shape.points.map(p =>
                `${this._hmm(p.x - shape.x)},${this._hmm(p.y - shape.y)}`
            ).join(' ');
            return `    <draw:polygon ${common} svg:viewBox="0 0 ${this._hmm(shape.width)} ${this._hmm(shape.height)}" draw:points="${pts}">${textXml}</draw:polygon>\n`;
        }
        const rx = shape.style && shape.style.rx ? shape.style.rx : 0;
        const corner = rx > 0 ? ` draw:corner-radius="${this._cm(rx)}cm"` : '';
        return `    <draw:rect ${common}${corner}>${textXml}</draw:rect>\n`;
    }

    /**
     * @param {any} shape
     * @param {string} para
     */
    _textContent(shape, para) {
        const runs = this._runs(shape.textRuns, shape.text);
        if (!runs.length) return '';
        return runs.map(r => {
            const p = r.style ? this._para(r.style, shape.isContainer ? 'start' : 'center') : para;
            return `<text:p text:style-name="${p}">${this._esc(r.text)}</text:p>`;
        }).join('');
    }

    /**
     * @param {any} text
     * @param {number} i
     * @returns {string}
     */
    _text(text, i) {
        const st = text.style || {};
        const fs = st.fontSize || 14;
        const raw = String(text.text || '');
        const lines = raw.split('\n');
        const widthPx = Math.max(24, [...raw].length * fs * 0.56 + 8);
        const heightPx = Math.max(fs * 1.3, lines.length * fs * 1.3);
        const anchor = st.textAnchor || 'start';
        let xPx = text.x - this.vb.x;
        if (anchor === 'middle') xPx -= widthPx / 2;
        else if (anchor === 'end') xPx -= widthPx;
        const yPx = text.y - this.vb.y - fs * 0.85;
        const style = this._graphic({
            fill: 'none', stroke: 'none', fillOpacity: 0, strokeOpacity: 0,
            strokeWidth: 0, opacity: 1, strokeDasharray: null
        }, { arrowStart: false, arrowEnd: false, line: false, align: 'left', valign: 'top', fitText: true });
        const para = this._para(st, anchor);
        const paras = lines.map(l => `<text:p text:style-name="${para}">${this._esc(l)}</text:p>`).join('');
        const id = `id_t${i}`;
        return `    <draw:rect draw:style-name="${style}" draw:id="${id}" xml:id="${id}" draw:layer="layout" svg:x="${this._cm(xPx)}cm" svg:y="${this._cm(yPx)}cm" svg:width="${this._cm(widthPx)}cm" svg:height="${this._cm(heightPx)}cm">${paras}</draw:rect>\n`;
    }

    /**
     * @param {any} conn
     * @param {number} i
     * @returns {string}
     */
    _connector(conn, i) {
        const pts = conn.points || [];
        if (pts.length < 2) return '';
        const style = this._graphic(conn.style, {
            arrowStart: !!conn.arrowStart,
            arrowEnd: !!conn.arrowEnd,
            line: true,
            align: 'center',
            valign: 'middle'
        });
        const id = `id_c${i}`;
        let label = '';
        if (conn.text) {
            const para = this._para(conn.style, 'center');
            label = `<text:p text:style-name="${para}">${this._esc(conn.text)}</text:p>`;
        }

        // LibreOffice drops svg:d on draw:connector as soon as the edge is
        // glued, then auto-routes around groups. A polyline keeps the SVG
        // route. Two-point tile-to-tile edges stay connectors so they glue.
        const poly = pts.length > 2;
        const fromBox = conn.fromShape != null && !this.scene.shapes[conn.fromShape].isContainer;
        const toBox = conn.toShape != null && !this.scene.shapes[conn.toShape].isContainer;
        const glue = !poly && fromBox && toBox;
        if (poly) return this._polyline(conn, style, id, label);

        const a = pts[0];
        const b = pts[pts.length - 1];
        const start = glue ? ` draw:start-shape="${this.shapeXmlId[conn.fromShape]}"` : '';
        const end = glue ? ` draw:end-shape="${this.shapeXmlId[conn.toShape]}"` : '';
        return `    <draw:connector draw:style-name="${style}" draw:id="${id}" xml:id="${id}" draw:layer="layout" draw:type="line"${start}${end} svg:x1="${this._cm(a.x - this.vb.x)}cm" svg:y1="${this._cm(a.y - this.vb.y)}cm" svg:x2="${this._cm(b.x - this.vb.x)}cm" svg:y2="${this._cm(b.y - this.vb.y)}cm">${label}</draw:connector>\n`;
    }

    /**
     * @param {any} conn
     * @param {string} style
     * @param {string} id
     * @param {string} label
     */
    _polyline(conn, style, id, label) {
        const pts = conn.points;
        const xs = pts.map((p) => p.x - this.vb.x);
        const ys = pts.map((p) => p.y - this.vb.y);
        const minX = Math.min.apply(null, xs);
        const minY = Math.min.apply(null, ys);
        const w = Math.max(Math.max.apply(null, xs) - minX, 1);
        const h = Math.max(Math.max.apply(null, ys) - minY, 1);
        const vbW = Math.max(this._hmmInt(w), 1);
        const vbH = Math.max(this._hmmInt(h), 1);
        const points = pts.map((p) => {
            const x = this._hmmInt((p.x - this.vb.x) - minX);
            const y = this._hmmInt((p.y - this.vb.y) - minY);
            return `${x},${y}`;
        }).join(' ');
        return `    <draw:polyline draw:style-name="${style}" draw:id="${id}" xml:id="${id}" draw:layer="layout" svg:x="${this._cm(minX)}cm" svg:y="${this._cm(minY)}cm" svg:width="${this._cm(w)}cm" svg:height="${this._cm(h)}cm" svg:viewBox="0 0 ${vbW} ${vbH}" draw:points="${points}">${label}</draw:polyline>\n`;
    }

    /**
     * @param {any} st
     * @param {{arrowStart: boolean, arrowEnd: boolean, line: boolean, align?: string, valign?: string, fitText?: boolean}} opt
     * @returns {string}
     */
    _graphic(st, opt) {
        const fill = this._hex(st && st.fill);
        const stroke = this._hex(st && st.stroke);
        const sw = st && st.strokeWidth != null ? st.strokeWidth : 1;
        const fo = st ? (st.fillOpacity == null ? 1 : st.fillOpacity) * (st.opacity == null ? 1 : st.opacity) : 1;
        const so = st ? (st.strokeOpacity == null ? 1 : st.strokeOpacity) * (st.opacity == null ? 1 : st.opacity) : 1;
        const dash = !!(st && st.strokeDasharray);
        const align = opt.align || 'center';
        const valign = opt.valign || 'middle';
        const fitText = !!opt.fitText;
        const key = [fill, stroke, sw, fo, so, dash, opt.arrowStart, opt.arrowEnd, opt.line, align, valign, fitText].join('|');
        if (this.styleCache.has(key)) return /** @type {string} */ (this.styleCache.get(key));
        const name = 'gr' + (++this.styleSeq);
        if (dash) this.needDash = true;
        if (opt.arrowStart || opt.arrowEnd) this.needArrow = true;
        const fillAttr = (!opt.line && fill)
            ? `draw:fill="solid" draw:fill-color="${fill}" draw:opacity="${this._n(fo * 100)}%"`
            : 'draw:fill="none"';
        const strokeAttr = stroke
            ? `draw:stroke="${dash ? 'dash' : 'solid'}" svg:stroke-color="${stroke}" svg:stroke-width="${this._cm(Math.max(sw, 0.4))}cm" svg:stroke-opacity="${this._n(so)}"`
            : 'draw:stroke="none"';
        const dashAttr = dash ? ' draw:stroke-dash="Dash"' : '';
        const mw = this._cm(6);
        const startM = opt.arrowStart ? ` draw:marker-start="Arrow" draw:marker-start-width="${mw}cm"` : '';
        const endM = opt.arrowEnd ? ` draw:marker-end="Arrow" draw:marker-end-width="${mw}cm"` : '';
        const fit = fitText ? ' draw:fit-to-size="true" style:shrink-to-fit="true"' : '';
        this.autoStyles.push(
            `<style:style style:name="${name}" style:family="graphic" style:parent-style-name="standard">` +
            `<style:graphic-properties ${fillAttr} ${strokeAttr}${dashAttr}${startM}${endM} draw:textarea-horizontal-align="${align}" draw:textarea-vertical-align="${valign}" draw:auto-grow-width="false" draw:auto-grow-height="false"${fit} fo:padding-top="0.04cm" fo:padding-bottom="0.04cm" fo:padding-left="0.06cm" fo:padding-right="0.06cm"/>` +
            `</style:style>`
        );
        this.styleCache.set(key, name);
        return name;
    }

    /**
     * @param {any} st
     * @param {string} anchor
     * @returns {string}
     */
    _para(st, anchor) {
        const fs = (st && st.fontSize) || 14;
        const color = this._hex(st && (st.textColor || st.fill)) || '#000000';
        const weight = st && st.fontWeight;
        const bold = weight === 'bold' || Number(weight) >= 700;
        const align = anchor === 'middle' ? 'center' : anchor === 'end' ? 'end' : 'start';
        const key = `p|${fs}|${color}|${bold}|${align}`;
        if (this.styleCache.has(key)) return /** @type {string} */ (this.styleCache.get(key));
        const name = 'P' + (++this.styleSeq);
        const pt = this._n(fs * 72 / 96);
        this.autoStyles.push(
            `<style:style style:name="${name}" style:family="paragraph">` +
            `<style:paragraph-properties fo:text-align="${align}"/>` +
            `<style:text-properties fo:color="${color}" fo:font-size="${pt}pt" fo:font-weight="${bold ? 'bold' : 'normal'}" style:font-name="Arial"/>` +
            `</style:style>`
        );
        this.styleCache.set(key, name);
        return name;
    }

    /**
     * @param {number} wcm
     * @param {number} hcm
     */
    _diamondPoints(wcm, hcm) {
        const w = this._hmmFromCm(wcm);
        const h = this._hmmFromCm(hcm);
        return `${w / 2},0 ${w},${h / 2} ${w / 2},${h} 0,${h / 2}`;
    }

    /**
     * @param {any[]|undefined} runs
     * @param {string|null} text
     */
    _runs(runs, text) {
        const src = (runs && runs.length) ? runs : (text ? [{ text, style: null }] : []);
        const out = [];
        for (const run of src) {
            for (const line of String(run.text || '').split('\n')) out.push({ text: line, style: run.style });
        }
        return out.filter(r => r.text !== '' || out.length === 1);
    }

    /** px → cm, 2 decimals */
    _cm(px) {
        return this._n(px * this.pxToCm);
    }

    /** px → 1/100 mm (ODF polygon / connector path space) */
    _hmm(px) {
        return this._n(px * 25.4 / 96 * 100);
    }

    _hmmInt(px) {
        return Math.round(px * 25.4 / 96 * 100);
    }

    _hmmFromCm(cm) {
        return this._n(cm * 1000);
    }

    /** @param {string} [color] */
    _hex(color) {
        if (!color || color === 'none' || color === 'transparent') return null;
        if (/^url\(/i.test(String(color).trim())) return null;
        const named = {
            white: '#FFFFFF', black: '#000000', red: '#FF0000', green: '#008000',
            blue: '#0000FF', yellow: '#FFFF00', orange: '#FFA500', gray: '#808080',
            grey: '#808080', silver: '#C0C0C0', navy: '#000080', teal: '#008080'
        };
        const lower = String(color).toLowerCase().trim();
        if (named[lower]) return named[lower];
        const rgb = String(color).match(/rgba?\(\s*(\d+)\s*,\s*(\d+)\s*,\s*(\d+)/);
        if (rgb) {
            const h = (n) => Number(n).toString(16).padStart(2, '0');
            return ('#' + h(rgb[1]) + h(rgb[2]) + h(rgb[3])).toUpperCase();
        }
        if (color.startsWith('#') && color.length === 4) {
            return ('#' + color[1] + color[1] + color[2] + color[2] + color[3] + color[3]).toUpperCase();
        }
        if (color.startsWith('#') && color.length >= 7) return color.slice(0, 7).toUpperCase();
        return '#000000';
    }

    /** @param {number} n */
    _n(n) {
        if (!isFinite(n)) return 0;
        return Math.round(n * 100) / 100;
    }

    /** @param {string} s */
    _esc(s) {
        return String(s)
            .replace(/&/g, '&amp;')
            .replace(/</g, '&lt;')
            .replace(/>/g, '&gt;');
    }
}
