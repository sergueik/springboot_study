// @ts-check
/**
 * Native draw.io (.drawio / mxGraph) export from the scene model.
 *
 * VSDX import in draw.io already glues edges, but it drops polyline
 * waypoints (edgeStyle=none, straight diagonals). Writing mxGraph ourselves
 * keeps the route, puts every label inside its tile, and uses real groups.
 */

class DrawioBuilder {
    /**
     * @param {any} scene
     */
    constructor(scene) {
        this.scene = scene;
        this.vb = scene.viewBox;
        this.nextId = 2;
        /** @type {number[]} */
        this.shapeCell = [];
        /** @type {number[]} */
        this.textCell = [];
        /** @type {number[]} */
        this.connectorCell = [];
        /** @type {Map<number|null, number[]>} */
        this.childShapes = new Map();
        /** @type {Map<number|null, number[]>} */
        this.childTexts = new Map();
    }

    /** @returns {string} */
    build() {
        this.nextId = 2;
        this.shapeCell = this.scene.shapes.map(() => this._id());
        this.textCell = this.scene.texts.map(() => this._id());
        this.connectorCell = this.scene.connectors.map(() => this._id());
        this.childShapes = new Map();
        this.childTexts = new Map();
        this.scene.shapes.forEach((s, i) => this._add(this.childShapes, s.parentShape, i));
        this.scene.texts.forEach((t, i) => this._add(this.childTexts, t.parentShape, i));

        const cells = ['<mxCell id="0"/>', '<mxCell id="1" parent="0"/>'];
        this._emitTree(null, '1', { x: this.vb.x, y: this.vb.y }, cells);
        // Edges live on the page. Nested in a group, glue double-moves when
        // the frame is dragged; mxGraph expects source/target on the default
        // parent and waypoints in page coordinates.
        const pageOrigin = { x: this.vb.x, y: this.vb.y };
        this.scene.connectors.forEach((_, i) => this._emitConnector(i, '1', pageOrigin, cells));

        const pw = Math.max(1, Math.round(this.vb.width));
        const ph = Math.max(1, Math.round(this.vb.height));
        return `<?xml version="1.0" encoding="UTF-8"?>
<mxfile host="svgtovisio" type="device" version="21">
  <diagram id="page1" name="Page-1">
    <mxGraphModel dx="0" dy="0" grid="1" gridSize="10" guides="1" tooltips="1" connect="1" arrows="1" fold="1" page="1" pageScale="1" pageWidth="${pw}" pageHeight="${ph}" math="0" shadow="0">
      <root>
        ${cells.join('\n        ')}
      </root>
    </mxGraphModel>
  </diagram>
</mxfile>
`;
    }

    _id() {
        return this.nextId++;
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
     * @param {string} parentId
     * @param {{x: number, y: number}} origin
     * @param {string[]} cells
     */
    _emitTree(parentIndex, parentId, origin, cells) {
        for (const i of (this.childShapes.get(parentIndex) || [])) {
            const shape = this.scene.shapes[i];
            this._emitShape(i, parentId, origin, cells);
            if (shape.isContainer) {
                this._emitTree(i, String(this.shapeCell[i]), { x: shape.x, y: shape.y }, cells);
            }
        }
        for (const i of (this.childTexts.get(parentIndex) || [])) {
            this._emitText(i, parentId, origin, cells);
        }
    }

    /**
     * @param {number} i
     * @param {string} parentId
     * @param {{x: number, y: number}} origin
     * @param {string[]} cells
     */
    _emitShape(i, parentId, origin, cells) {
        const shape = this.scene.shapes[i];
        const id = this.shapeCell[i];
        const x = this._n(shape.x - origin.x);
        const y = this._n(shape.y - origin.y);
        const w = this._n(Math.max(shape.width, 1));
        const h = this._n(Math.max(shape.height, 1));
        const style = this._shapeStyle(shape);
        const value = this._shapeValue(shape);
        cells.push(
            `<mxCell id="${id}" parent="${parentId}" vertex="1" value="${this._html(value)}" style="${style}">` +
            `<mxGeometry x="${x}" y="${y}" width="${w}" height="${h}" as="geometry"/>` +
            `</mxCell>`
        );
    }

    /**
     * @param {any} shape
     * @returns {string}
     */
    _shapeStyle(shape) {
        const st = shape.style || {};
        const parts = ['html=1', 'whiteSpace=wrap', 'overflow=width'];
        if (shape.isContainer) {
            parts.push('container=1', 'collapsible=0', 'recursiveResize=0', 'expand=0');
        }
        if (shape.type === 'ellipse' || shape.type === 'circle') parts.push('ellipse');
        else if (shape.type === 'diamond') parts.push('rhombus');
        else if (shape.type === 'polygon' || shape.type === 'path-shape') {
            const pts = this._relPoly(shape);
            if (pts) parts.push('shape=mxgraph.basic.polygon', `polyCoords=${pts}`);
        }
        const rx = st.rx || 0;
        if (rx > 0 && shape.type === 'rect') {
            parts.push('rounded=1');
            const min = Math.min(shape.width, shape.height) || 1;
            parts.push(`arcSize=${this._n(Math.min(50, (rx / min) * 100))}`);
        }
        if (shape.angle) {
            parts.push(`rotation=${this._n(-shape.angle * 180 / Math.PI)}`);
        }
        this._paint(parts, st, false);
        const ts = shape.textStyle || st;
        this._font(parts, ts, shape.isContainer ? 'left' : 'center',
            shape.isContainer ? 'top' : 'middle');
        return parts.join(';') + ';';
    }

    /**
     * @param {any} shape
     * @returns {string}
     */
    _shapeValue(shape) {
        const runs = this._runs(shape.textRuns, shape.text);
        if (!runs.length) return '';
        return runs.map(r => r.text).join('\n');
    }

    /**
     * @param {any} shape
     * @returns {string|null}
     */
    _relPoly(shape) {
        const pts = shape.points;
        if (!pts || pts.length < 3) return null;
        const w = shape.width || 1;
        const h = shape.height || 1;
        const coords = pts.map(p => {
            const x = this._n((p.x - shape.x) / w);
            const y = this._n((p.y - shape.y) / h);
            return `[${x},${y}]`;
        });
        return `[${coords.join(',')}]`;
    }

    /**
     * @param {number} i
     * @param {string} parentId
     * @param {{x: number, y: number}} origin
     * @param {string[]} cells
     */
    _emitText(i, parentId, origin, cells) {
        const text = this.scene.texts[i];
        const id = this.textCell[i];
        const st = text.style || {};
        const fs = st.fontSize || 14;
        const raw = String(text.text || '');
        const lines = raw.split('\n');
        const width = Math.max(24, this._textWidth(raw, fs, st.fontWeight) + 8);
        const height = Math.max(fs * 1.25, lines.length * fs * 1.25);
        const anchor = st.textAnchor || 'start';
        let x = text.x - origin.x;
        const y = text.y - origin.y - fs * 0.85;
        if (anchor === 'middle') x -= width / 2;
        else if (anchor === 'end') x -= width;
        const align = anchor === 'middle' ? 'center' : anchor === 'end' ? 'right' : 'left';
        const parts = ['text', 'html=1', 'whiteSpace=wrap', 'strokeColor=none',
            'fillColor=none', 'overflow=width', 'verticalAlign=bottom'];
        this._font(parts, st, align, 'bottom');
        cells.push(
            `<mxCell id="${id}" parent="${parentId}" vertex="1" value="${this._html(raw)}" style="${parts.join(';')};">` +
            `<mxGeometry x="${this._n(x)}" y="${this._n(y)}" width="${this._n(width)}" height="${this._n(height)}" as="geometry"/>` +
            `</mxCell>`
        );
    }

    /**
     * @param {number} i
     * @param {string} parentId
     * @param {{x: number, y: number}} origin
     * @param {string[]} cells
     */
    _emitConnector(i, parentId, origin, cells) {
        const conn = this.scene.connectors[i];
        const id = this.connectorCell[i];
        const pts = conn.points || [];
        if (pts.length < 2) return;
        const st = conn.style || {};
        const parts = ['html=1', 'edgeStyle=none', 'curved=0', 'rounded=0',
            'endCap=flat', 'startCap=flat'];
        parts.push(`endArrow=${conn.arrowEnd ? 'classic' : 'none'}`);
        parts.push(`startArrow=${conn.arrowStart ? 'classic' : 'none'}`);
        if (conn.arrowEnd || conn.arrowStart) {
            parts.push('endFill=1', 'startFill=1', 'endSize=6', 'startSize=6');
        }
        this._paint(parts, st, true);
        const src = conn.fromShape != null ? this.shapeCell[conn.fromShape] : null;
        const tgt = conn.toShape != null ? this.shapeCell[conn.toShape] : null;
        if (src != null && conn.fromShape != null) {
            const ex = this._port(this.scene.shapes[conn.fromShape], pts[0]);
            parts.push(`exitX=${ex.x}`, `exitY=${ex.y}`, 'exitPerimeter=1');
        }
        if (tgt != null && conn.toShape != null) {
            const en = this._port(this.scene.shapes[conn.toShape], pts[pts.length - 1]);
            parts.push(`entryX=${en.x}`, `entryY=${en.y}`, 'entryPerimeter=1');
        }
        const srcAttr = src != null ? ` source="${src}"` : '';
        const tgtAttr = tgt != null ? ` target="${tgt}"` : '';
        const mids = pts.slice(1, -1);
        let geoInner = '';
        if (mids.length) {
            geoInner = `<Array as="points">${mids.map(p =>
                `<mxPoint x="${this._n(p.x - origin.x)}" y="${this._n(p.y - origin.y)}"/>`
            ).join('')}</Array>`;
        }
        if (src == null || tgt == null) {
            const a = pts[0];
            const b = pts[pts.length - 1];
            geoInner +=
                `<mxPoint x="${this._n(a.x - origin.x)}" y="${this._n(a.y - origin.y)}" as="sourcePoint"/>` +
                `<mxPoint x="${this._n(b.x - origin.x)}" y="${this._n(b.y - origin.y)}" as="targetPoint"/>`;
        }
        const value = conn.text ? this._html(conn.text) : '';
        cells.push(
            `<mxCell id="${id}" parent="${parentId}" edge="1"${srcAttr}${tgtAttr} value="${value}" style="${parts.join(';')};">` +
            `<mxGeometry relative="1" as="geometry">${geoInner}</mxGeometry>` +
            `</mxCell>`
        );
    }

    /**
     * @param {any} shape
     * @param {{x: number, y: number}} pt
     */
    _port(shape, pt) {
        const w = shape.width || 1;
        const h = shape.height || 1;
        const x = (pt.x - shape.x) / w;
        const y = (pt.y - shape.y) / h;
        return {
            x: this._n(Math.max(0, Math.min(1, x))),
            y: this._n(Math.max(0, Math.min(1, y)))
        };
    }

    /**
     * @param {string[]} parts
     * @param {any} st
     * @param {boolean} lineOnly
     */
    _paint(parts, st, lineOnly) {
        const stroke = this._hex(st.stroke);
        const fill = this._hex(st.fill);
        const sw = st.strokeWidth == null ? 1 : st.strokeWidth;
        if (!lineOnly) {
            parts.push(fill ? `fillColor=${fill}` : 'fillColor=none');
            const fo = (st.fillOpacity == null ? 1 : st.fillOpacity) * (st.opacity == null ? 1 : st.opacity);
            if (fo < 0.999) parts.push(`fillOpacity=${this._n(fo * 100)}`);
        }
        parts.push(stroke ? `strokeColor=${stroke}` : 'strokeColor=none');
        if (sw !== 1) parts.push(`strokeWidth=${this._n(sw)}`);
        const so = (st.strokeOpacity == null ? 1 : st.strokeOpacity) * (st.opacity == null ? 1 : st.opacity);
        if (so < 0.999) parts.push(`strokeOpacity=${this._n(so * 100)}`);
        if (st.strokeDasharray) {
            parts.push('dashed=1');
            const nums = String(st.strokeDasharray).split(/[\s,]+/).map(Number).filter(n => n > 0);
            if (nums.length) parts.push(`dashPattern=${nums.map(n => this._n(n)).join(' ')}`);
        }
    }

    /**
     * @param {string[]} parts
     * @param {any} st
     * @param {string} align
     * @param {string} valign
     */
    _font(parts, st, align, valign) {
        const fs = st.fontSize || 14;
        const color = this._hex(st.textColor || st.fill) || '#000000';
        parts.push(`fontSize=${this._n(fs)}`, `fontColor=${color}`, `align=${align}`, `verticalAlign=${valign}`);
        const weight = st.fontWeight;
        if (weight === 'bold' || Number(weight) >= 700) parts.push('fontStyle=1');
        const family = this._face(st.fontFamily);
        if (family) parts.push(`fontFamily=${family}`);
        parts.push('spacing=2');
    }

    /**
     * @param {any[]|undefined} runs
     * @param {string|null} text
     */
    _runs(runs, text) {
        const src = (runs && runs.length) ? runs : (text ? [{ text, style: null }] : []);
        const out = [];
        for (const run of src) {
            for (const line of String(run.text || '').split('\n')) {
                if (line === '' && out.length === 0) continue;
                out.push({ text: line, style: run.style });
            }
        }
        return out;
    }

    /**
     * @param {string} text
     * @param {number} fs
     * @param {string|number} [weight]
     */
    _textWidth(text, fs, weight) {
        const bold = weight === 'bold' || Number(weight) >= 700;
        return [...String(text)].length * fs * (bold ? 0.62 : 0.56);
    }

    /** @param {string} [stack] */
    _face(stack) {
        if (!stack) return 'Arial';
        const first = String(stack).split(',')[0].trim().replace(/^["']|["']$/g, '');
        if (!first || first === 'sans-serif' || first === 'system-ui') return 'Arial';
        return first;
    }

    /** @param {string} [color] */
    _hex(color) {
        if (!color || color === 'none' || color === 'transparent') return null;
        if (/^url\(/i.test(color.trim())) return null;
        const named = {
            white: '#FFFFFF', black: '#000000', red: '#FF0000', green: '#008000',
            blue: '#0000FF', yellow: '#FFFF00', orange: '#FFA500', gray: '#808080',
            grey: '#808080', silver: '#C0C0C0', navy: '#000080', teal: '#008080'
        };
        const lower = color.toLowerCase().trim();
        if (named[lower]) return named[lower];
        const rgb = color.match(/rgba?\(\s*(\d+)\s*,\s*(\d+)\s*,\s*(\d+)/);
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
            .replace(/>/g, '&gt;')
            .replace(/"/g, '&quot;');
    }

    /**
     * html=1 values: each source line is escaped text, joined with &lt;br/&gt;.
     * A raw newline in an attribute is legal XML but draw.io treats &lt;br/&gt;
     * as the line break.
     * @param {string} s
     */
    _html(s) {
        return String(s).split('\n').map((line) => this._esc(line)).join('&lt;br/&gt;');
    }
}
