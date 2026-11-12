// @ts-check
/* global SceneModel, SceneLayout, SvgTransform, SvgStyleResolver */
/**
 * SVG Parser - Extracts shapes, connectors, and text from Claude-generated SVGs.
 *
 * Claude typically generates SVGs with:
 * - <rect> for boxes/cards
 * - <circle>, <ellipse> for circles
 * - <line>, <polyline>, <path> for arrows/connectors
 * - <polygon> for diamonds/triangles
 * - <text> for labels
 * - <marker> definitions for arrowheads
 * - <g> groups combining shapes with their labels
 */

class SvgParser {
    constructor(svgString) {
        this.svgString = svgString;
        this.parser = new DOMParser();
        this.doc = null;
        this.svgEl = null;
        this.viewBox = { x: 0, y: 0, width: 800, height: 600 };
        this.shapes = [];
        this.connectors = [];
        this.texts = [];
        this.defs = {};
        this.idMap = {};
        this.warnings = [];
        this._warned = new Set();
        /** @type {SvgStyleResolver} */
        this.style = null;
    }

    /** Combine a parent matrix with a child transform attribute. */
    _combine(parent, child) {
        return SvgTransform.combine(parent, child, (msg) => this._warn(msg));
    }

    /** Record a conversion caveat once, so the UI can surface what was dropped. */
    _warn(message) {
        if (this._warned.has(message)) return;
        this._warned.add(message);
        this.warnings.push(message);
    }

    parse() {
        this.doc = this.parser.parseFromString(this.svgString, 'image/svg+xml');
        const errorNode = this.doc.querySelector('parsererror');
        if (errorNode) {
            throw new Error('Invalid SVG: ' + errorNode.textContent.slice(0, 200));
        }

        this.svgEl = this.doc.querySelector('svg');
        if (!this.svgEl) {
            throw new Error('No <svg> element found');
        }

        this._parseViewBox();
        this.style = new SvgStyleResolver(this.svgEl, (msg) => this._warn(msg));
        this._parseDefs();
        this._parseElements(this.svgEl, null);

        const scene = SceneModel.normalise({
            warnings: this.warnings,
            viewBox: this.viewBox,
            shapes: this.shapes,
            connectors: this.connectors,
            texts: this.texts
        });

        // Container detection, label assignment and connector gluing are not
        // SVG-specific, so they run on the scene for every input format.
        return SceneLayout.apply(scene);
    }

    _parseViewBox() {
        const vb = this.svgEl.getAttribute('viewBox');
        if (vb) {
            const parts = vb.split(/[\s,]+/).map(Number);
            if (parts.length === 4) {
                this.viewBox = { x: parts[0], y: parts[1], width: parts[2], height: parts[3] };
            }
        } else {
            const w = this.svgEl.getAttribute('width');
            const h = this.svgEl.getAttribute('height');
            if (w && h) {
                this.viewBox.width = parseFloat(w);
                this.viewBox.height = parseFloat(h);
            }
        }
    }

    _parseDefs() {
        // Markers (arrowheads) - only their existence matters for conversion
        this.svgEl.querySelectorAll('marker').forEach(marker => {
            if (marker.id) this.defs[marker.id] = { type: 'marker', id: marker.id };
        });

        // Index every id once so <use> can resolve references cheaply
        this.svgEl.querySelectorAll('[id]').forEach(el => {
            const id = el.getAttribute('id');
            if (id && !this.idMap[id]) this.idMap[id] = el;
        });
    }


    _parseElements(parent, groupTransform, depth) {
        depth = depth || 0;
        for (const el of parent.children) {
            const tag = el.tagName.toLowerCase();

            if (tag === 'defs' || tag === 'style' || tag === 'title' || tag === 'desc' ||
                tag === 'metadata' || tag === 'marker' || tag === 'lineargradient' ||
                tag === 'radialgradient' || tag === 'pattern' || tag === 'clippath' ||
                tag === 'mask' || tag === 'filter' || tag === 'symbol' || tag === 'script') {
                continue;
            }

            if (tag === 'g' || tag === 'a' || tag === 'switch') {
                if (tag === 'switch') this._warn('<switch> was flattened - every branch was converted');
                const transform = this._combine(groupTransform, el.getAttribute('transform'));
                this._parseElements(el, transform, depth);
                continue;
            }

            if (tag === 'use') {
                this._parseUse(el, groupTransform, depth);
                continue;
            }

            if (el.getAttribute('clip-path') || el.getAttribute('mask') || el.getAttribute('filter')) {
                this._warn('clip-path / mask / filter are ignored - shapes are converted unclipped');
            }

            const transform = this._combine(groupTransform, el.getAttribute('transform'));
            const style = SvgTransform.scaleStyle(this.style.resolve(el), transform);
            this._dispatchElement(tag, el, style, transform);
        }
    }

    _dispatchElement(tag, el, style, transform) {
        switch (tag) {
            case 'rect':
                this._parseRect(el, style, transform);
                break;
            case 'circle':
                this._parseCircle(el, style, transform);
                break;
            case 'ellipse':
                this._parseEllipse(el, style, transform);
                break;
            case 'polygon':
                this._parsePolygon(el, style, transform);
                break;
            case 'line':
                this._parseLine(el, style, transform);
                break;
            case 'polyline':
                this._parsePolyline(el, style, transform);
                break;
            case 'path':
                this._parsePath(el, style, transform);
                break;
            case 'text':
                this._parseText(el, style, transform);
                break;
            case 'image':
                this._warn('<image> was skipped - embedded bitmaps are not converted');
                break;
            case 'foreignobject':
                this._warn('<foreignObject> was skipped - embedded HTML is not converted');
                break;
            default:
                this._warn(`<${tag}> is not supported and was skipped`);
        }
    }

    /** Expand <use> by converting the element it references. */
    _parseUse(el, groupTransform, depth) {
        if (depth >= 8) {
            this._warn('<use> references nest too deeply and were not expanded');
            return;
        }

        const href = el.getAttribute('href') || el.getAttribute('xlink:href');
        if (!href || href[0] !== '#') {
            this._warn('<use> without a local #reference was skipped');
            return;
        }

        const target = this.idMap[href.slice(1)];
        if (!target) {
            this._warn(`<use> references "${href}", which does not exist in this file`);
            return;
        }

        let transform = this._combine(groupTransform, el.getAttribute('transform'));
        const x = parseFloat(el.getAttribute('x') || 0);
        const y = parseFloat(el.getAttribute('y') || 0);
        if (x || y) {
            transform = this._combine(transform, { a: 1, b: 0, c: 0, d: 1, e: x, f: y });
        }

        const tag = target.tagName.toLowerCase();
        if (tag === 'g' || tag === 'symbol' || tag === 'svg') {
            const inner = this._combine(transform, target.getAttribute('transform'));
            this._parseElements(target, inner, depth + 1);
            return;
        }

        const inner = this._combine(transform, target.getAttribute('transform'));
        const style = SvgTransform.scaleStyle(this.style.resolve(target), inner);
        this._dispatchElement(tag, target, style, inner);
    }


    _parseRect(el, style, transform) {
        const x = parseFloat(el.getAttribute('x') || 0);
        const y = parseFloat(el.getAttribute('y') || 0);
        const w = parseFloat(el.getAttribute('width') || 0);
        const h = parseFloat(el.getAttribute('height') || 0);

        if (w === 0 || h === 0) return;

        const box = SvgTransform.rect(x, y, w, h, transform);

        this.shapes.push({
            type: 'rect',
            x: box.x,
            y: box.y,
            width: box.width,
            height: box.height,
            angle: box.angle,
            style,
            id: el.id || null,
            text: null
        });
    }

    _parseCircle(el, style, transform) {
        const cx = parseFloat(el.getAttribute('cx') || 0);
        const cy = parseFloat(el.getAttribute('cy') || 0);
        const r = parseFloat(el.getAttribute('r') || 0);

        if (r === 0) return;

        const box = SvgTransform.rect(cx - r, cy - r, r * 2, r * 2, transform);

        this.shapes.push({
            type: 'circle',
            x: box.x,
            y: box.y,
            width: box.width,
            height: box.height,
            angle: box.angle,
            cx: box.x + box.width / 2,
            cy: box.y + box.height / 2,
            r: box.width / 2,
            style,
            id: el.id || null,
            text: null
        });
    }

    _parseEllipse(el, style, transform) {
        const cx = parseFloat(el.getAttribute('cx') || 0);
        const cy = parseFloat(el.getAttribute('cy') || 0);
        const rx = parseFloat(el.getAttribute('rx') || 0);
        const ry = parseFloat(el.getAttribute('ry') || 0);

        if (rx === 0 || ry === 0) return;

        const box = SvgTransform.rect(cx - rx, cy - ry, rx * 2, ry * 2, transform);

        this.shapes.push({
            type: 'ellipse',
            x: box.x,
            y: box.y,
            width: box.width,
            height: box.height,
            angle: box.angle,
            style,
            id: el.id || null,
            text: null
        });
    }

    _parsePolygon(el, style, transform) {
        const pointsStr = el.getAttribute('points');
        if (!pointsStr) return;

        const points = this._parsePointsString(pointsStr);
        if (points.length < 3) return;

        let minX = Infinity, minY = Infinity, maxX = -Infinity, maxY = -Infinity;
        for (const p of points) {
            const tp = SvgTransform.apply(p.x, p.y, transform);
            minX = Math.min(minX, tp.x);
            minY = Math.min(minY, tp.y);
            maxX = Math.max(maxX, tp.x);
            maxY = Math.max(maxY, tp.y);
        }

        // Detect diamond shape (4 points forming a rhombus)
        const isDiamond = points.length === 4 && this._isDiamondShape(points);

        this.shapes.push({
            type: isDiamond ? 'diamond' : 'polygon',
            x: minX,
            y: minY,
            width: maxX - minX,
            height: maxY - minY,
            points: points.map(p => SvgTransform.apply(p.x, p.y, transform)),
            style,
            id: el.id || null,
            text: null
        });
    }

    _isDiamondShape(points) {
        if (points.length !== 4) return false;
        const cx = points.reduce((s, p) => s + p.x, 0) / 4;
        const cy = points.reduce((s, p) => s + p.y, 0) / 4;

        // Check if points are roughly at top, right, bottom, left of center
        let hasTop = false, hasRight = false, hasBottom = false, hasLeft = false;
        const tolerance = 5;
        for (const p of points) {
            if (Math.abs(p.x - cx) < tolerance && p.y < cy) hasTop = true;
            if (Math.abs(p.x - cx) < tolerance && p.y > cy) hasBottom = true;
            if (p.x > cx && Math.abs(p.y - cy) < tolerance) hasRight = true;
            if (p.x < cx && Math.abs(p.y - cy) < tolerance) hasLeft = true;
        }
        return hasTop && hasRight && hasBottom && hasLeft;
    }

    _parseLine(el, style, transform) {
        const x1 = parseFloat(el.getAttribute('x1') || 0);
        const y1 = parseFloat(el.getAttribute('y1') || 0);
        const x2 = parseFloat(el.getAttribute('x2') || 0);
        const y2 = parseFloat(el.getAttribute('y2') || 0);

        const p1 = SvgTransform.apply(x1, y1, transform);
        const p2 = SvgTransform.apply(x2, y2, transform);

        const arrows = this._arrowMarkers(el);

        this.connectors.push({
            type: 'line',
            points: [p1, p2],
            arrowStart: arrows.start,
            arrowEnd: arrows.end,
            style,
            id: el.id || null,
            text: null
        });
    }

    _parsePolyline(el, style, transform) {
        const pointsStr = el.getAttribute('points');
        if (!pointsStr) return;

        const points = this._parsePointsString(pointsStr).map(p =>
            SvgTransform.apply(p.x, p.y, transform)
        );

        if (points.length < 2) return;

        const arrows = this._arrowMarkers(el);

        this.connectors.push({
            type: 'polyline',
            points,
            arrowStart: arrows.start,
            arrowEnd: arrows.end,
            style,
            id: el.id || null,
            text: null
        });
    }

    _parsePath(el, style, transform) {
        const d = el.getAttribute('d');
        if (!d) return;

        const arrows = this._arrowMarkers(el);
        // An arrowhead is a strong hint that a path is a connector, not a shape
        const hasArrow = arrows.start || arrows.end;
        const points = this._pathToPoints(d, transform);

        if (points.length < 2) return;

        // Determine if this is a connector (line-like) or a filled shape
        const isFilled = style.fill !== 'none' && style.fill !== 'transparent' &&
                         style.fill !== '' && style.fill !== undefined;
        const isClosed = d.toLowerCase().includes('z');
        const isLikelyConnector = !isFilled || hasArrow ||
            (style.stroke !== 'none' && !isClosed);

        if (isLikelyConnector && !isClosed) {
            this.connectors.push({
                type: 'path',
                points,
                arrowStart: arrows.start,
                arrowEnd: arrows.end,
                style,
                d,
                id: el.id || null,
                text: null
            });
        } else if (isClosed && isFilled) {
            // Treat as a shape
            let minX = Infinity, minY = Infinity, maxX = -Infinity, maxY = -Infinity;
            for (const p of points) {
                minX = Math.min(minX, p.x);
                minY = Math.min(minY, p.y);
                maxX = Math.max(maxX, p.x);
                maxY = Math.max(maxY, p.y);
            }
            this.shapes.push({
                type: 'path-shape',
                x: minX,
                y: minY,
                width: maxX - minX,
                height: maxY - minY,
                points,
                style,
                d,
                id: el.id || null,
                text: null
            });
        } else {
            // Default to connector
            this.connectors.push({
                type: 'path',
                points,
                arrowStart: arrows.start,
                arrowEnd: arrows.end,
                style,
                d,
                id: el.id || null,
                text: null
            });
        }
    }

    /** Nearest xml:space declaration, walking up to the root. */
    _xmlSpacePreserved(el) {
        let node = el;
        while (node && node.nodeType === 1) {
            const v = node.getAttribute('xml:space');
            if (v) return v === 'preserve';
            if (node === this.svgEl) break;
            node = node.parentNode;
        }
        return false;
    }

    _parseText(el, style, transform) {
        if (el.querySelector('textPath')) {
            this._warn('<textPath> is not supported - the text was placed at its anchor instead');
        }

        const baseX = parseFloat(el.getAttribute('x') || 0);
        const baseY = parseFloat(el.getAttribute('y') || 0);

        // Walk the children rather than collecting every <tspan> at once: a
        // tspan carrying its own x/y is a new line at a new position, and the
        // bare text before it must not be thrown away.
        const pieces = [];
        let cur = { x: baseX, y: baseY, style, parts: [] };

        // SVG collapses runs of whitespace unless xml:space="preserve"
        const preserve = this._xmlSpacePreserved(el);

        const flush = () => {
            const raw = cur.parts.join('');
            const text = preserve ? raw.replace(/[\n\r\t]/g, ' ') : raw.replace(/\s+/g, ' ').trim();
            if (text) pieces.push({ x: cur.x, y: cur.y, style: cur.style, text });
            cur = { x: cur.x, y: cur.y, style: cur.style, parts: [] };
        };

        for (const node of el.childNodes) {
            if (node.nodeType === 3) {           // text node
                cur.parts.push(node.textContent);
                continue;
            }
            if (node.nodeType !== 1) continue;

            if (node.tagName.toLowerCase() !== 'tspan') {
                cur.parts.push(node.textContent);
                continue;
            }

            const hasOwnPos = node.hasAttribute('x') || node.hasAttribute('y') ||
                              node.hasAttribute('dx') || node.hasAttribute('dy');
            const spanStyle = SvgTransform.scaleStyle(this.style.resolve(node), transform);

            if (!hasOwnPos) {
                cur.parts.push(node.textContent);
                continue;
            }

            flush();
            const nx = node.hasAttribute('x') ? parseFloat(node.getAttribute('x'))
                     : cur.x + parseFloat(node.getAttribute('dx') || 0);
            const ny = node.hasAttribute('y') ? parseFloat(node.getAttribute('y'))
                     : cur.y + parseFloat(node.getAttribute('dy') || 0);
            cur = { x: nx, y: ny, style: spanStyle, parts: [node.textContent] };
            flush();
        }
        flush();

        for (const piece of pieces) {
            const pos = SvgTransform.apply(piece.x, piece.y, transform);
            const st = Object.assign({}, piece.style);
            // Text is painted with `fill`; SVG's default text colour is black.
            st.textColor = (st.fill && st.fill !== 'none') ? st.fill : '#000000';

            this.texts.push({
                x: pos.x,
                y: pos.y,
                text: piece.text,
                style: st,
                id: el.id || null
            });
        }
    }

    /**
     * Which ends carry an arrowhead. Reporting only "has an arrow" put the
     * head on the wrong end for marker-start, and lost one head on a
     * double-headed connector.
     */
    _arrowMarkers(el) {
        return {
            start: !!el.getAttribute('marker-start'),
            end: !!el.getAttribute('marker-end')
        };
    }

    _hasArrowMarker(el) {
        const m = this._arrowMarkers(el);
        return m.start || m.end;
    }

    _parsePointsString(str) {
        const nums = str.trim().split(/[\s,]+/).map(Number);
        const points = [];
        for (let i = 0; i < nums.length - 1; i += 2) {
            if (!isNaN(nums[i]) && !isNaN(nums[i + 1])) {
                points.push({ x: nums[i], y: nums[i + 1] });
            }
        }
        return points;
    }

    _pathToPoints(d, transform) {
        const points = [];
        let curX = 0, curY = 0;
        let startX = 0, startY = 0;

        // Tokenize path data
        const tokens = d.match(/[a-zA-Z][^a-zA-Z]*/g) || [];

        for (const token of tokens) {
            const cmd = token[0];
            const nums = (token.slice(1).match(/-?\d+\.?\d*(?:e[+-]?\d+)?/gi) || []).map(Number);

            switch (cmd) {
                case 'M':
                    for (let i = 0; i < nums.length; i += 2) {
                        curX = nums[i]; curY = nums[i + 1];
                        startX = curX; startY = curY;
                        const p = SvgTransform.apply(curX, curY, transform);
                        points.push(p);
                    }
                    break;
                case 'm':
                    for (let i = 0; i < nums.length; i += 2) {
                        curX += nums[i]; curY += nums[i + 1];
                        if (i === 0) { startX = curX; startY = curY; }
                        const p = SvgTransform.apply(curX, curY, transform);
                        points.push(p);
                    }
                    break;
                case 'L':
                    for (let i = 0; i < nums.length; i += 2) {
                        curX = nums[i]; curY = nums[i + 1];
                        const p = SvgTransform.apply(curX, curY, transform);
                        points.push(p);
                    }
                    break;
                case 'l':
                    for (let i = 0; i < nums.length; i += 2) {
                        curX += nums[i]; curY += nums[i + 1];
                        const p = SvgTransform.apply(curX, curY, transform);
                        points.push(p);
                    }
                    break;
                case 'H':
                    for (const n of nums) {
                        curX = n;
                        const p = SvgTransform.apply(curX, curY, transform);
                        points.push(p);
                    }
                    break;
                case 'h':
                    for (const n of nums) {
                        curX += n;
                        const p = SvgTransform.apply(curX, curY, transform);
                        points.push(p);
                    }
                    break;
                case 'V':
                    for (const n of nums) {
                        curY = n;
                        const p = SvgTransform.apply(curX, curY, transform);
                        points.push(p);
                    }
                    break;
                case 'v':
                    for (const n of nums) {
                        curY += n;
                        const p = SvgTransform.apply(curX, curY, transform);
                        points.push(p);
                    }
                    break;
                case 'C':
                    // Cubic bezier - take endpoint
                    for (let i = 0; i < nums.length; i += 6) {
                        if (i + 5 < nums.length) {
                            // Add control points for curve approximation
                            const midX = (curX + nums[i + 4]) / 2;
                            const midY = (curY + nums[i + 5]) / 2;
                            const mp = SvgTransform.apply(midX, midY, transform);
                            points.push(mp);
                            curX = nums[i + 4]; curY = nums[i + 5];
                            const p = SvgTransform.apply(curX, curY, transform);
                            points.push(p);
                        }
                    }
                    break;
                case 'c':
                    for (let i = 0; i < nums.length; i += 6) {
                        if (i + 5 < nums.length) {
                            const midX = curX + nums[i + 4] / 2;
                            const midY = curY + nums[i + 5] / 2;
                            const mp = SvgTransform.apply(midX, midY, transform);
                            points.push(mp);
                            curX += nums[i + 4]; curY += nums[i + 5];
                            const p = SvgTransform.apply(curX, curY, transform);
                            points.push(p);
                        }
                    }
                    break;
                case 'Q':
                    for (let i = 0; i < nums.length; i += 4) {
                        if (i + 3 < nums.length) {
                            curX = nums[i + 2]; curY = nums[i + 3];
                            const p = SvgTransform.apply(curX, curY, transform);
                            points.push(p);
                        }
                    }
                    break;
                case 'q':
                    for (let i = 0; i < nums.length; i += 4) {
                        if (i + 3 < nums.length) {
                            curX += nums[i + 2]; curY += nums[i + 3];
                            const p = SvgTransform.apply(curX, curY, transform);
                            points.push(p);
                        }
                    }
                    break;
                case 'A': case 'a':
                    // Arc - just take the endpoint
                    for (let i = 0; i < nums.length; i += 7) {
                        if (i + 6 < nums.length) {
                            if (cmd === 'a') {
                                curX += nums[i + 5]; curY += nums[i + 6];
                            } else {
                                curX = nums[i + 5]; curY = nums[i + 6];
                            }
                            const p = SvgTransform.apply(curX, curY, transform);
                            points.push(p);
                        }
                    }
                    break;
                case 'Z': case 'z':
                    curX = startX; curY = startY;
                    break;
            }
        }

        return points;
    }


    getStats() {
        return {
            shapes: this.shapes.length,
            connectors: this.connectors.length,
            texts: this.texts.length,
            warnings: this.warnings,
            viewBox: this.viewBox
        };
    }
}
