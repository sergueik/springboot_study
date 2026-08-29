/**
 * Draw.io Parser - Extracts shapes, connectors, and text from Draw.io XML files.
 *
 * Draw.io uses mxGraphModel with mxCell elements:
 * - vertex="1" cells are shapes (with mxGeometry for position/size)
 * - edge="1" cells are connectors (with source/target references)
 * - Style is a semicolon-separated key=value string
 *
 * Output format matches SvgParser for compatibility with VsdxBuilder.
 */

class DrawioParser {
    constructor(xmlString) {
        this.xmlString = xmlString;
        this.parser = new DOMParser();
        this.viewBox = { x: 0, y: 0, width: 800, height: 600 };
        this.shapes = [];
        this.connectors = [];
        this.texts = [];

        // Maps Draw.io cell IDs to shape indices in this.shapes
        this.cellIdToShapeIndex = {};
    }

    parse() {
        let xmlContent = this.xmlString;

        // Handle <mxfile> wrapper — may contain compressed or raw <mxGraphModel>
        const doc = this.parser.parseFromString(xmlContent, 'text/xml');
        const errorNode = doc.querySelector('parsererror');
        if (errorNode) {
            throw new Error('Invalid XML: ' + errorNode.textContent.slice(0, 200));
        }

        // Find the mxGraphModel — could be root or inside <diagram>
        let graphModel = doc.querySelector('mxGraphModel');

        if (!graphModel) {
            // Check if diagram content is compressed (base64+deflate)
            const diagramEl = doc.querySelector('diagram');
            if (diagramEl) {
                const compressed = diagramEl.textContent.trim();
                if (compressed && !compressed.includes('<')) {
                    const decompressed = this._decompress(compressed);
                    const innerDoc = this.parser.parseFromString(decompressed, 'text/xml');
                    graphModel = innerDoc.querySelector('mxGraphModel');
                }
            }
        }

        if (!graphModel) {
            throw new Error('No <mxGraphModel> found in Draw.io file');
        }

        this._parseGraphModel(graphModel);

        return {
            viewBox: this.viewBox,
            shapes: this.shapes,
            connectors: this.connectors,
            texts: this.texts
        };
    }

    getStats() {
        return {
            shapes: this.shapes.length,
            connectors: this.connectors.length,
            texts: this.texts.length,
            viewBox: this.viewBox
        };
    }

    _decompress(data) {
        // Draw.io compressed format: base64 → inflate → URI decode
        const binary = atob(data);
        const bytes = new Uint8Array(binary.length);
        for (let i = 0; i < binary.length; i++) {
            bytes[i] = binary.charCodeAt(i);
        }

        // Use pako for decompression (included via CDN)
        if (typeof pako === 'undefined') {
            throw new Error('pako library required for compressed Draw.io files. Include pako via CDN.');
        }

        const inflated = pako.inflateRaw(bytes, { to: 'string' });
        return decodeURIComponent(inflated);
    }

    _parseGraphModel(model) {
        // Extract page dimensions from model attributes
        const pageWidth = parseFloat(model.getAttribute('pageWidth')) || 800;
        const pageHeight = parseFloat(model.getAttribute('pageHeight')) || 600;

        const root = model.querySelector('root');
        if (!root) {
            throw new Error('No <root> element in mxGraphModel');
        }

        // Collect all cells (both mxCell and object>mxCell)
        const cells = [];
        for (const child of root.children) {
            if (child.tagName === 'mxCell') {
                cells.push({ cell: child, label: child.getAttribute('value') || '' });
            } else if (child.tagName === 'object' || child.tagName === 'UserObject') {
                const innerCell = child.querySelector('mxCell');
                if (innerCell) {
                    cells.push({ cell: innerCell, label: child.getAttribute('label') || '' });
                }
            }
        }

        // First pass: parse all vertices (shapes)
        for (const { cell, label } of cells) {
            if (cell.getAttribute('vertex') === '1') {
                this._parseVertex(cell, label);
            }
        }

        // Compute bounding box for viewBox
        this._computeViewBox(pageWidth, pageHeight);

        // Second pass: parse all edges (connectors)
        for (const { cell, label } of cells) {
            if (cell.getAttribute('edge') === '1') {
                this._parseEdge(cell, label);
            }
        }
    }

    _parseVertex(cell, label) {
        const geom = cell.querySelector('mxGeometry');
        if (!geom) return;

        const x = parseFloat(geom.getAttribute('x')) || 0;
        const y = parseFloat(geom.getAttribute('y')) || 0;
        const w = parseFloat(geom.getAttribute('width')) || 100;
        const h = parseFloat(geom.getAttribute('height')) || 60;

        const styleStr = cell.getAttribute('style') || '';
        const style = this._parseStyle(styleStr);

        // Determine shape type from style
        let type = 'rect';
        const shapeName = style['shape'] || '';

        if (shapeName === 'ellipse' || styleStr.includes('ellipse')) {
            type = 'ellipse';
        } else if (shapeName === 'rhombus' || styleStr.includes('rhombus')) {
            type = 'diamond';
        } else if (shapeName === 'triangle' || styleStr.includes('triangle')) {
            type = 'polygon';
        } else if (shapeName === 'hexagon' || styleStr.includes('hexagon')) {
            type = 'polygon';
        } else if (shapeName === 'cylinder' || shapeName === 'cylinder3') {
            type = 'rect'; // approximate as rect
        } else if (shapeName === 'parallelogram') {
            type = 'polygon';
        }

        // Strip HTML from label
        const text = this._stripHtml(label);

        // Build style object matching SvgParser format
        const fillColor = style['fillColor'] || '#FFFFFF';
        const strokeColor = style['strokeColor'] || '#000000';
        const strokeWidth = parseFloat(style['strokeWidth']) || 1;
        const opacity = parseFloat(style['opacity']) || 100;
        const rounded = style['rounded'] === '1';
        const dashed = style['dashed'] === '1';

        const shapeObj = {
            type: type,
            x: x,
            y: y,
            width: w,
            height: h,
            style: {
                fill: fillColor === 'none' ? 'none' : fillColor,
                stroke: strokeColor === 'none' ? 'none' : strokeColor,
                strokeWidth: strokeWidth,
                strokeDasharray: dashed ? '5,5' : null,
                opacity: opacity / 100,
                fillOpacity: 1,
                rx: rounded ? Math.min(w, h) * 0.1 : 0
            },
            text: text || null,
            textStyle: null,
            verticalAlign: style['verticalAlign'] || 'middle'
        };

        if (text) {
            const fontSize = parseFloat(style['fontSize']) || 12;
            const fontColor = style['fontColor'] || '#000000';
            const fontStyleVal = parseInt(style['fontStyle']) || 0;

            shapeObj.textStyle = {
                fontSize: fontSize,
                textColor: fontColor,
                fontWeight: (fontStyleVal & 1) ? 'bold' : 'normal'
            };
        }

        // Handle polygon points for specific shapes
        if (type === 'polygon' && shapeName === 'triangle') {
            shapeObj.points = [
                { x: x + w / 2, y: y },
                { x: x + w, y: y + h },
                { x: x, y: y + h }
            ];
        } else if (type === 'polygon' && shapeName === 'hexagon') {
            const inset = w * 0.25;
            shapeObj.points = [
                { x: x + inset, y: y },
                { x: x + w - inset, y: y },
                { x: x + w, y: y + h / 2 },
                { x: x + w - inset, y: y + h },
                { x: x + inset, y: y + h },
                { x: x, y: y + h / 2 }
            ];
        } else if (type === 'polygon' && shapeName === 'parallelogram') {
            const inset = w * 0.2;
            shapeObj.points = [
                { x: x + inset, y: y },
                { x: x + w, y: y },
                { x: x + w - inset, y: y + h },
                { x: x, y: y + h }
            ];
        }

        const cellId = cell.getAttribute('id');
        this.cellIdToShapeIndex[cellId] = this.shapes.length;
        this.shapes.push(shapeObj);
    }

    _parseEdge(cell, label) {
        const styleStr = cell.getAttribute('style') || '';
        const style = this._parseStyle(styleStr);

        const sourceId = cell.getAttribute('source');
        const targetId = cell.getAttribute('target');

        // Determine connection points
        const points = this._getEdgePoints(cell, sourceId, targetId);
        if (points.length < 2) return;

        const strokeColor = style['strokeColor'] || '#000000';
        const strokeWidth = parseFloat(style['strokeWidth']) || 1;
        const dashed = style['dashed'] === '1';

        // Detect arrows
        const endArrow = style['endArrow'] || 'classic';
        const startArrow = style['startArrow'] || 'none';
        const hasArrow = endArrow !== 'none' && endArrow !== '0';

        const fromShape = sourceId != null ? (this.cellIdToShapeIndex[sourceId] ?? null) : null;
        const toShape = targetId != null ? (this.cellIdToShapeIndex[targetId] ?? null) : null;

        this.connectors.push({
            type: 'line',
            points: points,
            hasArrow: hasArrow,
            style: {
                stroke: strokeColor,
                strokeWidth: strokeWidth,
                strokeDasharray: dashed ? '5,5' : null,
                opacity: 1
            },
            fromShape: fromShape,
            toShape: toShape
        });

        // If edge has a label, add as standalone text at midpoint
        const text = this._stripHtml(label);
        if (text) {
            const midIdx = Math.floor(points.length / 2);
            const midPt = points[midIdx];
            this.texts.push({
                x: midPt.x,
                y: midPt.y,
                text: text,
                style: {
                    fontSize: parseFloat(style['fontSize']) || 11,
                    textColor: style['fontColor'] || '#000000',
                    fontWeight: 'normal',
                    fill: 'none'
                }
            });
        }
    }

    _getEdgePoints(cell, sourceId, targetId) {
        const points = [];
        const geom = cell.querySelector('mxGeometry');

        // Source point
        if (sourceId && this.cellIdToShapeIndex[sourceId] !== undefined) {
            const shape = this.shapes[this.cellIdToShapeIndex[sourceId]];
            points.push({ x: shape.x + shape.width / 2, y: shape.y + shape.height / 2 });
        } else if (geom) {
            const srcPt = geom.querySelector('mxPoint[as="sourcePoint"]');
            if (srcPt) {
                points.push({
                    x: parseFloat(srcPt.getAttribute('x')) || 0,
                    y: parseFloat(srcPt.getAttribute('y')) || 0
                });
            }
        }

        // Intermediate waypoints
        if (geom) {
            const waypoints = geom.querySelector('Array[as="points"]');
            if (waypoints) {
                for (const pt of waypoints.querySelectorAll('mxPoint')) {
                    points.push({
                        x: parseFloat(pt.getAttribute('x')) || 0,
                        y: parseFloat(pt.getAttribute('y')) || 0
                    });
                }
            }
        }

        // Target point
        if (targetId && this.cellIdToShapeIndex[targetId] !== undefined) {
            const shape = this.shapes[this.cellIdToShapeIndex[targetId]];
            points.push({ x: shape.x + shape.width / 2, y: shape.y + shape.height / 2 });
        } else if (geom) {
            const tgtPt = geom.querySelector('mxPoint[as="targetPoint"]');
            if (tgtPt) {
                points.push({
                    x: parseFloat(tgtPt.getAttribute('x')) || 0,
                    y: parseFloat(tgtPt.getAttribute('y')) || 0
                });
            }
        }

        return points;
    }

    _computeViewBox(defaultWidth, defaultHeight) {
        if (this.shapes.length === 0) {
            this.viewBox = { x: 0, y: 0, width: defaultWidth, height: defaultHeight };
            return;
        }

        let minX = Infinity, minY = Infinity, maxX = -Infinity, maxY = -Infinity;
        for (const s of this.shapes) {
            minX = Math.min(minX, s.x);
            minY = Math.min(minY, s.y);
            maxX = Math.max(maxX, s.x + s.width);
            maxY = Math.max(maxY, s.y + s.height);
        }

        // Add padding
        const pad = 40;
        this.viewBox = {
            x: 0,
            y: 0,
            width: Math.max(maxX + pad, defaultWidth),
            height: Math.max(maxY + pad, defaultHeight)
        };
    }

    _parseStyle(styleStr) {
        const result = {};
        if (!styleStr) return result;

        const parts = styleStr.split(';');
        for (const part of parts) {
            const eq = part.indexOf('=');
            if (eq > 0) {
                const key = part.substring(0, eq).trim();
                const val = part.substring(eq + 1).trim();
                result[key] = val;
            }
        }
        return result;
    }

    _stripHtml(str) {
        if (!str) return '';
        // Remove HTML tags and decode entities
        return str
            .replace(/<br\s*\/?>/gi, '\n')
            .replace(/<[^>]+>/g, '')
            .replace(/&amp;/g, '&')
            .replace(/&lt;/g, '<')
            .replace(/&gt;/g, '>')
            .replace(/&quot;/g, '"')
            .replace(/&#39;/g, "'")
            .replace(/&nbsp;/g, ' ')
            .trim();
    }
}
