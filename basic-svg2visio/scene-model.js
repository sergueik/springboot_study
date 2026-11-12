// @ts-check
/**
 * The scene model - the contract between every parser and the builder.
 *
 * Both SvgParser and DrawioParser must produce exactly this shape, and
 * VsdxBuilder may rely on it without defensive fallbacks. Until this file
 * existed the two parsers emitted different fields and the builder guessed
 * which one it was handed.
 *
 * Coordinates are SVG user units (pixels, y growing downwards). Conversion to
 * Visio inches happens in the builder and nowhere else.
 */

/**
 * @typedef {Object} Style
 * @property {string}  fill             CSS colour, or 'none'
 * @property {number}  fillOpacity      0..1
 * @property {string}  stroke           CSS colour, or 'none'
 * @property {number}  strokeWidth      user units
 * @property {number}  strokeOpacity    0..1
 * @property {string|null} strokeDasharray  raw dash array, or null when solid
 * @property {number}  fontSize         user units
 * @property {string}  fontFamily
 * @property {string}  fontWeight       'normal' | 'bold' | numeric string
 * @property {'start'|'middle'|'end'} textAnchor
 * @property {number}  opacity          0..1
 * @property {number}  rx               corner radius
 * @property {number}  ry
 * @property {string} [textColor]       resolved text colour (text nodes only)
 */

/**
 * @typedef {Object} Point
 * @property {number} x
 * @property {number} y
 */

/**
 * @typedef {Object} TextRun
 * @property {string} text
 * @property {Style}  style
 */

/**
 * @typedef {Object} Shape
 * @property {'rect'|'circle'|'ellipse'|'diamond'|'polygon'|'path-shape'} type
 * @property {number} x                 left edge of the unrotated box
 * @property {number} y                 top edge of the unrotated box
 * @property {number} width
 * @property {number} height
 * @property {number} angle             radians, counter-clockwise, about the centre
 * @property {Style}  style
 * @property {string|null} id
 * @property {string|null} text         all runs joined with '\n', or null
 * @property {TextRun[]}   textRuns     one entry per source line
 * @property {Style|null}  textStyle    dominant run's style
 * @property {boolean} isContainer      encloses other shapes; never labelled or glued to
 * @property {number|null} parentShape   index of the frame this sits inside
 * @property {Point[]} [points]         polygon / path outline
 * @property {string}  [d]              original path data
 */

/**
 * @typedef {Object} Connector
 * @property {'line'|'polyline'|'path'} type
 * @property {Point[]} points           at least two
 * @property {boolean} arrowStart
 * @property {boolean} arrowEnd
 * @property {Style}   style
 * @property {string|null} id
 * @property {string|null} text
 * @property {number|null} fromShape    index into Scene.shapes
 * @property {number|null} toShape
 * @property {number|null} parentShape  index of the frame this sits inside
 * @property {string} [d]
 */

/**
 * @typedef {Object} StandaloneText
 * @property {number} x                 anchor point
 * @property {number} y                 baseline
 * @property {string} text
 * @property {Style}  style
 * @property {string|null} id
 * @property {boolean} standalone  the parser already decided this text is
 *                                 free-floating; the layout stage must not
 *                                 fold it into a shape
 * @property {number|null} parentShape   index of the frame this sits inside
 */

/**
 * @typedef {Object} ViewBox
 * @property {number} x
 * @property {number} y
 * @property {number} width
 * @property {number} height
 */

/**
 * @typedef {Object} Scene
 * @property {ViewBox}  viewBox
 * @property {Shape[]}  shapes
 * @property {Connector[]} connectors
 * @property {StandaloneText[]} texts
 * @property {string[]} warnings        things the .vsdx cannot reproduce
 */

const SceneModel = {
    SHAPE_TYPES: Object.freeze(['rect', 'circle', 'ellipse', 'diamond', 'polygon', 'path-shape']),
    CONNECTOR_TYPES: Object.freeze(['line', 'polyline', 'path']),
    TEXT_ANCHORS: Object.freeze(['start', 'middle', 'end']),

    /**
     * A style with every field populated. Parsers start from this so no
     * consumer ever has to test a field for undefined.
     * @returns {Style}
     */
    defaultStyle() {
        return {
            fill: 'none',
            fillOpacity: 1,
            stroke: 'none',
            strokeWidth: 1,
            strokeOpacity: 1,
            strokeDasharray: null,
            fontSize: 14,
            fontFamily: 'Calibri',
            fontWeight: 'normal',
            textAnchor: 'start',
            opacity: 1,
            rx: 0,
            ry: 0
        };
    },

    /**
     * Fill in whatever a parser left out, so the model is complete before it
     * reaches the layout stage or the builder.
     * @param {any} scene
     * @returns {Scene}
     */
    normalise(scene) {
        const vb = scene.viewBox || {};
        scene.viewBox = {
            x: vb.x || 0,
            y: vb.y || 0,
            width: vb.width || 800,
            height: vb.height || 600
        };
        scene.shapes = scene.shapes || [];
        scene.connectors = scene.connectors || [];
        scene.texts = scene.texts || [];
        scene.warnings = scene.warnings || [];

        for (const shape of scene.shapes) {
            shape.style = Object.assign(this.defaultStyle(), shape.style);
            if (shape.id === undefined) shape.id = null;
            if (shape.angle === undefined) shape.angle = 0;
            if (shape.text === undefined) shape.text = null;
            if (shape.isContainer === undefined) shape.isContainer = false;
            if (shape.parentShape === undefined) shape.parentShape = null;
            if (shape.textStyle === undefined) shape.textStyle = null;
            if (!shape.textRuns) {
                shape.textRuns = shape.text
                    ? [{ text: shape.text, style: shape.textStyle || shape.style }]
                    : [];
            }
        }

        for (const conn of scene.connectors) {
            conn.style = Object.assign(this.defaultStyle(), conn.style);
            if (conn.id === undefined) conn.id = null;
            if (conn.text === undefined) conn.text = null;
            if (conn.fromShape === undefined) conn.fromShape = null;
            if (conn.toShape === undefined) conn.toShape = null;
            if (conn.parentShape === undefined) conn.parentShape = null;
            // Legacy scenes that only say "there is an arrow" mean the end
            if (conn.arrowEnd === undefined) conn.arrowEnd = !!conn.hasArrow;
            if (conn.arrowStart === undefined) conn.arrowStart = false;
        }

        for (const text of scene.texts) {
            text.style = Object.assign(this.defaultStyle(), text.style);
            if (text.id === undefined) text.id = null;
            if (text.standalone === undefined) text.standalone = false;
            if (text.parentShape === undefined) text.parentShape = null;
        }

        return scene;
    },

    /**
     * Structural check used by the tests. Returns the problems found so a
     * parser that drifts from the contract fails loudly instead of producing
     * a subtly wrong .vsdx.
     * @param {any} scene
     * @returns {string[]}
     */
    validate(scene) {
        const problems = [];
        const num = (v) => typeof v === 'number' && isFinite(v);
        const shapeIndex = (v) => v === null ||
            (Number.isInteger(v) && v >= 0 && v < scene.shapes.length);

        if (!scene || typeof scene !== 'object') return ['scene is not an object'];

        for (const key of ['viewBox', 'shapes', 'connectors', 'texts', 'warnings']) {
            if (scene[key] === undefined) problems.push(`scene.${key} is missing`);
        }
        if (problems.length) return problems;

        for (const key of ['x', 'y', 'width', 'height']) {
            if (!num(scene.viewBox[key])) problems.push(`viewBox.${key} is not a number`);
        }

        const checkStyle = (style, where) => {
            if (!style) { problems.push(`${where}.style is missing`); return; }
            for (const key of ['fill', 'stroke', 'fontFamily', 'fontWeight', 'textAnchor']) {
                if (typeof style[key] !== 'string') problems.push(`${where}.style.${key} is not a string`);
            }
            for (const key of ['fillOpacity', 'strokeWidth', 'strokeOpacity', 'fontSize', 'opacity', 'rx', 'ry']) {
                if (!num(style[key])) problems.push(`${where}.style.${key} is not a number`);
            }
            if (this.TEXT_ANCHORS.indexOf(style.textAnchor) === -1) {
                problems.push(`${where}.style.textAnchor is "${style.textAnchor}"`);
            }
        };

        scene.shapes.forEach((shape, i) => {
            const where = `shapes[${i}]`;
            if (this.SHAPE_TYPES.indexOf(shape.type) === -1) problems.push(`${where}.type is "${shape.type}"`);
            for (const key of ['x', 'y', 'width', 'height', 'angle']) {
                if (!num(shape[key])) problems.push(`${where}.${key} is not a number`);
            }
            if (typeof shape.isContainer !== 'boolean') problems.push(`${where}.isContainer is not a boolean`);
            if (!shapeIndex(shape.parentShape)) {
                problems.push(`${where}.parentShape is not null or a valid shape index`);
            } else if (shape.parentShape === i) {
                problems.push(`${where}.parentShape points at itself`);
            }
            if (!Array.isArray(shape.textRuns)) problems.push(`${where}.textRuns is not an array`);
            if (shape.text !== null && typeof shape.text !== 'string') problems.push(`${where}.text is neither string nor null`);
            checkStyle(shape.style, where);
        });

        scene.connectors.forEach((conn, i) => {
            const where = `connectors[${i}]`;
            if (this.CONNECTOR_TYPES.indexOf(conn.type) === -1) problems.push(`${where}.type is "${conn.type}"`);
            if (!Array.isArray(conn.points) || conn.points.length < 2) {
                problems.push(`${where}.points has fewer than 2 entries`);
            } else {
                conn.points.forEach((p, j) => {
                    if (!num(p.x) || !num(p.y)) problems.push(`${where}.points[${j}] is not a point`);
                });
            }
            for (const key of ['arrowStart', 'arrowEnd']) {
                if (typeof conn[key] !== 'boolean') problems.push(`${where}.${key} is not a boolean`);
            }
            for (const key of ['fromShape', 'toShape', 'parentShape']) {
                if (!shapeIndex(conn[key])) {
                    problems.push(`${where}.${key} is not null or a valid shape index`);
                }
            }
            checkStyle(conn.style, where);
        });

        scene.texts.forEach((text, i) => {
            const where = `texts[${i}]`;
            if (!num(text.x) || !num(text.y)) problems.push(`${where} has no numeric position`);
            if (typeof text.text !== 'string') problems.push(`${where}.text is not a string`);
            if (typeof text.standalone !== 'boolean') problems.push(`${where}.standalone is not a boolean`);
            if (!shapeIndex(text.parentShape)) {
                problems.push(`${where}.parentShape is not null or a valid shape index`);
            }
            checkStyle(text.style, where);
        });

        // A parent chain that loops would make the builder recurse forever.
        // Nesting only ever points at a strictly larger frame, so a cycle
        // means a heuristic went wrong rather than a drawing being odd.
        scene.shapes.forEach((shape, i) => {
            const seen = new Set([i]);
            let p = shape.parentShape;
            while (Number.isInteger(p) && p >= 0 && p < scene.shapes.length) {
                if (seen.has(p)) { problems.push(`shapes[${i}].parentShape chain is a cycle`); break; }
                seen.add(p);
                p = scene.shapes[p].parentShape;
            }
        });

        return problems;
    }
};
