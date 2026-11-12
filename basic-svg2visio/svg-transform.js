// @ts-check
/**
 * Affine transform maths, free of any DOM or parser state.
 *
 * Transforms are matrices {a,b,c,d,e,f}:
 *   x' = a*x + c*y + e
 *   y' = b*x + d*y + f
 *
 * They used to be carried as concatenated attribute strings, so only the
 * outermost translate of a nested group ever applied and scale, rotate, skew
 * and matrix were dropped entirely.
 *
 * @typedef {{a:number,b:number,c:number,d:number,e:number,f:number}} Matrix
 */

const SvgTransform = {
    /** @type {Matrix} */
    IDENTITY: Object.freeze({ a: 1, b: 0, c: 0, d: 1, e: 0, f: 0 }),


    multiply(m1, m2) {
        return {
            a: m1.a * m2.a + m1.c * m2.b,
            b: m1.b * m2.a + m1.d * m2.b,
            c: m1.a * m2.c + m1.c * m2.d,
            d: m1.b * m2.c + m1.d * m2.d,
            e: m1.a * m2.e + m1.c * m2.f + m1.e,
            f: m1.b * m2.e + m1.d * m2.f + m1.f
        };
    },

    /** Parse a transform attribute (any number of chained functions). */
    parseAttr(str, warn) {
        if (!str) return null;
        let m = null;
        const fnRe = /([a-zA-Z]+)\s*\(([^)]*)\)/g;
        let match;
        while ((match = fnRe.exec(str)) !== null) {
            const name = match[1].toLowerCase();
            const args = match[2].trim().split(/[\s,]+/).map(parseFloat).filter(n => !isNaN(n));
            const step = this.functionToMatrix(name, args);
            if (!step) {
                if (warn) warn(`transform "${name}()" is not supported and was ignored`);
                continue;
            }
            m = m ? this.multiply(m, step) : step;
        }
        return m;
    },

    functionToMatrix(name, a) {
        const rad = deg => deg * Math.PI / 180;
        switch (name) {
            case 'translate':
                return { a: 1, b: 0, c: 0, d: 1, e: a[0] || 0, f: a[1] || 0 };
            case 'scale': {
                const sx = a[0] !== undefined ? a[0] : 1;
                const sy = a[1] !== undefined ? a[1] : sx;
                return { a: sx, b: 0, c: 0, d: sy, e: 0, f: 0 };
            }
            case 'rotate': {
                const t = rad(a[0] || 0);
                const cos = Math.cos(t), sin = Math.sin(t);
                const rot = { a: cos, b: sin, c: -sin, d: cos, e: 0, f: 0 };
                if (a.length < 3) return rot;
                // rotate(angle cx cy) == translate(cx,cy) rotate(angle) translate(-cx,-cy)
                const to = { a: 1, b: 0, c: 0, d: 1, e: a[1], f: a[2] };
                const back = { a: 1, b: 0, c: 0, d: 1, e: -a[1], f: -a[2] };
                return this.multiply(this.multiply(to, rot), back);
            }
            case 'skewx':
                return { a: 1, b: 0, c: Math.tan(rad(a[0] || 0)), d: 1, e: 0, f: 0 };
            case 'skewy':
                return { a: 1, b: Math.tan(rad(a[0] || 0)), c: 0, d: 1, e: 0, f: 0 };
            case 'matrix':
                if (a.length < 6) return null;
                return { a: a[0], b: a[1], c: a[2], d: a[3], e: a[4], f: a[5] };
            default:
                return null;
        }
    },

    combine(parent, child, warn) {
        const childMatrix = typeof child === 'string' ? this.parseAttr(child, warn) : child;
        if (!parent) return childMatrix || null;
        if (!childMatrix) return parent;
        return this.multiply(parent, childMatrix);
    },

    apply(x, y, m) {
        if (!m) return { x, y };
        return {
            x: m.a * x + m.c * y + m.e,
            y: m.b * x + m.d * y + m.f
        };
    },

    /** Uniform scale factor of a matrix (used for stroke widths and font sizes). */
    scaleOf(m) {
        if (!m) return 1;
        const det = Math.abs(m.a * m.d - m.b * m.c);
        return det > 0 ? Math.sqrt(det) : 1;
    },

    /**
     * Map an axis-aligned box through a matrix. Pure translate/scale keeps it
     * axis aligned; a rotation is reported separately so Visio can spin the
     * shape via its Angle cell instead of distorting the geometry.
     */
    rect(x, y, w, h, m) {
        if (!m) return { x, y, width: w, height: h, angle: 0 };

        const centre = this.apply(x + w / 2, y + h / 2, m);
        const sx = Math.hypot(m.a, m.b);
        const sy = Math.hypot(m.c, m.d);
        const width = w * sx;
        const height = h * sy;
        // SVG rotates clockwise (y grows downwards), Visio counter-clockwise.
        const angle = -Math.atan2(m.b, m.a);

        return {
            x: centre.x - width / 2,
            y: centre.y - height / 2,
            width,
            height,
            angle: Math.abs(angle) < 1e-9 ? 0 : angle
        };
    },

    /** Stroke widths and font sizes scale with the transform. */
    scaleStyle(style, m) {
        const s = this.scaleOf(m);
        if (s === 1) return style;
        return Object.assign({}, style, {
            strokeWidth: style.strokeWidth * s,
            fontSize: style.fontSize * s,
            rx: style.rx * s,
            ry: style.ry * s
        });
    }
};
