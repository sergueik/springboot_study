// @ts-check
/**
 * Layout heuristics, applied to the scene model after parsing.
 *
 * These decide which shapes are grouping frames, which label belongs to which
 * box, and which shapes a connector is glued to. They live here rather than in
 * a parser because they are not SVG-specific: a Draw.io drawing with nested
 * containers must behave the same way as the equivalent SVG. Previously they
 * only ran for SVG input.
 *
 * Every threshold is in TUNING. When a drawing converts badly, this is the
 * first place to look.
 */

const SceneLayout = {
    TUNING: Object.freeze({
        /**
         * A shape counts as a frame only if the shape it encloses is
         * meaningfully smaller. Without the margin, two rectangles of nearly
         * identical size would each call the other a container.
         */
        containerAreaRatio: 0.9,

        /**
         * Slack, in user units, when testing whether a label sits inside a
         * box. Generous padding lets a caption next to a small swatch get
         * swallowed by it, so keep this tight.
         */
        labelPadding: 2,

        /**
         * Slack, in user units, when testing whether a shape sits inside a
         * frame. A tile drawn flush with its frame's edge still belongs to it,
         * and drawings round their coordinates.
         */
        nestPadding: 1,

        /**
         * The connector glue radius is this fraction of the drawing's
         * diagonal, clamped below. A fixed radius is far too coarse on a small
         * drawing and far too tight on a large one.
         */
        glueRadiusFraction: 0.015,
        glueRadiusMin: 8,
        glueRadiusMax: 48,

        /**
         * Average em width used to decide whether a label fits inside a tile.
         * Visio wraps shape text at the tile width; SVG just overflows. A
         * line that would wrap is left floating so it stays one line.
         */
        labelFitEm: 0.55,
        labelFitEmBold: 0.59,
        labelFitInset: 0
    }),

    /**
     * Run every heuristic over a scene, in order.      * Containers must be known
     * before labels are assigned, labels before nesting (so a label folded
     * into a box is not also nested as a loose text), and gluing before a
     * connector is nested, because which frame can hold a connector depends
     * on where its ends are glued.
     * @param {any} scene
     * @param {any} [tuning]
     */
    apply(scene, tuning) {
        const t = tuning || this.TUNING;
        this.markContainers(scene, t);
        this.associateTexts(scene, t);
        this.nest(scene, t);
        this.glueConnectors(scene, t);
        this.nestConnectors(scene, t);
        return scene;
    },

    /** Glue radius for a drawing of this size, in user units. */
    glueRadius(scene, tuning) {
        const diagonal = Math.hypot(scene.viewBox.width, scene.viewBox.height);
        return Math.max(tuning.glueRadiusMin,
                        Math.min(tuning.glueRadiusMax, diagonal * tuning.glueRadiusFraction));
    },

    /**
     * A shape that fully encloses another shape is a grouping frame (subnet,
     * VNet, legend box), not a labelled box. Such frames must not swallow the
     * labels of the shapes inside them, and connectors must not glue to them.
     */
    markContainers(scene, tuning) {
        for (const a of scene.shapes) {
            a.isContainer = false;
            const areaA = a.width * a.height;
            if (areaA <= 0) continue;
            for (const b of scene.shapes) {
                if (b === a) continue;
                const areaB = b.width * b.height;
                if (areaB <= 0 || areaB >= areaA * tuning.containerAreaRatio) continue;
                if (b.x >= a.x - 1 && b.y >= a.y - 1 &&
                    b.x + b.width <= a.x + a.width + 1 &&
                    b.y + b.height <= a.y + a.height + 1) {
                    a.isContainer = true;
                    break;
                }
            }
        }
    },

    /** The frames something can nest into: unrotated containers. */
    nestingFrames(scene) {
        const frames = [];
        scene.shapes.forEach((shape, i) => {
            // A rotated frame would turn its children with it, and the child
            // coordinates are axis-aligned, so leave those flat.
            if (shape.isContainer && !shape.angle) {
                frames.push({ index: i, shape, area: shape.width * shape.height });
            }
        });
        return frames;
    },

    /**
     * The innermost frame enclosing this box, or null.
     *
     * Only frames strictly larger than the box adopt it: that ordering by
     * area is what makes a parent chain impossible to loop.
     */
    innermostFrame(frames, tuning, x, y, width, height, ownIndex, ownArea) {
        const PAD = tuning.nestPadding;
        let best = null;
        let bestArea = Infinity;

        for (const frame of frames) {
            if (frame.index === ownIndex) continue;
            if (frame.area <= ownArea) continue;
            const f = frame.shape;
            if (x < f.x - PAD || y < f.y - PAD ||
                x + width > f.x + f.width + PAD ||
                y + height > f.y + f.height + PAD) continue;
            if (frame.area < bestArea) {
                bestArea = frame.area;
                best = frame.index;
            }
        }

        return best;
    },

    /**
     * Give every shape and loose text the frame it sits in.
     *
     * A drawing's frames are the whole structure a reader sees - a subnet
     * inside a VNet inside a subscription - but on the page they are just
     * rectangles that happen to be drawn around other rectangles. Recording
     * the containment lets the builder emit real groups, so dragging a frame
     * takes its contents along instead of sliding out from under them.
     */
    nest(scene, tuning) {
        const frames = this.nestingFrames(scene);

        scene.shapes.forEach((shape, i) => {
            shape.parentShape = this.innermostFrame(frames, tuning, shape.x, shape.y,
                shape.width, shape.height, i, shape.width * shape.height);
        });

        // A text has no extent of its own worth testing - its anchor decides,
        // the same way associateTexts decides which box owns a label.
        // Do not nest a long overflowing line into a frame narrower than the
        // line: Visio/Aspose clip members to the group, which wraps the text.
        for (const text of scene.texts) {
            let host = this.innermostFrame(frames, tuning, text.x, text.y, 0, 0, -1, 0);
            const need = this.estimatedLineWidth(text, tuning);
            while (host !== null && scene.shapes[host].width < need) {
                host = scene.shapes[host].parentShape;
            }
            text.parentShape = host;
        }
    },

    /**
     * Give a connector the frame it sits in, if one holds all of it.
     *
     * A connector joins two things and cannot belong to a frame that holds
     * only one of them, so it stays on the page unless its whole route and
     * both its ends are inside the same frame. Glue already carries a
     * connector along when both ends are glued to shapes that move; this is
     * for the rest - a legend's key lines, glued at one end or neither, which
     * would otherwise stretch out of shape when their frame moves.
     *
     * This has to run after gluing: which frame can hold a connector depends
     * on where its ends are glued.
     */
    nestConnectors(scene, tuning) {
        const frames = this.nestingFrames(scene);

        // Is b the frame a, or nested inside it?
        const within = (b, a) => {
            let p = b;
            while (Number.isInteger(p)) {
                if (p === a) return true;
                p = scene.shapes[p].parentShape;
            }
            return false;
        };

        for (const conn of scene.connectors) {
            const xs = conn.points.map(p => p.x);
            const ys = conn.points.map(p => p.y);
            const minX = Math.min.apply(null, xs);
            const minY = Math.min.apply(null, ys);
            const ends = [conn.fromShape, conn.toShape];

            let host = this.innermostFrame(frames, tuning, minX, minY,
                Math.max.apply(null, xs) - minX,
                Math.max.apply(null, ys) - minY, -1, 0);
            while (host !== null && !ends.every(t => t === null || within(t, host))) {
                host = scene.shapes[host].parentShape;
            }
            conn.parentShape = host;
        }
    },

    associateTexts(scene, tuning) {
        const PAD = tuning.labelPadding;
        const candidates = new Map();

        for (const text of scene.texts) {
            // A parser that already classified this text keeps the last word
            if (text.standalone) continue;

            let best = null;
            let bestArea = Infinity;

            for (const shape of scene.shapes) {
                if (shape.isContainer) continue;

                const inside = text.x >= shape.x - PAD && text.x <= shape.x + shape.width + PAD &&
                               text.y >= shape.y - PAD && text.y <= shape.y + shape.height + PAD;
                if (!inside) continue;

                // Innermost (smallest) enclosing shape wins, not the nearest centre -
                // otherwise a label lands in the big frame around its box.
                const area = shape.width * shape.height;
                if (area < bestArea) {
                    bestArea = area;
                    best = shape;
                }
            }

            if (best) {
                if (!candidates.has(best)) candidates.set(best, []);
                candidates.get(best).push(text);
            }
        }

        for (const [shape, texts] of candidates) {
            // All-or-nothing: Visio centers shape text in the tile. Folding
            // only the lines that fit leaves them sitting on top of the
            // overflow lines we left floating.
            if (!texts.every(t => this.labelFitsBox(t, shape, tuning))) continue;
            texts.sort((a, b) => (a.y - b.y) || (a.x - b.x));
            shape.text = texts.map(t => t.text).join('\n');
            // One run per source line so a heading keeps its own size/weight.
            shape.textRuns = texts.map(t => ({ text: t.text, style: t.style }));
            // The largest line drives the shape's overall font styling.
            shape.textStyle = texts.reduce((a, b) =>
                (b.style.fontSize || 0) > (a.style.fontSize || 0) ? b : a).style;
            for (const t of texts) t._associated = true;
        }

        // Keep unassociated texts as standalone text shapes
        scene.texts = scene.texts.filter(t => !t._associated);
    },

    /**
     * True when the line is short enough that Visio will not wrap it inside
     * the tile. Longer lines stay free-floating, matching the SVG overflow.
     *
     * @param {{text: string, style: any}} text
     * @param {{width: number}} shape
     * @param {{labelFitEm: number, labelFitEmBold: number, labelFitInset: number}} tuning
     */
    labelFitsBox(text, shape, tuning) {
        const size = (text.style && text.style.fontSize) || 14;
        const weight = text.style && text.style.fontWeight;
        const bold = weight === 'bold' || Number(weight) >= 700;
        const em = bold ? tuning.labelFitEmBold : tuning.labelFitEm;
        const width = [...String(text.text)].length * size * em;
        return width <= shape.width - tuning.labelFitInset;
    },

    /**
     * @param {{text: string, style: any}} text
     * @param {{labelFitEm: number, labelFitEmBold: number}} tuning
     */
    estimatedLineWidth(text, tuning) {
        const size = (text.style && text.style.fontSize) || 14;
        const weight = text.style && text.style.fontWeight;
        const bold = weight === 'bold' || Number(weight) >= 700;
        const em = bold ? tuning.labelFitEmBold : tuning.labelFitEm;
        return [...String(text.text)].length * size * em;
    },

    glueConnectors(scene, tuning) {
        // A fixed 30px radius is far too coarse on a small drawing and far too
        // tight on a large one, so scale it with the diagram's diagonal.
        // Try to detect which shapes connectors connect to
        for (const conn of scene.connectors) {
            if (conn.points.length < 2) continue;

            // Only derive an endpoint the parser could not supply itself.
            // Draw.io knows its edges' source and target exactly; guessing
            // geometrically would be strictly worse.
            if (conn.fromShape === null) {
                conn.fromShape = this.findNearestShape(scene, tuning, conn.points[0]);
            }
            if (conn.toShape === null) {
                conn.toShape = this.findNearestShape(scene, tuning, conn.points[conn.points.length - 1]);
            }
        }
    },

    /**
     * How far a connector endpoint is from a shape, in user units.
     *
     * A box counts as hit anywhere on or inside it: an arrow drawn into a box
     * is meant for that box. A frame is measured to its border instead,
     * because an arrow that merely crosses a subscription-sized rectangle is
     * not attached to it - and measuring a frame to its border is what stops
     * every endpoint on the page from gluing to the outermost one, which is
     * why frames used to be skipped altogether. An arrow that starts on a
     * frame's edge really does belong to that frame, and now that frames are
     * emitted as groups, gluing to one makes the arrow follow it.
     */
    glueDistance(shape, point) {
        const cx = shape.x + shape.width / 2;
        const cy = shape.y + shape.height / 2;
        const dx = Math.abs(point.x - cx) - shape.width / 2;
        const dy = Math.abs(point.y - cy) - shape.height / 2;

        if (dx <= 0 && dy <= 0) {
            return shape.isContainer ? Math.min(-dx, -dy) : 0;
        }
        return Math.hypot(Math.max(0, dx), Math.max(0, dy));
    },

    findNearestShape(scene, tuning, point) {
        const MAX_DIST = this.glueRadius(scene, tuning);
        let best = null;
        let bestRank = Infinity;
        let bestDist = MAX_DIST;
        let bestArea = Infinity;

        for (let i = 0; i < scene.shapes.length; i++) {
            const shape = scene.shapes[i];
            const dist = this.glueDistance(shape, point);
            if (dist >= MAX_DIST) continue;

            // A box is always the better anchor, even a little further away:
            // an arrow leaving a band of services under one of them belongs to
            // that service, not to the band. A frame is what is left when no
            // box is near enough - which is the only case where gluing to it
            // beats not gluing at all.
            const rank = shape.isContainer ? 1 : 0;
            const area = shape.width * shape.height;
            const closer = dist < bestDist - 0.5 ||
                (Math.abs(dist - bestDist) <= 0.5 && area < bestArea);
            if (rank < bestRank || (rank === bestRank && closer)) {
                bestRank = rank;
                bestDist = dist;
                bestArea = area;
                best = i;
            }
        }

        return best;
    }
};
