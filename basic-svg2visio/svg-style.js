// @ts-check
/* global SceneModel */
/**
 * The SVG style cascade: presentation attributes, <style> rules and inline
 * styles, plus inheritance down the element tree and paint-server resolution.
 *
 * Kept apart from the DOM walk because this is where class-styled diagrams
 * used to lose every fill, stroke and font: only attributes and inline styles
 * were read, so `class="box"` resolved to nothing.
 */

class SvgStyleResolver {
    /**
     * Presentation attributes that also exist as CSS properties.
     * These form the lowest layer of the cascade (specificity 0).
     */
    static PRESENTATION_ATTRS = Object.freeze([
        'fill', 'fill-opacity', 'stroke', 'stroke-width', 'stroke-dasharray',
        'stroke-opacity', 'stroke-linecap', 'stroke-linejoin', 'opacity',
        'font-size', 'font-family', 'font-weight', 'font-style', 'text-anchor',
        'rx', 'ry', 'color', 'visibility', 'stop-color'
    ]);

    /** Properties that SVG inherits from ancestor elements. */
    static INHERITED_PROPS = Object.freeze([
        'fill', 'fill-opacity', 'stroke', 'stroke-width', 'stroke-dasharray',
        'stroke-opacity', 'stroke-linecap', 'stroke-linejoin', 'font-size',
        'font-family', 'font-weight', 'font-style', 'text-anchor', 'color',
        'visibility'
    ]);

    /**
     * @param {Element} svgEl   the root <svg>
     * @param {(msg: string) => void} warn  conversion-caveat sink
     */
    constructor(svgEl, warn) {
        this.svgEl = svgEl;
        this.warn = warn || function () {};
        this.cssRules = [];
        /** @type {Object<string, Element>} */
        this.gradients = {};
        // resolve() walks the ancestor chain of every element, so the same
        // ancestors get resolved over and over; cache both layers.
        this._ownPropsCache = new Map();
        this._ruleCache = new Map();

        this.parseStyleSheet();
        this.svgEl.querySelectorAll('linearGradient, radialGradient').forEach((g) => {
            if (g.id) this.gradients[g.id] = g;
        });
    }

    /** Colour of a gradient stop, from either the attribute or its style. */
    stopColor(stop) {
        if (!stop) return null;
        const inline = stop.getAttribute('style');
        if (inline) {
            const decls = this.parseDeclarations(inline);
            if (decls['stop-color']) return decls['stop-color'];
        }
        const own = this.ownProps(stop);
        return stop.getAttribute('stop-color') || own['stop-color'] || null;
    }

    /**
     * Visio has no gradient fill here, so a gradient is flattened to the blend
     * of its first and last stop - far closer than the previous fallback,
     * which turned every gradient into solid black.
     */
    resolveGradient(id) {
        let g = this.gradients[id];
        if (!g) return null;

        // A gradient may inherit its stops from another one via href
        let stops = g.querySelectorAll('stop');
        let guard = 0;
        while (stops.length === 0 && guard++ < 8) {
            const href = g.getAttribute('href') || g.getAttribute('xlink:href');
            if (!href || href[0] !== '#') break;
            g = this.gradients[href.slice(1)];
            if (!g) break;
            stops = g.querySelectorAll('stop');
        }
        if (stops.length === 0) return null;

        const first = this.stopColor(stops[0]);
        const last = this.stopColor(stops[stops.length - 1]);
        if (!first) return null;
        if (!last || last === first) return first;

        const mix = this.blend(first, last);
        return mix || first;
    }

    /** Average two colours; returns null when either cannot be read as hex. */
    blend(c1, c2) {
        const toRgb = c => {
            const m = /^#([0-9a-f]{3}|[0-9a-f]{6})$/i.exec(c.trim());
            if (!m) return null;
            let h = m[1];
            if (h.length === 3) h = h[0] + h[0] + h[1] + h[1] + h[2] + h[2];
            return [parseInt(h.slice(0, 2), 16), parseInt(h.slice(2, 4), 16), parseInt(h.slice(4, 6), 16)];
        };
        const a = toRgb(c1), b = toRgb(c2);
        if (!a || !b) return null;
        const hex = n => Math.round(n).toString(16).padStart(2, '0');
        return '#' + hex((a[0] + b[0]) / 2) + hex((a[1] + b[1]) / 2) + hex((a[2] + b[2]) / 2);
    }

    /** Turn a paint value into a plain colour, resolving url(#gradient). */
    resolvePaint(value, what) {
        if (!value) return value;
        const url = /^url\(\s*['"]?#([^)'"\s]+)['"]?\s*\)/.exec(value.trim());
        if (!url) return value;

        const resolved = this.resolveGradient(url[1]);
        if (resolved) {
            this.warn(`gradient "#${url[1]}" was flattened to a single colour (${resolved})`);
            return resolved;
        }
        this.warn(`${what} references "#${url[1]}" (pattern or unknown paint) and was left unpainted`);
        return 'none';
    }


    /**
     * Collect the rules of every <style> block so that class-based SVGs
     * (.box, .fw, ...) keep their fills, strokes and fonts.
     */
    parseStyleSheet() {
        this.cssRules = [];
        this.svgEl.querySelectorAll('style').forEach(styleEl => {
            const css = styleEl.textContent.replace(/\/\*[\s\S]*?\*\//g, '');
            const ruleRe = /([^{}]+)\{([^{}]*)\}/g;
            let m;
            while ((m = ruleRe.exec(css)) !== null) {
                const decls = this.parseDeclarations(m[2]);
                if (Object.keys(decls).length === 0) continue;
                for (const sel of m[1].split(',').map(x => x.trim()).filter(Boolean)) {
                    const spec = this.selectorSpecificity(sel);
                    if (spec === null) continue; // combinators / pseudo classes unsupported
                    this.cssRules.push({ sel, spec, decls, order: this.cssRules.length });
                }
            }
        });
    }

    parseDeclarations(text) {
        const decls = {};
        for (const prop of text.split(';')) {
            const idx = prop.indexOf(':');
            if (idx < 0) continue;
            const key = prop.slice(0, idx).trim().toLowerCase();
            const val = prop.slice(idx + 1).trim();
            if (key && val) decls[key] = val;
        }
        return decls;
    }

    /** Specificity of a simple compound selector, or null when unsupported. */
    selectorSpecificity(sel) {
        if (/[\s>+~:[\]*]/.test(sel)) return null;
        const ids = (sel.match(/#[\w-]+/g) || []).length;
        const classes = (sel.match(/\.[\w-]+/g) || []).length;
        const tag = /^[a-zA-Z]/.test(sel) ? 1 : 0;
        return ids * 10000 + classes * 100 + tag;
    }

    selectorMatches(sel, el) {
        const tagPart = sel.match(/^[a-zA-Z][\w-]*/);
        if (tagPart && el.tagName.toLowerCase() !== tagPart[0].toLowerCase()) return false;
        for (const id of sel.match(/#[\w-]+/g) || []) {
            if (el.getAttribute('id') !== id.slice(1)) return false;
        }
        const classAttr = (el.getAttribute('class') || '').split(/\s+/);
        for (const cls of sel.match(/\.[\w-]+/g) || []) {
            if (classAttr.indexOf(cls.slice(1)) === -1) return false;
        }
        return true;
    }

    /**
     * CSS cascade for a single element:
     * presentation attributes < stylesheet rules (by specificity) < inline style.
     */
    ownProps(el) {
        const cached = this._ownPropsCache.get(el);
        if (cached) return cached;

        const props = {};
        for (const name of SvgStyleResolver.PRESENTATION_ATTRS) {
            const v = el.getAttribute(name);
            if (v !== null && v !== '') props[name] = v;
        }
        for (const r of this.matchingRules(el)) Object.assign(props, r.decls);

        const inline = el.getAttribute('style');
        if (inline) Object.assign(props, this.parseDeclarations(inline));

        this._ownPropsCache.set(el, props);
        return props;
    }

    /**
     * Stylesheet rules that apply to an element, weakest first. Elements that
     * share a tag/class/id share the answer, so this is resolved once per
     * distinct selector target rather than once per element.
     */
    matchingRules(el) {
        const rules = this.cssRules || [];
        if (rules.length === 0) return rules;

        const key = el.tagName.toLowerCase() + '|' +
                    (el.getAttribute('class') || '') + '|' +
                    (el.getAttribute('id') || '');
        let matched = this._ruleCache.get(key);
        if (!matched) {
            matched = rules
                .filter(r => this.selectorMatches(r.sel, el))
                .sort((a, b) => (a.spec - b.spec) || (a.order - b.order));
            this._ruleCache.set(key, matched);
        }
        return matched;
    }

    resolve(el) {
        // Ancestor chain (root first) so inherited properties resolve correctly.
        const chain = [];
        let node = el;
        while (node && node.nodeType === 1) {
            chain.unshift(node);
            if (node === this.svgEl) break;
            node = node.parentNode;
        }

        const props = {};
        for (let i = 0; i < chain.length; i++) {
            const own = this.ownProps(chain[i]);
            if (i === chain.length - 1) {
                Object.assign(props, own);
            } else {
                for (const key of SvgStyleResolver.INHERITED_PROPS) {
                    if (own[key] !== undefined) props[key] = own[key];
                }
            }
        }

        const num = (v, fallback) => {
            const n = parseFloat(v);
            return isNaN(n) ? fallback : n;
        };
        const dash = props['stroke-dasharray'];

        return Object.assign(SceneModel.defaultStyle(), {
            fill: this.resolvePaint(props['fill'] !== undefined ? props['fill'] : 'none', 'fill'),
            fillOpacity: num(props['fill-opacity'], 1),
            stroke: this.resolvePaint(props['stroke'] !== undefined ? props['stroke'] : 'none', 'stroke'),
            strokeWidth: num(props['stroke-width'], 1),
            strokeOpacity: num(props['stroke-opacity'], 1),
            strokeDasharray: (dash && dash !== 'none') ? dash : null,
            fontSize: num(props['font-size'], 14),
            fontFamily: props['font-family'] || 'Calibri',
            fontWeight: props['font-weight'] || 'normal',
            // SVG default is "start" - "middle" silently re-centres every left-aligned label.
            textAnchor: props['text-anchor'] || 'start',
            opacity: num(props['opacity'], 1),
            rx: num(props['rx'], 0),
            ry: num(props['ry'], 0)
        });
    }
}
