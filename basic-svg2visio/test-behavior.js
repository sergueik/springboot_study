// @ts-check
/**
 * Simulate the grouping and glue Visio would use, without Visio.
 *
 * A real drag in Visio cannot run here. What we can check is the model that
 * makes that drag work: children live in a group's local coordinates (so
 * moving the group's pin moves them in page space), 1-D connectors that
 * claim glue name a real sheet and watch it with XFTRIGGER, and a labelled
 * tile is not wrapped in its own group.
 */

const fs = require('fs');
const path = require('path');
const { load } = require('./tools/load.js');

const { SvgParser, DrawioParser, VsdxBuilder, domParser } = load();

let passed = 0;
let failed = 0;

function assert(condition, msg) {
    if (condition) {
        console.log(`  PASS: ${msg}`);
        passed++;
    } else {
        console.log(`  FAIL: ${msg}`);
        failed++;
    }
}

function cell(el, name) {
    const nodes = el.getElementsByTagName('Cell');
    for (let i = 0; i < nodes.length; i++) {
        if (nodes[i].parentNode === el && nodes[i].getAttribute('N') === name) {
            return nodes[i];
        }
    }
    return null;
}

function cellNum(el, name) {
    const c = cell(el, name);
    return c ? parseFloat(c.getAttribute('V')) : null;
}

function cellF(el, name) {
    const c = cell(el, name);
    return c ? c.getAttribute('F') : null;
}

function directShapes(parent) {
    const out = [];
    for (let n = parent.firstChild; n; n = n.nextSibling) {
        if (n.nodeType !== 1) continue;
        if (n.localName === 'Shape') out.push(n);
        if (n.localName === 'Shapes') {
            for (let c = n.firstChild; c; c = c.nextSibling) {
                if (c.nodeType === 1 && c.localName === 'Shape') out.push(c);
            }
        }
    }
    return out;
}

function textOf(shape) {
    const t = shape.getElementsByTagName('Text')[0];
    return t ? t.textContent : '';
}

function boxOf(shape, parentLeft, parentBottom) {
    const w = cellNum(shape, 'Width') || 0;
    const h = cellNum(shape, 'Height') || 0;
    const pinX = cellNum(shape, 'PinX') || 0;
    const pinY = cellNum(shape, 'PinY') || 0;
    const locX = cellNum(shape, 'LocPinX') || w / 2;
    const locY = cellNum(shape, 'LocPinY') || h / 2;
    const left = parentLeft + pinX - locX;
    const bottom = parentBottom + pinY - locY;
    return { left, bottom, right: left + w, top: bottom + h, w, h, pinX, pinY };
}

function walk(shapes, parentLeft, parentBottom, depth, acc) {
    for (const s of shapes) {
        const b = boxOf(s, parentLeft, parentBottom);
        const rec = {
            el: s,
            id: s.getAttribute('ID'),
            type: s.getAttribute('Type'),
            oneD: cell(s, 'OneD') !== null,
            text: textOf(s).replace(/\s+/g, ' ').trim().slice(0, 40),
            depth,
            box: b,
            kids: directShapes(s)
        };
        acc.push(rec);
        if (rec.kids.length) walk(rec.kids, b.left, b.bottom, depth + 1, acc);
    }
}

function parsePage(xml) {
    const doc = domParser.parseFromString(xml, 'application/xml');
    const page = doc.getElementsByTagName('PageContents')[0] || doc.documentElement;
    const top = [];
    const all = page.getElementsByTagName('Shapes');
    const root = all[0];
    const recs = [];
    if (root) {
        for (let c = root.firstChild; c; c = c.nextSibling) {
            if (c.nodeType === 1 && c.localName === 'Shape') top.push(c);
        }
        walk(top, 0, 0, 0, recs);
    }
    const connects = [];
    const cn = doc.getElementsByTagName('Connect');
    for (let i = 0; i < cn.length; i++) {
        connects.push({
            from: cn[i].getAttribute('FromSheet'),
            to: cn[i].getAttribute('ToSheet'),
            fromCell: cn[i].getAttribute('FromCell'),
            toCell: cn[i].getAttribute('ToCell')
        });
    }
    return { recs, connects };
}

function byId(recs) {
    const m = {};
    for (const r of recs) m[r.id] = r;
    return m;
}

/** Descendants: records after the group until depth <= group.depth */
function descendants(recs, group) {
    const i = recs.indexOf(group);
    const out = [];
    for (let j = i + 1; j < recs.length; j++) {
        if (recs[j].depth <= group.depth) break;
        out.push(recs[j]);
    }
    return out;
}

function checkFile(name, xml) {
    console.log(`\n--- ${name} ---`);
    const { recs, connects } = parsePage(xml);
    const ids = byId(recs);
    const groups = recs.filter(r => r.type === 'Group');

    assert(groups.length > 0 || recs.length < 8,
        `${name} has ${groups.length} groups`);

    for (const g of groups) {
        const kids = descendants(recs, g);
        const boxes = kids.filter(k => !k.oneD && cellNum(k.el, 'FillPattern') === 1);
        let inside = 0;
        for (const k of boxes) {
            const slack = 0.2;
            const ok = k.box.left >= g.box.left - slack &&
                k.box.bottom >= g.box.bottom - slack &&
                k.box.right <= g.box.right + slack &&
                k.box.top <= g.box.top + slack;
            if (ok) inside++;
        }
        assert(boxes.length === 0 || inside === boxes.length,
            `${name} group ${g.id} keeps its ${boxes.length} filled tiles inside (${inside} were)`);

        const direct = kids.filter(k => k.depth === g.depth + 1);
        assert(direct.every(k => (cellNum(k.el, 'PinX') || 0) <= g.box.w + 0.5),
            `${name} group ${g.id} children use local PinX, so dragging the frame takes them along`);
    }

    const oneD = recs.filter(r => r.oneD);
    const gluedIds = new Set(connects.map(c => c.from));
    for (const c of oneD) {
        if (!gluedIds.has(c.id)) continue;
        const beg = cellF(c.el, 'BegTrigger') || '';
        const end = cellF(c.el, 'EndTrigger') || '';
        const links = connects.filter(x => x.from === c.id);
        const ok = links.every(x => {
            const f = x.fromCell === 'BeginX' ? beg : end;
            return f.indexOf('Sheet.' + x.to + '!') !== -1;
        });
        assert(ok,
            `${name} connector ${c.id} XFTRIGGER names the glued sheets`);
        assert(links.every(x => ids[x.to]),
            `${name} connector ${c.id} glues to sheets that exist`);
    }

    const tileAsGroup = groups.filter(g => {
        if (cellNum(g.el, 'FillPattern') !== 1) return false;
        const direct = directShapes(g.el);
        return direct.length >= 2 && direct.every(k =>
            cellNum(k, 'FillPattern') === 0 && cell(k, 'OneD') === null);
    });
    assert(tileAsGroup.length === 0,
        `${name} a labelled tile is not a group of its own lines (${tileAsGroup.length} were)`);
}

(async () => {
    const samples = [
        ['paloalto-hub-v1.svg', SvgParser],
        ['nested-frames.svg', SvgParser],
        ['koeln-bgp.drawio', DrawioParser],
        ['architecture.svg', SvgParser]
    ];
    const dir = path.join(__dirname, 'test-samples');
    for (const [file, Parser] of samples) {
        const scene = new Parser(fs.readFileSync(path.join(dir, file), 'utf8')).parse();
        const xml = new VsdxBuilder(scene)._page1();
        checkFile(file, xml);
    }

    console.log(`\n=== Behavior: ${passed} passed, ${failed} failed ===`);
    process.exit(failed > 0 ? 1 : 0);
})();
