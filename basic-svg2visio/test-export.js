// @ts-check
/**
 * Native draw.io and LibreOffice exports: well-formed XML, glue, groups.
 */

const fs = require('fs');
const path = require('path');
const { load } = require('./tools/load.js');

const { SceneModel, SvgParser, DrawioParser, DrawioBuilder, OdgBuilder, domParser } = load();
const SAMPLES = path.join(__dirname, 'test-samples');

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

function xmlError(xml) {
    const doc = domParser.parseFromString(xml, 'application/xml');
    const err = doc.querySelector('parsererror');
    return err ? err.textContent.slice(0, 160) : null;
}

function parseSample(file) {
    const source = fs.readFileSync(path.join(SAMPLES, file), 'utf8');
    const isDrawio = /\.(drawio|xml)$/.test(file);
    return new (isDrawio ? DrawioParser : SvgParser)(source).parse();
}

function checkDrawio(xml, scene, file) {
    const err = xmlError(xml);
    assert(err === null, `${file} .drawio is well-formed${err ? ' (' + err + ')' : ''}`);

    const doc = domParser.parseFromString(xml, 'application/xml');
    const cells = [...doc.querySelectorAll('mxCell')];
    const edges = cells.filter(c => c.getAttribute('edge') === '1');
    const vertices = cells.filter(c => c.getAttribute('vertex') === '1');
    assert(vertices.length >= scene.shapes.length,
        `${file} draw.io has a vertex per shape (${vertices.length} >= ${scene.shapes.length})`);
    assert(edges.length === scene.connectors.length,
        `${file} draw.io has an edge per connector (${edges.length}/${scene.connectors.length})`);

    const glued = scene.connectors.filter(c => c.fromShape != null && c.toShape != null).length;
    const gluedEdges = edges.filter(e => e.getAttribute('source') && e.getAttribute('target')).length;
    assert(gluedEdges === glued,
        `${file} draw.io glued edges match scene (${gluedEdges}/${glued})`);

    const groups = scene.shapes.filter(s => s.isContainer).length;
    const containers = cells.filter(c => /container=1/.test(c.getAttribute('style') || '')).length;
    assert(containers === groups,
        `${file} draw.io container cells match frames (${containers}/${groups})`);

    const nested = cells.filter(c => {
        const p = c.getAttribute('parent');
        return p && p !== '0' && p !== '1';
    }).length;
    if (groups > 0) {
        assert(nested > 0, `${file} draw.io nests children under frames (${nested})`);
    }

    const edgeOnPage = edges.filter(e => e.getAttribute('parent') === '1').length;
    assert(edgeOnPage === edges.length,
        `${file} draw.io edges sit on the page (${edgeOnPage}/${edges.length})`);

    const withWaypoints = edges.filter(e => e.querySelector('Array')).length;
    const poly = scene.connectors.filter(c => (c.points || []).length > 2).length;
    assert(withWaypoints === poly,
        `${file} draw.io keeps polyline waypoints (${withWaypoints}/${poly})`);
    assert(/whiteSpace=wrap/.test(xml), `${file} draw.io wraps text when the box shrinks`);
}

function checkOdg(xml, scene, file) {
    const err = xmlError(xml);
    assert(err === null, `${file} .fodg is well-formed${err ? ' (' + err + ')' : ''}`);

    const connectors = (xml.match(/<draw:connector\b/g) || []).length;
    const polylines = (xml.match(/<draw:polyline\b/g) || []).length;
    const routes = connectors + polylines;
    assert(routes === scene.connectors.length,
        `${file} fodg has a route per edge (${connectors} connectors + ${polylines} polylines = ${routes}/${scene.connectors.length})`);
    const scenePoly = scene.connectors.filter(c => (c.points || []).length > 2).length;
    assert(polylines === scenePoly,
        `${file} fodg polylines match multi-point connectors (${polylines}/${scenePoly})`);

    const glueable = (c) => {
        if ((c.points || []).length > 2) return false;
        const from = c.fromShape != null && !scene.shapes[c.fromShape].isContainer;
        const to = c.toShape != null && !scene.shapes[c.toShape].isContainer;
        return from && to;
    };
    const froms = scene.connectors.filter(c => glueable(c) && c.fromShape != null).length;
    const tos = scene.connectors.filter(c => glueable(c) && c.toShape != null).length;
    const start = (xml.match(/draw:start-shape="/g) || []).length;
    const end = (xml.match(/draw:end-shape="/g) || []).length;
    assert(start === froms && end === tos,
        `${file} fodg two-point connectors name start and end shapes (${start}/${froms} start, ${end}/${tos} end)`);

    const groups = scene.shapes.filter(s => s.isContainer).length;
    const gs = (xml.match(/<draw:g>/g) || []).length;
    assert(gs === groups, `${file} fodg groups match frames (${gs}/${groups})`);
    if (scene.shapes.some(s => !s.isContainer && s.text)) {
        assert(/draw:fit-to-size="true"/.test(xml),
            `${file} fodg shrinks tile text when the box shrinks`);
    }
}

(function main() {
    const files = fs.readdirSync(SAMPLES).filter(f => /\.(svg|drawio|xml)$/.test(f)).sort();
    assert(files.length >= 7, `found ${files.length} samples`);

    for (const file of files) {
        console.log(`\n--- ${file} ---`);
        let scene;
        try {
            scene = parseSample(file);
        } catch (e) {
            assert(false, `${file} parses (${e.message})`);
            continue;
        }
        const problems = SceneModel.validate(scene);
        assert(problems.length === 0,
            `${file} scene is valid${problems.length ? ': ' + problems[0] : ''}`);

        let drawio;
        try {
            drawio = new DrawioBuilder(scene).build();
        } catch (e) {
            assert(false, `${file} draw.io builds (${e.message})`);
            continue;
        }
        checkDrawio(drawio, scene, file);

        let fodg;
        try {
            fodg = new OdgBuilder(scene).build();
        } catch (e) {
            assert(false, `${file} fodg builds (${e.message})`);
            continue;
        }
        checkOdg(fodg, scene, file);
    }

    // Tiny scene: moving a box in the mxGraph must leave the edge glued.
    {
        console.log('\n--- glue contract ---');
        const scene = SceneModel.normalise({
            viewBox: { x: 0, y: 0, width: 400, height: 200 },
            shapes: [
                { type: 'rect', x: 10, y: 40, width: 80, height: 40, text: 'A' },
                { type: 'rect', x: 250, y: 40, width: 80, height: 40, text: 'B' }
            ],
            connectors: [{
                type: 'line',
                points: [{ x: 90, y: 60 }, { x: 250, y: 60 }],
                arrowEnd: true,
                fromShape: 0,
                toShape: 1
            }],
            texts: []
        });
        const xml = new DrawioBuilder(scene).build();
        const doc = domParser.parseFromString(xml, 'application/xml');
        const edge = [...doc.querySelectorAll('mxCell')].find(c => c.getAttribute('edge') === '1');
        assert(!!edge && edge.getAttribute('source') && edge.getAttribute('target'),
            'two-box scene edge has source and target');
        const fodg = new OdgBuilder(scene).build();
        assert(/draw:start-shape="id_s0"/.test(fodg) && /draw:end-shape="id_s1"/.test(fodg),
            'two-box fodg connector names both boxes');
        const edgeParent = edge && edge.getAttribute('parent');
        assert(edgeParent === '1', 'two-box draw.io edge is parented to the page');
    }

    console.log(`\n${passed} passed, ${failed} failed`);
    if (failed) process.exit(1);
})();
