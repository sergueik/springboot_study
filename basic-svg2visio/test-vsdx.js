// @ts-check
/**
 * Convert every sample to a .vsdx and check the things that made Visio
 * show empty frames or wrap labels: missing DisplayMode, raw line feeds
 * in <Text>, formulas on cells Visio owns, a broken package.
 *
 * Scene snapshots (test-golden.js) cannot catch this: they never look at
 * the ZIP Visio opens.
 */

const fs = require('fs');
const path = require('path');
const { load } = require('./tools/load.js');

const { SceneModel, SvgParser, DrawioParser, VsdxBuilder, domParser } = load();

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
    return err ? err.textContent.slice(0, 120) : null;
}

function parseSample(file) {
    const source = fs.readFileSync(path.join(SAMPLES, file), 'utf8');
    const isDrawio = /\.(drawio|xml)$/.test(file);
    return new (isDrawio ? DrawioParser : SvgParser)(source).parse();
}

/**
 * @param {string} pageXml
 * @param {string} file
 */
function checkPage(pageXml, file) {
    const err = xmlError(pageXml);
    assert(err === null, `${file} page1.xml is well-formed${err ? ' (' + err + ')' : ''}`);

    const groups = (pageXml.match(/Type="Group"/g) || []).length;
    const display = (pageXml.match(/<Cell N="DisplayMode" V="1"\/>/g) || []).length;
    assert(display === groups,
        `${file} every group has DisplayMode=1 (${display}/${groups})`);

    const texts = pageXml.match(/<Text>[\s\S]*?<\/Text>/g) || [];
    const rawBreaks = texts.filter(t => t.indexOf('&#10;') !== -1 || t.indexOf('\n') !== -1);
    assert(rawBreaks.length === 0,
        `${file} <Text> has no line feeds (${rawBreaks.length} of ${texts.length})`);

    const oneDBlocks = pageXml.split(/<Shape /).filter(s => /N="OneD" V="1"/.test(s));
    const bad1d = oneDBlocks.filter(s =>
        /<Cell N="PinX"[^>]*F=/.test(s) || /<Cell N="Angle"[^>]*F=/.test(s));
    assert(bad1d.length === 0,
        `${file} 1-D transforms are values, not formulas (${oneDBlocks.length} connectors)`);
    const flat = oneDBlocks.filter(s => {
        const m = s.match(/N="Height" V="([^"]+)"/);
        return m && Number(m[1]) < 0.01;
    });
    assert(flat.length === 0,
        `${file} no 1-D connector is flat (Height 0)`);

    const locked = (pageXml.match(/<Cell N="TxtWidth"[^>]*F="Width"\/>/g) || []).length;
    assert(locked > 0 || !/<Cell N="TxtWidth"/.test(pageXml),
        `${file} TxtWidth follows Width so shrinking a field wraps the text (${locked})`);
}

(async () => {
    const files = fs.readdirSync(SAMPLES).filter(f => /\.(svg|drawio|xml)$/.test(f)).sort();
    assert(files.length >= 7, `found ${files.length} samples to convert`);

    const required = ['[Content_Types].xml', '_rels/.rels', 'visio/document.xml',
                      'visio/pages/pages.xml', 'visio/pages/page1.xml'];

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

        let builder;
        let pageXml;
        try {
            builder = new VsdxBuilder(scene);
            pageXml = builder._page1();
        } catch (e) {
            assert(false, `${file} builds a page (${e.message})`);
            continue;
        }
        checkPage(pageXml, file);

        try {
            const blob = await builder.build();
            const zip = await JSZip.loadAsync(await blob.arrayBuffer());
            for (const part of required) {
                assert(zip.file(part) !== null, `${file} package contains ${part}`);
            }
            const page = await zip.file('visio/pages/page1.xml').async('string');
            assert(page.indexOf('<PageContents') !== -1, `${file} package page is a Visio page`);
            const doc = await zip.file('visio/document.xml').async('string');
            const used = [...new Set([...page.matchAll(/N="Font" V="(\d+)"/g)].map(m => m[1]))];
            const faces = {};
            for (const m of doc.matchAll(/<FaceName ID="(\d+)" Name="([^"]+)"/g)) {
                faces[m[1]] = m[2];
            }
            assert(used.every(id => faces[id] !== undefined),
                `${file} every Font id is declared (${used.join(',') || 'none'})`);
        } catch (e) {
            assert(false, `${file} package builds (${e.message})`);
        }
    }

    console.log(`\n=== VSDX results: ${passed} passed, ${failed} failed ===`);
    process.exit(failed > 0 ? 1 : 0);
})();
