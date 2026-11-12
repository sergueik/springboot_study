#!/usr/bin/env node
// @ts-check
/**
 * Render every sample through draw.io and LibreOffice. Moving a glued box
 * in the .drawio must keep its edges (source/target stay set).
 *
 * A missing renderer is skipped, not failed.
 *
 *   node tools/verify-render.js
 */

const fs = require('fs');
const path = require('path');
const { spawnSync } = require('child_process');
const os = require('os');
const { load } = require('./load.js');

const ROOT = path.join(__dirname, '..');
const SAMPLES = path.join(ROOT, 'test-samples');
const DRAWIO = process.env.DRAWIO || path.join(os.homedir(), '.local/bin/drawio');
const SOFFICE = process.env.SOFFICE || 'soffice';

let passed = 0;
let failed = 0;
let skipped = 0;

function assert(cond, msg) {
    if (cond) {
        console.log(`  PASS: ${msg}`);
        passed++;
    } else {
        console.log(`  FAIL: ${msg}`);
        failed++;
    }
}

function skip(msg) {
    console.log(`  SKIP: ${msg}`);
    skipped++;
}

function have(cmd) {
    if (cmd.includes('/')) return fs.existsSync(cmd);
    return spawnSync('which', [cmd], { encoding: 'utf8' }).status === 0;
}

function run(cmd, args) {
    return spawnSync(cmd, args, { encoding: 'utf8', timeout: 120000 });
}

function parseSample(file) {
    const { SvgParser, DrawioParser } = load();
    const source = fs.readFileSync(path.join(SAMPLES, file), 'utf8');
    const isDrawio = /\.(drawio|xml)$/.test(file);
    return new (isDrawio ? DrawioParser : SvgParser)(source).parse();
}

function stem(file) {
    return file.replace(/[.]/g, '-');
}

function main() {
    const out = fs.mkdtempSync(path.join(os.tmpdir(), 'svgtovisio-render-'));
    const { DrawioBuilder, OdgBuilder, domParser } = load();
    const files = fs.readdirSync(SAMPLES).filter(f => /\.(svg|drawio|xml)$/.test(f)).sort();
    const hasDrawio = have(DRAWIO);
    const hasSoffice = have(SOFFICE);
    if (!hasDrawio) skip('draw.io CLI not installed');
    if (!hasSoffice) skip('LibreOffice soffice not installed');

    /** @type {string|null} */
    let flowchartDrawio = null;

    for (const file of files) {
        console.log(`\n--- ${file} ---`);
        const scene = parseSample(file);
        const base = stem(file);
        const drawioPath = path.join(out, base + '.drawio');
        const fodgPath = path.join(out, base + '.fodg');
        fs.writeFileSync(drawioPath, new DrawioBuilder(scene).build());
        fs.writeFileSync(fodgPath, new OdgBuilder(scene).build());
        assert(fs.statSync(drawioPath).size > 200, `${file} wrote a .drawio`);
        assert(fs.statSync(fodgPath).size > 200, `${file} wrote a .fodg`);
        if (file === 'flowchart.drawio') flowchartDrawio = drawioPath;

        if (hasDrawio) {
            const png = path.join(out, base + '-drawio.png');
            const r = run(DRAWIO, ['--no-sandbox', '--disable-gpu', '-x', '-f', 'png', '-o', png, drawioPath]);
            assert(r.status === 0 && fs.existsSync(png) && fs.statSync(png).size > 1000,
                `${file} draw.io PNG`);
        }
        if (hasSoffice) {
            const r = run(SOFFICE, ['--headless', '--convert-to', 'png', '--outdir', out, fodgPath]);
            const png = path.join(out, base + '.png');
            assert(r.status === 0 && fs.existsSync(png) && fs.statSync(png).size > 1000,
                `${file} LibreOffice PNG`);
        }
    }

    if (hasDrawio && flowchartDrawio) {
        console.log('\n--- glue move ---');
        const xml = fs.readFileSync(flowchartDrawio, 'utf8');
        const doc = domParser.parseFromString(xml, 'application/xml');
        const handle = [...doc.querySelectorAll('mxCell')].find(c =>
            /Handle Error/.test(c.getAttribute('value') || ''));
        assert(!!handle, 'flowchart has a Handle Error vertex');
        if (handle) {
            const id = handle.getAttribute('id');
            const geo = handle.querySelector('mxGeometry');
            const x0 = Number(geo && geo.getAttribute('x'));
            const edges = [...doc.querySelectorAll('mxCell')].filter(c => c.getAttribute('edge') === '1');
            const glued = edges.filter(e =>
                e.getAttribute('source') === id || e.getAttribute('target') === id);
            assert(glued.length >= 2, `Handle Error has glued edges (${glued.length})`);
            const movedXml = xml.replace(
                new RegExp(`(id="${id}"[\\s\\S]*?<mxGeometry x=")${x0}`),
                `$1${x0 + 220}`
            );
            assert(movedXml !== xml, 'Handle Error geometry was rewritten');
            const movedPath = path.join(out, 'flowchart-moved.drawio');
            fs.writeFileSync(movedPath, movedXml);
            const png = path.join(out, 'flowchart-moved.png');
            const r = run(DRAWIO, ['--no-sandbox', '--disable-gpu', '-x', '-f', 'png', '-o', png, movedPath]);
            assert(r.status === 0 && fs.existsSync(png) && fs.statSync(png).size > 1000,
                'flowchart glue-move PNG');
        }
    }

    console.log(`\noutput: ${out}`);
    console.log(`${passed} passed, ${failed} failed, ${skipped} skipped`);
    if (failed) process.exit(1);
}

main();
