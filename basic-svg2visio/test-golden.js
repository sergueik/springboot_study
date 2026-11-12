// @ts-check
/**
 * Golden-file tests: the parsed scene of every sample is snapshotted as JSON
 * and committed. Any change to a parser or to the layout heuristics shows up
 * as a reviewable diff instead of silently altering what lands in Visio.
 *
 * Hand-written assertions only catch what someone thought to assert; every
 * one of the conversion bugs this project has had would have surfaced here.
 *
 *   node test-golden.js              check against the committed snapshots
 *   UPDATE_GOLDEN=1 node test-golden.js   rewrite them after an intended change
 */

const fs = require('fs');
const path = require('path');
const { load } = require('./tools/load.js');

const { SceneModel, SvgParser, DrawioParser } = load();

const SAMPLES = path.join(__dirname, 'test-samples');
const GOLDEN = path.join(__dirname, 'test-golden');
const update = process.env.UPDATE_GOLDEN === '1';

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

/** Round every number so float noise never shows up as a diff. */
function stable(value) {
    if (typeof value === 'number') {
        return Number.isInteger(value) ? value : Math.round(value * 1e4) / 1e4;
    }
    if (Array.isArray(value)) return value.map(stable);
    if (value && typeof value === 'object') {
        const out = {};
        for (const key of Object.keys(value).sort()) out[key] = stable(value[key]);
        return out;
    }
    return value;
}

function parseSample(file) {
    const source = fs.readFileSync(path.join(SAMPLES, file), 'utf8');
    const isDrawio = /\.(drawio|xml)$/.test(file);
    return new (isDrawio ? DrawioParser : SvgParser)(source).parse();
}

if (!fs.existsSync(GOLDEN)) fs.mkdirSync(GOLDEN);

const files = fs.readdirSync(SAMPLES).filter(f => /\.(svg|drawio|xml)$/.test(f)).sort();
assert(files.length > 0, `found ${files.length} samples to snapshot`);

for (const file of files) {
    console.log(`\n--- ${file} ---`);

    let scene;
    try {
        scene = parseSample(file);
    } catch (e) {
        assert(false, `${file} parses without throwing (${e.message})`);
        continue;
    }

    // The scene model is the contract every consumer relies on
    const problems = SceneModel.validate(scene);
    assert(problems.length === 0,
        `${file} produces a valid scene${problems.length ? ': ' + problems.slice(0, 3).join('; ') : ''}`);

    const snapshot = JSON.stringify(stable(scene), null, 2) + '\n';
    // Keep the extension: flowchart.svg and flowchart.drawio are different inputs
    const goldenPath = path.join(GOLDEN, file + '.json');

    if (update || !fs.existsSync(goldenPath)) {
        fs.writeFileSync(goldenPath, snapshot);
        console.log(`  ${update ? 'updated' : 'created'}: ${path.relative(__dirname, goldenPath)}`);
        continue;
    }

    const expected = fs.readFileSync(goldenPath, 'utf8');
    if (snapshot === expected) {
        assert(true, `${file} matches its snapshot`);
        continue;
    }

    // Show the first few differing lines so the change is reviewable here
    const a = expected.split('\n');
    const b = snapshot.split('\n');
    const diff = [];
    for (let i = 0; i < Math.max(a.length, b.length) && diff.length < 12; i++) {
        if (a[i] !== b[i]) diff.push(`    line ${i + 1}:\n      - ${a[i]}\n      + ${b[i]}`);
    }
    assert(false, `${file} matches its snapshot\n${diff.join('\n')}\n` +
        `    (run UPDATE_GOLDEN=1 npm test if this change is intended)`);
}

console.log(`\n=== Golden files: ${passed} passed, ${failed} failed ===`);
process.exit(failed > 0 ? 1 : 0);
