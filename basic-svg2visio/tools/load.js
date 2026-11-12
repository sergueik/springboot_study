// @ts-check
/**
 * Load the browser sources in Node exactly as the page does: concatenated in
 * <script> order, sharing one scope. Mirrors build.js, so a test can never
 * pass against a file layout the browser would not accept.
 */
const fs = require('fs');
const path = require('path');
const { JSDOM } = require('jsdom');

/** Same order as the <script> tags in index.html. */
const SOURCES = [
    'scene-model.js',
    'scene-layout.js',
    'svg-transform.js',
    'svg-style.js',
    'svg-parser.js',
    'drawio-parser.js',
    'vsdx-builder.js',
    'drawio-builder.js',
    'odg-builder.js'
];

const EXPORTS = [
    'SceneModel', 'SceneLayout', 'SvgTransform', 'SvgStyleResolver',
    'SvgParser', 'DrawioParser', 'VsdxBuilder', 'DrawioBuilder', 'OdgBuilder'
];

function load() {
    const root = path.join(__dirname, '..');
    const dom = new JSDOM('<!DOCTYPE html><html><body></body></html>');

    global.DOMParser = dom.window.DOMParser;
    global.JSZip = require(path.join(root, 'node_modules/jszip'));
    global.pako = require(path.join(root, 'node_modules/pako'));

    const bundle = SOURCES
        .map((f) => `/* ${f} */\n` + fs.readFileSync(path.join(root, f), 'utf8'))
        .join('\n');

    const api = new Function(`${bundle}\nreturn { ${EXPORTS.join(', ')} };`)();
    api.window = dom.window;
    api.domParser = new dom.window.DOMParser();
    return api;
}

module.exports = { load, SOURCES, EXPORTS };
