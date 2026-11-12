#!/usr/bin/env node
// @ts-check
/**
 * Convert an SVG or .drawio file to .vsdx, .drawio and .fodg.
 *
 *   node tools/export.js diagram.svg [outdir]
 */

const fs = require('fs');
const path = require('path');
const { load } = require('./load.js');

async function main() {
    const input = process.argv[2];
    if (!input) {
        console.error('Usage: node tools/export.js <file.svg|file.drawio> [outdir]');
        process.exit(1);
    }
    const abs = path.resolve(input);
    const outDir = path.resolve(process.argv[3] || path.dirname(abs));
    fs.mkdirSync(outDir, { recursive: true });

    const { SvgParser, DrawioParser, VsdxBuilder, DrawioBuilder, OdgBuilder } = load();
    const source = fs.readFileSync(abs, 'utf8');
    const isDrawio = /\.(drawio|xml)$/.test(abs);
    const scene = new (isDrawio ? DrawioParser : SvgParser)(source).parse();
    const base = path.basename(abs).replace(/\.(svg|drawio|xml)$/i, '');

    const drawio = new DrawioBuilder(scene).build();
    const fodg = new OdgBuilder(scene).build();
    fs.writeFileSync(path.join(outDir, base + '.drawio'), drawio);
    fs.writeFileSync(path.join(outDir, base + '.fodg'), fodg);

    const builder = new VsdxBuilder(scene);
    const blob = await builder.build();
    const buf = Buffer.from(await blob.arrayBuffer());
    fs.writeFileSync(path.join(outDir, base + '.vsdx'), buf);

    console.log(`${path.basename(abs)} -> ${base}.vsdx, ${base}.drawio, ${base}.fodg`);
    console.log(`  shapes=${scene.shapes.length} connectors=${scene.connectors.length} texts=${scene.texts.length}`);
}

main().catch((err) => {
    console.error(err);
    process.exit(1);
});
