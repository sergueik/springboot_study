#!/usr/bin/env node
/**
 * Build script - inlines ALL JS (including CDN libs) into index.html.
 * Produces a fully self-contained single HTML file with zero external dependencies.
 *
 * Usage: node build.js
 * Output: dist/index.html
 */

const fs = require('fs');
const path = require('path');

const srcDir = __dirname;
const distDir = path.join(srcDir, 'dist');

if (!fs.existsSync(distDir)) {
    fs.mkdirSync(distDir);
}

// Map CDN URLs to local node_modules files
const cdnMap = {
    'https://cdnjs.cloudflare.com/ajax/libs/jszip/3.10.1/jszip.min.js':
        path.join(srcDir, 'node_modules/jszip/dist/jszip.min.js'),
    'https://cdnjs.cloudflare.com/ajax/libs/pako/2.1.0/pako.min.js':
        path.join(srcDir, 'node_modules/pako/dist/pako.min.js'),
};

let html = fs.readFileSync(path.join(srcDir, 'index.html'), 'utf8');

// Inline ALL script src tags (CDN + local)
const scriptRegex = /<script src="([^"]+)">\s*<\/script>/g;
html = html.replace(scriptRegex, (match, src) => {
    let filePath;

    if (src.startsWith('http')) {
        // CDN script — resolve to local copy
        filePath = cdnMap[src];
        if (!filePath || !fs.existsSync(filePath)) {
            console.error(`WARNING: No local copy for CDN ${src}, keeping external ref`);
            return match;
        }
    } else {
        // Local script
        filePath = path.join(srcDir, src);
        if (!fs.existsSync(filePath)) {
            console.error(`WARNING: ${src} not found`);
            return match;
        }
    }

    const code = fs.readFileSync(filePath, 'utf8');
    const name = path.basename(filePath);
    console.log(`Inlined: ${name} (${(code.length / 1024).toFixed(1)} KB)`);
    return `<script>/* ${name} */\n${code}\n</script>`;
});

fs.writeFileSync(path.join(distDir, 'index.html'), html);
console.log(`\nBuilt: dist/index.html (${(html.length / 1024).toFixed(1)} KB)`);
