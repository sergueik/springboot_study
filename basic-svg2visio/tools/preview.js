// @ts-check
/**
 * Render the generated Visio page back to an SVG, so you can see what Visio
 * will draw without owning Visio.
 *
 * This reads the built page XML - not the input - so it shows what actually
 * survived the conversion. Comparing it against the source SVG in a browser is
 * the fastest way to spot a conversion regression.
 *
 *   node tools/preview.js input.svg out.svg
 *   node tools/preview.js input.drawio out.svg
 */

const fs = require('fs');
const path = require('path');
const { load } = require('./load.js');

const PX_PER_INCH = 96;

function esc(text) {
    return String(text).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
}

/**
 * @param {string} inputPath
 * @returns {{svg: string, pageXml: string, warnings: string[]}}
 */
function preview(inputPath) {
    const { SvgParser, DrawioParser, VsdxBuilder, domParser, window } = load();

    const source = fs.readFileSync(inputPath, 'utf8');
    const isDrawio = /\.(drawio|xml)$/.test(inputPath);
    const scene = new (isDrawio ? DrawioParser : SvgParser)(source).parse();

    const builder = new VsdxBuilder(scene);
    const pageXml = builder._page1();
    const doc = domParser.parseFromString(pageXml, 'application/xml');

    const pageW = builder.pageWidthInches;
    const pageH = builder.pageHeightInches;
    // Every lookup is scoped to the shape's own children: a group holds its
    // members' cells too, and a deep query would read a child's PinX as the
    // group's own.
    const own = (shape, tag, name) => Array.from(shape.children).find(
        c => c.nodeName === tag && c.getAttribute('N') === name) || null;
    const kids = (parent, tag) => (parent
        ? Array.from(parent.children).filter(c => c.nodeName === tag) : []);
    const cell = (shape, name) => {
        const c = own(shape, 'Cell', name);
        return c ? c.getAttribute('V') : null;
    };
    const num = (shape, name, fallback) => {
        const v = parseFloat(cell(shape, name));
        return isNaN(v) ? fallback : v;
    };
    const rowCell = (row, name) => {
        const c = own(row, 'Cell', name);
        return c ? c.getAttribute('V') : null;
    };
    const rowNum = (row, name, fallback) => {
        const v = parseFloat(rowCell(row, name));
        return isNaN(v) ? fallback : v;
    };

    let out = `<svg xmlns="http://www.w3.org/2000/svg" width="${pageW * PX_PER_INCH}" ` +
        `height="${pageH * PX_PER_INCH}" viewBox="0 0 ${pageW * PX_PER_INCH} ${pageH * PX_PER_INCH}" ` +
        `font-family="Arial, Helvetica, sans-serif">` +
        `<defs><marker id="a" markerWidth="10" markerHeight="10" refX="9" refY="5" orient="auto">` +
        `<path d="M0,0 L10,5 L0,10 z" fill="context-stroke"/></marker></defs>` +
        `<rect width="100%" height="100%" fill="#ffffff"/>`;

    // Page inches (y up) to SVG pixels (y down)
    const px = (x) => x * PX_PER_INCH;
    const py = (y) => (pageH - y) * PX_PER_INCH;

    /**
     * Draw one shape, then whatever is nested inside it.
     *
     * @param {any} shape
     * @param {number} ox origin of the enclosing group, in page inches
     * @param {number} oy
     */
    function render(shape, ox, oy) {
        const wIn = num(shape, 'Width', 0);
        const hIn = num(shape, 'Height', 0);
        const locX = num(shape, 'LocPinX', wIn / 2);
        const locY = num(shape, 'LocPinY', hIn / 2);
        // The group chain is always axis-aligned - nesting into a rotated
        // frame is refused upstream - so a parent contributes a translation.
        const leftIn = ox + num(shape, 'PinX', 0) - locX;
        const bottomIn = oy + num(shape, 'PinY', 0) - locY;

        const w = px(wIn);
        const h = px(hIn);
        const left = px(leftIn);
        const top = py(bottomIn + hIn);

        const fillPattern = num(shape, 'FillPattern', 1);
        const linePattern = num(shape, 'LinePattern', 1);
        const fill = fillPattern === 0 ? 'none' : (cell(shape, 'FillForegnd') || 'none');
        const stroke = linePattern === 0 ? 'none' : (cell(shape, 'LineColor') || '#000000');
        const lineWeight = num(shape, 'LineWeight', 1 / PX_PER_INCH) * PX_PER_INCH;
        const dash = { 2: '6 4', 3: '2 3', 4: '10 4 2 4' }[linePattern] || null;
        const dashAttr = dash ? ` stroke-dasharray="${dash}"` : '';
        const angle = num(shape, 'Angle', 0);
        const rotate = angle
            ? ` transform="rotate(${(-angle * 180 / Math.PI).toFixed(3)} ${left + w / 2} ${top + h / 2})"`
            : '';

        const geometry = own(shape, 'Section', 'Geometry');
        const geomRows = kids(geometry, 'Row');

        // Character and paragraph runs, referenced from <Text> by <cp>/<pp>
        const charRows = kids(own(shape, 'Section', 'Character'), 'Row').map(r => ({
            // The Size cell is in inches, like every other length in the page
            size: rowNum(r, 'Size', 0.14) * PX_PER_INCH,
            color: rowCell(r, 'Color') || '#000000',
            bold: rowCell(r, 'Style') === '1'
        }));
        const paraRows = kids(own(shape, 'Section', 'Paragraph'), 'Row')
            .map(r => rowNum(r, 'HorzAlign', 1));

        const runs = [];
        const textEl = Array.from(shape.children).find(c => c.nodeName === 'Text');
        if (textEl) {
            let charIx = 0;
            let paraIx = 0;
            for (const node of Array.from(textEl.childNodes)) {
                if (node.nodeType === 1) {
                    const name = node.nodeName.toLowerCase();
                    if (name === 'cp') charIx = parseInt(node.getAttribute('IX'), 10) || 0;
                    if (name === 'pp') paraIx = parseInt(node.getAttribute('IX'), 10) || 0;
                } else if (node.nodeType === 3) {
                    for (const line of node.textContent.split('\n')) {
                        if (line.length) {
                            runs.push({ text: line, char: charRows[charIx] || charRows[0], align: paraRows[paraIx] });
                        }
                    }
                }
            }
        }

        // A 1-D shape carries its route in a frame that runs along its own
        // endpoints, so its geometry has to be rotated, not just offset.
        if (cell(shape, 'OneD') === '1') {
            const cos = Math.cos(angle);
            const sin = Math.sin(angle);
            const pinXIn = ox + num(shape, 'PinX', 0);
            const pinYIn = oy + num(shape, 'PinY', 0);
            const points = [];
            for (const row of geomRows) {
                const x = parseFloat(rowCell(row, 'X'));
                const y = parseFloat(rowCell(row, 'Y'));
                if (isNaN(x) || isNaN(y)) continue;
                const dx = x - locX;
                const dy = y - locY;
                points.push(`${px(pinXIn + dx * cos - dy * sin)},${py(pinYIn + dx * sin + dy * cos)}`);
            }
            if (points.length >= 2) {
                const head = cell(shape, 'EndArrow') !== '0' ? ' marker-end="url(#a)"' : '';
                const tail = cell(shape, 'BeginArrow') !== '0' ? ' marker-start="url(#a)"' : '';
                out += `<polyline points="${points.join(' ')}" fill="none" stroke="${stroke}" ` +
                    `stroke-width="${lineWeight}"${dashAttr}${head}${tail}/>`;
            }
            return;
        }

        const isEllipse = geomRows.some(r => r.getAttribute('T') === 'Ellipse');
        if (fillPattern !== 0 || linePattern !== 0) {
            out += isEllipse
                ? `<ellipse cx="${left + w / 2}" cy="${top + h / 2}" rx="${w / 2}" ry="${h / 2}" ` +
                  `fill="${fill}" stroke="${stroke}" stroke-width="${lineWeight}"${dashAttr}${rotate}/>`
                : `<rect x="${left}" y="${top}" width="${w}" height="${h}" rx="${num(shape, 'Rounding', 0) * PX_PER_INCH}" ` +
                  `fill="${fill}" stroke="${stroke}" stroke-width="${lineWeight}"${dashAttr}${rotate}/>`;
        }

        if (runs.length) {
            // Text sits in its own block, which is at least as wide as the
            // text needs - so a long label overflows on one line here the way
            // it will in Visio, rather than silently fitting the shape.
            const txtW = num(shape, 'TxtWidth', wIn) * PX_PER_INCH;
            const txtPinX = num(shape, 'TxtPinX', wIn / 2) * PX_PER_INCH;
            const txtLocPinX = num(shape, 'TxtLocPinX', txtW / PX_PER_INCH / 2) * PX_PER_INCH;
            const txtLeft = left + txtPinX - txtLocPinX;
            const margin = num(shape, 'LeftMargin', 3 / PX_PER_INCH) * PX_PER_INCH;
            const lineHeight = (charRows[0] ? charRows[0].size : 12) * 1.25;
            let y = top + h / 2 - (runs.length * lineHeight) / 2 + lineHeight * 0.75;
            for (const run of runs) {
                const align = run.align === undefined ? 1 : run.align;
                const anchor = align === 0 ? 'start' : (align === 2 ? 'end' : 'middle');
                const x = align === 0 ? txtLeft + margin
                    : (align === 2 ? txtLeft + txtW - margin : txtLeft + txtW / 2);
                out += `<text x="${x}" y="${y}" font-size="${run.char.size}" fill="${run.char.color}" ` +
                    `text-anchor="${anchor}"${run.char.bold ? ' font-weight="bold"' : ''}>${esc(run.text)}</text>`;
                y += lineHeight;
            }
        }

        // Group members are placed in the group's own coordinate system
        for (const child of kids(Array.from(shape.children).find(c => c.nodeName === 'Shapes'), 'Shape')) {
            render(child, leftIn, bottomIn);
        }
    }

    for (const shape of kids(kids(doc.documentElement, 'Shapes')[0], 'Shape')) {
        render(shape, 0, 0);
    }

    out += '</svg>';
    window.close();
    return { svg: out, pageXml, warnings: scene.warnings };
}

module.exports = { preview };

if (require.main === module) {
    const [input, output] = process.argv.slice(2);
    if (!input) {
        console.error('usage: node tools/preview.js <input.svg|.drawio> [out.svg]');
        process.exit(2);
    }
    const result = preview(input);
    const target = output || path.basename(input).replace(/\.[^.]+$/, '') + '.preview.svg';
    fs.writeFileSync(target, result.svg);
    console.log(`wrote ${target}`);
    if (result.warnings.length) {
        console.log('\nthe .vsdx cannot reproduce:');
        for (const w of result.warnings) console.log('  - ' + w);
    }
}
