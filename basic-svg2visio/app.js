/**
 * App - Ties together the UI, SVG parser, and VSDX builder.
 */

(function () {
    const dropZone = document.getElementById('dropZone');
    const browseBtn = document.getElementById('browseBtn');
    const fileInput = document.getElementById('fileInput');
    const svgInput = document.getElementById('svgInput');
    const convertBtn = document.getElementById('convertBtn');
    const statusText = document.getElementById('statusText');
    const logEl = document.getElementById('log');
    const previewSection = document.getElementById('previewSection');
    const previewContainer = document.getElementById('previewContainer');
    const statsEl = document.getElementById('stats');

    let currentInput = '';
    let currentFormat = 'svg'; // 'svg' or 'drawio'

    // --- Drag and drop ---
    dropZone.addEventListener('dragover', (e) => {
        e.preventDefault();
        dropZone.classList.add('dragover');
    });

    dropZone.addEventListener('dragleave', () => {
        dropZone.classList.remove('dragover');
    });

    dropZone.addEventListener('drop', (e) => {
        e.preventDefault();
        dropZone.classList.remove('dragover');
        const file = e.dataTransfer.files[0];
        if (file) loadFile(file);
    });

    browseBtn.addEventListener('click', () => fileInput.click());
    dropZone.addEventListener('click', (e) => {
        if (e.target !== browseBtn) fileInput.click();
    });

    fileInput.addEventListener('change', () => {
        if (fileInput.files[0]) loadFile(fileInput.files[0]);
    });

    // --- Paste area ---
    svgInput.addEventListener('input', () => {
        const val = svgInput.value.trim();
        if (val && val.includes('<svg')) {
            currentInput = val;
            currentFormat = 'svg';
            convertBtn.disabled = false;
            showPreview(val);
        } else if (val && (val.includes('<mxGraphModel') || val.includes('<mxfile'))) {
            currentInput = val;
            currentFormat = 'drawio';
            convertBtn.disabled = false;
            showDrawioPreview(currentInput);
            log('Draw.io XML detected', 'info');
        } else {
            convertBtn.disabled = !currentInput;
        }
    });

    // --- Convert button ---
    convertBtn.addEventListener('click', async () => {
        if (!currentInput) return;
        await convert(currentInput, currentFormat);
    });

    function loadFile(file) {
        const name = file.name.toLowerCase();
        const isSvg = name.endsWith('.svg') || file.type === 'image/svg+xml';
        const isDrawio = name.endsWith('.drawio') || name.endsWith('.xml');

        if (!isSvg && !isDrawio) {
            log('Please select an SVG, .drawio, or .xml file.', 'error');
            return;
        }

        const reader = new FileReader();
        reader.onload = (e) => {
            const content = e.target.result;
            currentInput = content;
            svgInput.value = content;

            // Auto-detect format from content
            if (content.includes('<mxGraphModel') || content.includes('<mxfile')) {
                currentFormat = 'drawio';
                showDrawioPreview(currentInput);
                log(`Loaded Draw.io file: ${file.name} (${(file.size / 1024).toFixed(1)} KB)`, 'info');
            } else {
                currentFormat = 'svg';
                showPreview(content);
                log(`Loaded SVG: ${file.name} (${(file.size / 1024).toFixed(1)} KB)`, 'info');
            }

            convertBtn.disabled = false;
        };
        reader.readAsText(file);
    }

    function showPreview(svgString) {
        previewSection.style.display = 'block';
        if (svgString) {
            previewContainer.innerHTML = svgString;
            const svg = previewContainer.querySelector('svg');
            if (svg) {
                svg.style.maxWidth = '100%';
                svg.style.height = 'auto';
            }
        } else {
            previewContainer.innerHTML = '<p style="color:#888; padding:2rem;">Draw.io XML loaded (no visual preview)</p>';
        }
    }

    function showDrawioPreview(xmlString) {
        previewSection.style.display = 'block';
        try {
            const parser = new DrawioParser(xmlString);
            const parsed = parser.parse();
            const vb = parsed.viewBox;

            const esc = (s) => s.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;');

            // Render multi-line text as tspan elements
            function renderText(x, y, text, fontSize, fontColor, fontWeight, anchor, baseline) {
                const lines = text.split('\n');
                if (lines.length === 1) {
                    return `<text x="${x}" y="${y}" text-anchor="${anchor}" dominant-baseline="${baseline}" font-size="${fontSize}" font-weight="${fontWeight}" fill="${fontColor}" font-family="sans-serif">${esc(text)}</text>`;
                }
                // Multi-line: offset first line so block is vertically centered
                const lineHeight = fontSize * 1.25;
                let startY = y;
                if (baseline === 'central' || baseline === 'middle') {
                    startY = y - ((lines.length - 1) * lineHeight) / 2;
                }
                let out = `<text x="${x}" text-anchor="${anchor}" font-size="${fontSize}" font-weight="${fontWeight}" fill="${fontColor}" font-family="sans-serif">`;
                for (let i = 0; i < lines.length; i++) {
                    out += `<tspan x="${x}" dy="${i === 0 ? 0 : lineHeight}" y="${i === 0 ? startY : ''}">${esc(lines[i])}</tspan>`;
                }
                out += '</text>';
                return out;
            }

            let svg = `<svg xmlns="http://www.w3.org/2000/svg" viewBox="${vb.x} ${vb.y} ${vb.width} ${vb.height}" style="max-width:100%;height:auto;background:#fff;border-radius:8px;">`;

            // Draw shapes
            for (const s of parsed.shapes) {
                const fill = s.style.fill || '#FFFFFF';
                const stroke = s.style.stroke || '#000000';
                const sw = s.style.strokeWidth || 1;
                const dash = s.style.strokeDasharray ? ` stroke-dasharray="${s.style.strokeDasharray}"` : '';
                const rx = s.style.rx || 0;
                const opacity = s.style.opacity != null ? s.style.opacity : 1;
                const opAttr = opacity < 1 ? ` opacity="${opacity}"` : '';

                if (s.type === 'ellipse') {
                    const cx = s.x + s.width / 2;
                    const cy = s.y + s.height / 2;
                    svg += `<ellipse cx="${cx}" cy="${cy}" rx="${s.width / 2}" ry="${s.height / 2}" fill="${fill}" stroke="${stroke}" stroke-width="${sw}"${dash}${opAttr}/>`;
                } else if (s.type === 'polygon' && s.points) {
                    const pts = s.points.map(p => `${p.x},${p.y}`).join(' ');
                    svg += `<polygon points="${pts}" fill="${fill}" stroke="${stroke}" stroke-width="${sw}"${dash}${opAttr}/>`;
                } else if (s.type === 'diamond') {
                    const pts = [
                        `${s.x + s.width / 2},${s.y}`,
                        `${s.x + s.width},${s.y + s.height / 2}`,
                        `${s.x + s.width / 2},${s.y + s.height}`,
                        `${s.x},${s.y + s.height / 2}`
                    ].join(' ');
                    svg += `<polygon points="${pts}" fill="${fill}" stroke="${stroke}" stroke-width="${sw}"${dash}${opAttr}/>`;
                } else {
                    svg += `<rect x="${s.x}" y="${s.y}" width="${s.width}" height="${s.height}" rx="${rx}" fill="${fill}" stroke="${stroke}" stroke-width="${sw}"${dash}${opAttr}/>`;
                }

                // Draw text label
                if (s.text) {
                    const fs = (s.textStyle && s.textStyle.fontSize) || 12;
                    const fc = (s.textStyle && s.textStyle.textColor) || '#000';
                    const fw = (s.textStyle && s.textStyle.fontWeight) || 'normal';
                    const vAlign = s.verticalAlign || 'middle';

                    let tx = s.x + s.width / 2;
                    let ty, baseline;
                    if (vAlign === 'top') {
                        ty = s.y + fs + 4;
                        baseline = 'auto';
                    } else {
                        ty = s.y + s.height / 2;
                        baseline = 'central';
                    }
                    svg += renderText(tx, ty, s.text, fs, fc, fw, 'middle', baseline);
                }
            }

            // Draw connectors
            for (const c of parsed.connectors) {
                if (c.points.length < 2) continue;
                const stroke = c.style.stroke || '#000';
                const sw = c.style.strokeWidth || 1;
                const dash = c.style.strokeDasharray ? ` stroke-dasharray="${c.style.strokeDasharray}"` : '';
                const d = c.points.map((p, i) => `${i === 0 ? 'M' : 'L'}${p.x},${p.y}`).join(' ');
                svg += `<path d="${d}" fill="none" stroke="${stroke}" stroke-width="${sw}"${dash}/>`;

                // Draw arrowhead
                if (c.hasArrow && c.points.length >= 2) {
                    const last = c.points[c.points.length - 1];
                    const prev = c.points[c.points.length - 2];
                    const angle = Math.atan2(last.y - prev.y, last.x - prev.x);
                    const arrowLen = 10;
                    const arrowAngle = Math.PI / 6;
                    const x1 = last.x - arrowLen * Math.cos(angle - arrowAngle);
                    const y1 = last.y - arrowLen * Math.sin(angle - arrowAngle);
                    const x2 = last.x - arrowLen * Math.cos(angle + arrowAngle);
                    const y2 = last.y - arrowLen * Math.sin(angle + arrowAngle);
                    svg += `<polygon points="${last.x},${last.y} ${x1},${y1} ${x2},${y2}" fill="${stroke}"/>`;
                }
            }

            // Draw standalone texts
            for (const t of parsed.texts) {
                const fs = (t.style && t.style.fontSize) || 11;
                const fc = (t.style && t.style.textColor) || '#000';
                svg += renderText(t.x, t.y, t.text, fs, fc, 'normal', 'middle', 'central');
            }

            svg += '</svg>';
            previewContainer.innerHTML = svg;
        } catch (e) {
            previewContainer.innerHTML = `<p style="color:#f88; padding:2rem;">Preview error: ${e.message}</p>`;
        }
    }

    async function convert(inputString, format) {
        logEl.style.display = 'block';
        logEl.innerHTML = '';
        convertBtn.disabled = true;
        statusText.textContent = 'Converting...';

        try {
            // Parse input based on format
            const isDrawio = format === 'drawio';
            log(isDrawio ? 'Parsing Draw.io XML...' : 'Parsing SVG...', 'info');

            const parser = isDrawio ? new DrawioParser(inputString) : new SvgParser(inputString);
            const parsed = parser.parse();
            const stats = parser.getStats();

            log(`Found ${stats.shapes} shapes, ${stats.connectors} connectors, ${stats.texts} standalone texts`, 'info');
            showStats(stats);

            // Build VSDX
            log('Building VSDX file...', 'info');
            const builder = new VsdxBuilder(parsed);
            const blob = await builder.build();

            log(`Generated VSDX: ${(blob.size / 1024).toFixed(1)} KB`, 'success');

            // Download
            const url = URL.createObjectURL(blob);
            const a = document.createElement('a');
            a.href = url;
            a.download = 'diagram.vsdx';
            document.body.appendChild(a);
            a.click();
            document.body.removeChild(a);
            URL.revokeObjectURL(url);

            log('Download started!', 'success');
            statusText.textContent = 'Done! Check your downloads.';
        } catch (err) {
            log('Error: ' + err.message, 'error');
            statusText.textContent = 'Conversion failed.';
            console.error(err);
        } finally {
            convertBtn.disabled = false;
        }
    }

    function showStats(stats) {
        statsEl.innerHTML = `
            <div class="stat-badge">Shapes: <span>${stats.shapes}</span></div>
            <div class="stat-badge">Connectors: <span>${stats.connectors}</span></div>
            <div class="stat-badge">Texts: <span>${stats.texts}</span></div>
            <div class="stat-badge">Canvas: <span>${stats.viewBox.width} x ${stats.viewBox.height}</span></div>
        `;
    }

    function log(msg, type) {
        const line = document.createElement('div');
        line.className = type || '';
        line.textContent = `[${new Date().toLocaleTimeString()}] ${msg}`;
        logEl.appendChild(line);
        logEl.scrollTop = logEl.scrollHeight;
    }
})();
