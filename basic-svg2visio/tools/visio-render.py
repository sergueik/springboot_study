#!/usr/bin/env python3
"""Render a .vsdx to PNG the way Visio-like engines do, not libvisio.

LibreOffice Draw uses libvisio, which ignores ShapeSheet formulas and still
draws group members when DisplayMode is missing. That is why a file can look
fine in LibreOffice and empty in Visio.

Aspose.Diagram evaluates enough of the Visio model to reproduce that gap:
grouped files without DisplayMode=1 render as empty frames, the same picture
the drawing's author saw in Visio.

Install (once, evaluation watermark):
    pip install aspose-diagram

Usage:
    python3 tools/visio-render.py drawing.vsdx [out.png]
"""
from __future__ import annotations

import os
import sys


def main() -> int:
    if len(sys.argv) < 2:
        print(__doc__.strip(), file=sys.stderr)
        return 2
    src = sys.argv[1]
    dest = sys.argv[2] if len(sys.argv) > 2 else os.path.splitext(src)[0] + '.png'
    try:
        import jpype
        import asposediagram  # noqa: F401
    except ImportError:
        print('pip install aspose-diagram', file=sys.stderr)
        return 1
    jpype.startJVM()
    from asposediagram.api import Diagram, SaveFileFormat

    diagram = Diagram(src)
    page = diagram.getPages().get(0)
    top = page.getShapes().getCount()

    def count(shapes) -> int:
        n = shapes.getCount()
        for i in range(shapes.getCount()):
            n += count(shapes.get(i).getShapes())
        return n

    total = count(page.getShapes())
    diagram.save(dest, SaveFileFormat.PNG)
    print(f'{dest}  {os.path.getsize(dest)} bytes  top={top} shapes={total}')
    if top < total:
        print(f'note: {total - top} shapes sit inside groups')
    return 0


if __name__ == '__main__':
    raise SystemExit(main())
