#!/usr/bin/env python3

"""
teller_screen.py

Render a text screen specification as:

    1. ASCII/text
    2. PNG bitmap

Designed as a simple Python equivalent of the Java teller-screen POC.

Examples
--------

Render PNG and print the ASCII screen:

    python teller_screen.py \
        --screenfile example.txt \
        --outputfile images/console.png

Explicit font:

    python teller_screen.py \
        --screenfile example.txt \
        --outputfile images/console.png \
        --font /usr/share/fonts/opentype/3270/3270-Regular.otf

Windows:

    python teller_screen.py ^
        --screenfile example.txt ^
        --outputfile images\console.png ^
        --font "%USERPROFILE%\Downloads\3270NerdFontMono-Regular.ttf"

ASCII passthrough only:

    python teller_screen.py \
        --screenfile example.txt \
        --passthrough

Write ASCII to a separate file:

    python teller_screen.py \
        --screenfile example.txt \
        --outputfile images/console.png \
        --textfile results/console.txt

Character-granular rendering:

    python teller_screen.py \
        --screenfile example.txt \
        --outputfile images/console.png \
        --character-granular

JSON labels:

    python teller_screen.py \
        --screenfile example.txt \
        --outputfile images/console.png \
        --labelfile results/console.json

"""

import argparse
import json
import os
import platform
import sys
from pathlib import Path

from PIL import Image, ImageDraw, ImageFont


DEFAULT_COLS = 80
DEFAULT_ROWS = 24

DEFAULT_FONT_SIZE = 24

DEFAULT_LEFT = 30
DEFAULT_TOP = 30


def get_default_font_path():
    """
    Try to mimic the Java POC's platform-dependent default.
    """

    env_font = os.environ.get("FONT_PATH")

    if env_font:
        return Path(env_font)

    if platform.system().lower().startswith("windows"):

        return (
            Path.home()
            / "Downloads"
            / "3270NerdFontMono-Regular.ttf"
        )

    return Path(
        "/usr/share/fonts/opentype/3270/3270-Regular.otf"
    )


def read_screen(filename):
    """
    Read the screen exactly as lines of text.

    Trailing spaces are preserved because they may be meaningful
    when treating the file as a terminal/grid specification.
    """

    path = Path(filename)

    with path.open(
        "r",
        encoding="utf-8"
    ) as f:

        lines = [
            line.rstrip("\r\n")
            for line in f
        ]
    lines.append("Python")
    return lines

def print_screen(screen, output=None):
    """
    Emit the screen as ASCII/text.

    If output is None:
        write to stdout

    Otherwise:
        write to the specified file.
    """

    text = "\n".join(screen)

    if output is None:

        print(text)

    else:

        output_path = Path(output)

        output_path.parent.mkdir(
            parents=True,
            exist_ok=True
        )

        output_path.write_text(
            text + "\n",
            encoding="utf-8"
        )


def load_font(font_path, font_size):
    """
    Load a TrueType or OpenType font.
    """

    font_path = Path(font_path)

    if not font_path.exists():

        raise FileNotFoundError(
            f"Font not found: {font_path}"
        )

    return ImageFont.truetype(
        str(font_path),
        font_size
    )


def get_cell_size(font):
    """
    Determine an approximate terminal cell size.

    We deliberately measure 'M', following the same basic approach
    as the Java version.

    Pillow returns:

        (left, top, right, bottom)

    from which width and height can be calculated.
    """

    bbox = font.getbbox("M")

    cell_width = bbox[2] - bbox[0]
    cell_height = bbox[3] - bbox[1]

    return cell_width, cell_height


def render_screen(
    screen,
    output_file,
    font,
    cols=DEFAULT_COLS,
    rows=DEFAULT_ROWS,
    left=DEFAULT_LEFT,
    top=DEFAULT_TOP,
    foreground="lime",
    background="black",
    character_granular=False,
):
    """
    Render the supplied screen to PNG.

    Parameters
    ----------

    screen:
        List of strings.

    output_file:
        Destination PNG.

    font:
        Pillow font object.

    cols, rows:
        Nominal terminal dimensions.

        These are not required to match the actual text exactly.
        They merely establish the initial bitmap size.

    left, top:
        Margins.

    foreground, background:
        Any Pillow-compatible color.

    character_granular:
        False:
            draw each line in one operation.

        True:
            draw every character separately.

            This is useful for future experiments involving:
                - x/y jitter
                - spacing changes
                - per-character brightness
                - missing pixels
                - selective blur
                - deliberate corruption
    """

    cell_width, cell_height = get_cell_size(font)

    #
    # Ensure the image is large enough even when the supplied
    # screen exceeds the nominal 80x24 model.
    #

    actual_rows = max(rows, len(screen))

    actual_cols = max(
        [cols] +
        [len(line) for line in screen]
    )

    width = left * 2 + actual_cols * cell_width
    height = top * 2 + actual_rows * cell_height

    image = Image.new(
        "RGB",
        (width, height),
        background
    )

    draw = ImageDraw.Draw(image)

    #
    # LINE-GRANULAR MODE
    #

    if not character_granular:

        for row, line in enumerate(screen):

            x = left
            y = top + row * cell_height

            draw.text(
                (x, y),
                line,
                font=font,
                fill=foreground
            )

    #
    # CHARACTER-GRANULAR MODE
    #

    else:

        for row, line in enumerate(screen):

            for col, ch in enumerate(line):

                x = left + col * cell_width
                y = top + row * cell_height

                #
                # Future deliberate imperfection hooks:
                #
                # x += random_offset_x
                # y += random_offset_y
                #
                # foreground = altered_color
                #
                # omit character
                # alter spacing
                # blur selected region
                # change brightness
                # simulate phosphor decay
                #

                draw.text(
                    (x, y),
                    ch,
                    font=font,
                    fill=foreground
                )

    output_path = Path(output_file)

    output_path.parent.mkdir(
        parents=True,
        exist_ok=True
    )

    image.save(
        output_path,
        "PNG"
    )

    return image


def create_labels(
    screen,
    output_file,
    cols,
    rows,
    font_path,
    font_size,
    foreground,
    background,
    character_granular,
):
    """
    Write simple ground-truth metadata.

    This is intentionally uncomplicated.

    The screen text itself is the ground truth.

    Later this can grow into a richer format containing:

        - word coordinates
        - character coordinates
        - bounding boxes
        - corruption parameters
        - random seed
        - font metadata
        - degradation metadata
    """

    labels = {
        "cols": cols,
        "rows": rows,
        "font": str(font_path),
        "font_size": font_size,
        "foreground": foreground,
        "background": background,
        "character_granular": character_granular,
        "screen": screen,
        "text": "\n".join(screen),
    }

    output_path = Path(output_file)

    output_path.parent.mkdir(
        parents=True,
        exist_ok=True
    )

    with output_path.open(
        "w",
        encoding="utf-8"
    ) as f:

        json.dump(
            labels,
            f,
            indent=2,
            ensure_ascii=False
        )


def parse_args():
    parser = argparse.ArgumentParser(
        description=(
            "Render a text screen specification "
            "as ASCII and/or PNG."
        )
    )

    parser.add_argument(
        "--screenfile",
        required=True,
        help="Input screen text file"
    )

    parser.add_argument(
        "--outputfile",
        default=None,
        help="Output PNG file"
    )

    parser.add_argument(
        "--font",
        default=None,
        help=(
            "TrueType/OpenType font. "
            "Defaults to FONT_PATH or platform default."
        )
    )

    parser.add_argument(
        "--font-size",
        type=int,
        default=DEFAULT_FONT_SIZE
    )

    parser.add_argument(
        "--cols",
        type=int,
        default=DEFAULT_COLS
    )

    parser.add_argument(
        "--rows",
        type=int,
        default=DEFAULT_ROWS
    )

    parser.add_argument(
        "--left",
        type=int,
        default=DEFAULT_LEFT
    )

    parser.add_argument(
        "--top",
        type=int,
        default=DEFAULT_TOP
    )

    parser.add_argument(
        "--foreground",
        default="lime"
    )

    parser.add_argument(
        "--background",
        default="black"
    )

    parser.add_argument(
        "--character-granular",
        action="store_true",
        help="Draw each character separately"
    )

    parser.add_argument(
        "--passthrough",
        action="store_true",
        help=(
            "Print the screen text. "
            "Can be used without PNG rendering."
        )
    )

    parser.add_argument(
        "--textfile",
        default=None,
        help="Optional ASCII/text output file"
    )

    parser.add_argument(
        "--labelfile",
        default=None,
        help="Optional JSON ground-truth label file"
    )

    parser.add_argument(
        "--debug",
        action="store_true"
    )

    return parser.parse_args()


def main():

    args = parse_args()

    screen = read_screen(
        args.screenfile
    )

    #
    # ASCII passthrough.
    #

    if args.passthrough:

        print_screen(screen)

    #
    # Optional separate ASCII file.
    #

    if args.textfile:

        print_screen(
            screen,
            args.textfile
        )

        print(
            f"Wrote {args.textfile}"
        )

    #
    # PNG rendering is optional.
    #
    # This means:
    #
    #     --passthrough
    #
    # does not require a font or output PNG.
    #

    if args.outputfile:

        font_path = (
            Path(args.font)
            if args.font
            else get_default_font_path()
        )

        if args.debug:

            print(
                f"Font: {font_path}",
                file=sys.stderr
            )

            print(
                f"Screen rows: {len(screen)}",
                file=sys.stderr
            )

            print(
                "Maximum columns: "
                f"{max(map(len, screen), default=0)}",
                file=sys.stderr
            )

        font = load_font(
            font_path,
            args.font_size
        )

        image = render_screen(
            screen=screen,
            output_file=args.outputfile,
            font=font,
            cols=args.cols,
            rows=args.rows,
            left=args.left,
            top=args.top,
            foreground=args.foreground,
            background=args.background,
            character_granular=args.character_granular,
        )

        print(
            f"Wrote {args.outputfile}"
        )

        if args.debug:

            print(
                f"Image size: "
                f"{image.width}x{image.height}",
                file=sys.stderr
            )

        #
        # Labels only make sense once we know the actual rendering
        # configuration.
        #

        if args.labelfile:

            create_labels(
                screen=screen,
                output_file=args.labelfile,
                cols=args.cols,
                rows=args.rows,
                font_path=font_path,
                font_size=args.font_size,
                foreground=args.foreground,
                background=args.background,
                character_granular=args.character_granular,
            )

            print(
                f"Wrote {args.labelfile}"
            )

    #
    # No output requested at all.
    #

    elif not args.passthrough and not args.textfile:

        print(
            "Nothing to do. "
            "Specify --outputfile, --passthrough, or --textfile.",
            file=sys.stderr
        )

        return 1

    return 0


if __name__ == "__main__":

    sys.exit(main())
