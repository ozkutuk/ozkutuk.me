#!/usr/bin/env python

# Ripped from Tony Zorman's blog:
# https://github.com/slotThe/slotThe.github.io/blob/eb0abc8da56a5053c1fbf7e8fddcd25619b7cfb2/scripts/opt-fonts.py

import os
import re
from pathlib import Path

from bs4 import BeautifulSoup
from fontTools.subset import Options, Subsetter
from fontTools.ttLib import TTFont


def used_glyphs(path: str) -> str:
    html = [  # Get HTML for all pages
        BeautifulSoup(Path(f"{p}/{f}").read_text(), "html.parser")
        for (p, _, fs) in os.walk(path)
        for f in fs
        if f.endswith(".html")
    ]

    # latex_html = [p.find_all("span", class_=re.compile("katex*")) for p in html]
    # latex = set()  # Glyphs used in LaTeX
    # [latex.update(tag.get_text()) for page in latex_html for tag in page]

    code_html = [page.find_all("code") for page in html] + [
        page.find_all("div", class_="sourceCode") for page in html
    ]
    code = set()  # Glyphs used in code
    [code.update(tag.get_text()) for page in code_html for tag in page]

    # For the regular text, only keep what's strictly needed.
    normal = set()
    # [tag.extract() for page in latex_html for tag in page]  # Mutates `html`!
    [tag.extract() for page in code_html for tag in page]  # Mutates `html`!
    [normal.update(page.get_text()) for page in html]

    # Return only the relevant glyphs for each of the fonts.
    return "".join(code)

def optimise_font(in_file: str, out_file: str, text: str) -> None:
    options = Options(hinting=False, desubroutinize=True)
    font = TTFont(in_file, lazy=True)
    font.flavor = "woff2"
    subs = Subsetter(options)
    subs.populate(text=text)
    subs.subset(font)
    font.save(out_file)
    font.close()
    print(
        f"Size for {Path(in_file).stem} changed from "
        f"{os.path.getsize(in_file) / 1024:.1f}KB "
        f"to {os.path.getsize(out_file) / 1024:.1f}KB"
    )

def main() -> None:
    in_path = "uncompressed-fonts/"
    code = used_glyphs("_site")
    for font in os.listdir(in_path):
        in_file = in_path + font
        optimise_font(
            in_file,
            f"content/css/fonts/{font.replace('.ttf', '.woff2')}",
            code
        )

if __name__ == "__main__":
    main()
