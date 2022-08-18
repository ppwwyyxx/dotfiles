#!/usr/bin/env python3
"""
Replace logging.xxx by logging.getLogger(__name__).xxx automatically.

Usage:
    this_file.py <file_or_dir> <file_or_dir>
"""
import glob
import os
import sys
import logging


METHODS = ["info", "warning", "warn", "error", "exception"]


def process_file(filename):
    with open(filename) as f:
        lines = f.readlines()
    whole_file = "".join(lines)
    for meth in METHODS:
        if "logging." + meth in whole_file:
            break
    else:
        return

    for line in lines:
        if line.startswith("logger = logging.getLogger"):
            break
    else:
        try:
            idx = lines.index("import logging\n")
        except ValueError:
            return
        # find a good place to define logger
        # 1. find the last import
        last_import = 0
        for newidx, line in enumerate(lines[idx:]):
            if line.startswith("import ") or line.startswith("from "):
                last_import = idx + newidx
        # 2. find the first empty line after the last import
        try:
            empty_line = lines[last_import:].index("\n") + last_import
        except ValueError:
            logging.error(f"Cannot find a good place in {filename}")
        # 3. insert here and expect linter to move it later
        lines.insert(empty_line, "logger = logging.getLogger(__name__)\n")

    newlines = []
    cnt = 0
    for line in lines:
        newline = line
        for meth in METHODS:
            old = "logging." + meth + "("
            new = "logger." + meth + "("
            newline = newline.replace(old, new)
        if newline != line:
            cnt += 1
        newlines.append(newline)
    logging.info(f"Apply {cnt} fix to {filename}.")
    with open(filename, "w") as f:
        f.write("".join(newlines))


if __name__ == "__main__":
    logging.basicConfig(level=logging.INFO)
    for arg in sys.argv[1:]:
        if os.path.isfile(arg):
            process_file(arg)
        elif os.path.isdir(arg):
            files = glob.glob(f"{arg}/**/*.py") + glob.glob(f"{arg}/*.py")
            for f in files:
                process_file(f)
