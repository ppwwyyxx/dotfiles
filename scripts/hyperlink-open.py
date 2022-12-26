#!/usr/bin/env python
import os
import sys
import subprocess
import logging
from urllib.parse import urlparse
import socket

logging.basicConfig(filename="/tmp/file-hyperlink-open.log", level=logging.INFO)
BROWSER = os.environ.get("BROWSER", "google-chrome-stable")


def xdg_fallback(path):
    logging.info(f"Opening {path} with xdg-open ...")
    p = subprocess.run(["xdg-open", path])
    sys.exit(p.returncode)


def browser(path):
    logging.info(f"Opening {path} with browser={BROWSER} ...")
    p = subprocess.run([BROWSER, path])
    sys.exit(p.returncode)


if __name__ == "__main__":
    path = sys.argv[1]

    if path.startswith("file:"):
        url = urlparse(path)
        path = url.netloc + url.path
        if os.path.exists(path):
            xdg_fallback("file://" + path)

        if url.netloc == socket.gethostname():
            xdg_fallback("file://" + url.path)

        logging.warning(f"Don't know how to open {sys.argv[1]}")
        sys.exit(1)

    if path.startswith("b/") or path.startswith("go/") or path.startswith("cl/"):
        browser(path)

    xdg_fallback(path)
