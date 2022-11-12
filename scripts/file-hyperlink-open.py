#!/usr/bin/env python
import os, sys
import subprocess
import logging
from urllib.parse import urlparse
import socket

logging.basicConfig(filename="/tmp/file-hyperlink-open.log", level=logging.INFO)


def xdg_fallback(path):
    p = subprocess.run(["xdg-open", path])
    sys.exit(p.returncode)


if __name__ == "__main__":
  path = sys.argv[1]
  logging.info(f"Opening {path} ...")
  if not path.startswith("file:"):
    xdg_fallback(path)

  url = urlparse(path)
  path = url.netloc + url.path
  if os.path.exists(path):
    xdg_fallback("file://" + path)

  if url.netloc == socket.gethostname():
    xdg_fallback("file://" + url.path)
  logging.info(f"Don't know how to open {sys.argv[1]}")
