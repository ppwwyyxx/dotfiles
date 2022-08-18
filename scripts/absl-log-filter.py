#!/usr/bin/env python3
import multiprocessing as mp
import stat
import atexit
import logging
import sys
from termcolor import colored
from dataclasses import dataclass
import os
import argparse


@dataclass
class LogRecord:
    severity: int
    time: str
    loc: str

    def write(self, buffer):
        text = f"[{self.time} {self.loc}] "
        if self.severity == logging.INFO:
            color = "green"
        else:
            color = "yellow"
        buffer.write(colored(text, color).encode('utf-8'))


def parse_prefix(line):
    severity = line[0]
    if severity == ord(b'I'):
        severity = logging.INFO
    elif severity == ord(b'W'):
        severity = logging.WARNING
    else:
        assert False, str(severity)
    assert line[1:5].isdigit()  # date
    assert line[5] == ord(b' ')
    time = line[6:14].decode('utf-8')
    assert line[14] == ord(b'.') and line[21] == ord(b' ')
    next_space = line.find(b' ', 22)  # skip threadid
    assert next_space > 0
    end_of_prefix = line.find(b']', next_space)
    assert end_of_prefix > 0
    location = line[next_space + 1:end_of_prefix].decode('utf-8')

    record = LogRecord(severity, time, location)
    return record, line[end_of_prefix + 2:]


def process(line, is_newline, is_stdout):
    outf = sys.stdout.buffer if is_stdout else sys.stderr.buffer
    if not is_newline:
        outf.write(line)
        return

    try:
        record, line = parse_prefix(line)
    except Exception:
        outf.write(line)
        return
    record.write(outf)
    outf.write(line)


def run(fname, is_stdout):
    fname = os.path.expanduser(fname)
    is_newline = True
    with open(fname, "rb") as f:
        while True:
            line = f.readline(4096)
            if not line:
                break
            process(line, is_newline, is_stdout)
            is_newline = line.endswith(b'\n')


def ispipe(path):
    return stat.S_ISFIFO(os.stat(path).st_mode)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('--stdout')
    parser.add_argument('--stderr', required=False)
    args = parser.parse_args()

    if ispipe(args.stdout):
        atexit.register(os.unlink, args.stdout)
    if args.stderr and ispipe(args.stderr):
        atexit.register(os.unlink, args.stderr)

    if args.stderr is None:
        run(args.stdout, True)
    else:
        proc1 = mp.Process(target=run, args=(args.stdout, True), daemon=True)
        proc2 = mp.Process(target=run, args=(args.stderr, False), daemon=True)

        proc1.start()
        proc2.start()

        proc1.join()
        proc2.join()
