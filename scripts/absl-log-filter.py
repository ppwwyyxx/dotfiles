#!/usr/bin/env python3
import fnmatch
from typing import Optional
import multiprocessing as mp
import functools
import stat
import atexit
import logging
import sys
from termcolor import colored
from dataclasses import dataclass, field
import os
import argparse


READ_BUFFER = 4096
CHAR_TO_SEVERITY = {
    'D': logging.DEBUG,
    'I': logging.INFO,
    'W': logging.WARNING,
    'E': logging.ERROR,
    'F': logging.FATAL,
}


@dataclass
class LogRecord:
    severity: int
    time: str
    location: str
    file: str = field(init=False)
    linenum: str = field(init=False)

    def __post_init__(self):
        loc = self.location.split(":")
        self.file = loc[0]
        self.linenum = int(loc[1])

    def write_to(self, buffer):
        text = f"[{self.time} {self.location}] "
        color = {
            logging.INFO: "green",
            logging.WARNING: "yellow",
            logging.ERROR: "red",
            logging.FATAL: "red",
        }[self.severity]
        buffer.write(colored(text, color).encode('utf-8'))

    def should_log(self):
        filters = get_logfilter()
        for f in filters[::-1]:  # Latter filters take priority
            if f.match(self):
                return f.sign
        return True


@dataclass
class Filter:
    sign: bool  # +/-
    pattern: str
    severity: Optional[int]

    def match(self, log: LogRecord) -> bool:
        if self.severity is not None and self.severity != log.severity:
            return False
        return fnmatch.fnmatch(log.file, self.pattern) or \
            fnmatch.fnmatch(log.location, self.pattern)

    @staticmethod
    def from_arg(s: str):
        if s[0] not in ["+", "-"]:
            raise ValueError(f"Filter must start with + or -. Got {s}.")
        severity = CHAR_TO_SEVERITY.get(s[-1], None)
        pattern = s[1:-1] if severity is not None else s[1:]
        return Filter(sign=s[0] == "+", pattern=pattern, severity=severity)


@functools.lru_cache()
def get_logfilter():
    env = os.environ.get('LOGFILTER', '')
    filters = [Filter.from_arg(f) for f in env.split(',') if f]
    return filters


def parse_prefix(line):
    """
    Example of ABSL log line:
        I0817 19:37:27.908660 1708908 x.cc:510] asdf

    Returns: logrecord, rest_of_line
    """
    severity = CHAR_TO_SEVERITY[chr(line[0])]
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


def run(fname, is_stdout):
    outf = sys.stdout.buffer if is_stdout else sys.stderr.buffer
    fname = os.path.expanduser(fname)
    with open(fname, "rb") as f:
        while True:
            line = f.readline(READ_BUFFER)
            if not line:  # EOF
                break

            try:
                record, line = parse_prefix(line)
            except Exception:
                outf.write(line)
            else:
                if not record.should_log():
                    # Consume the whole line and continue
                    while not line.endswith(b'\n'):
                        line = f.readline(READ_BUFFER)
                    continue
                record.write_to(outf)
                outf.write(line)

            # Finish logging the whole line
            while not line.endswith(b'\n'):
                line = f.readline(READ_BUFFER)
                outf.write(line)


def ispipe(path):
    return stat.S_ISFIFO(os.stat(path).st_mode)


def kill_proc(proc):
    proc.terminate()

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
        proc1.start()

        atexit.register(kill_proc, proc1)

        run(args.stderr, False)
        proc1.join()
