#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.12"
# dependencies = []
# ///
"""Checks the SSE responses of datastar-leanin, through the fan-out example."""

import argparse
import http.client
import json
import logging
import subprocess
import time
from contextlib import contextmanager
from pathlib import Path

HOST = "127.0.0.1"
PACKAGE = Path(__file__).resolve().parent

log = logging.getLogger("smoke")


def expect(name, got, want):
    if got != want:
        raise AssertionError(f"{name}: expected {want!r}, got {got!r}")


def run_body(jobs, deadline):
    return json.dumps({"jobs": jobs, "deadline": deadline})


@contextmanager
def request(port, method, path, body=None):
    connection = http.client.HTTPConnection(HOST, port, timeout=10)
    try:
        connection.request(method, path, body)
        response = connection.getresponse()
        yield response
        response.close()
    finally:
        connection.close()


def status(port, path):
    with request(port, "GET", path) as response:
        response.read()
        return response.status


def read_event(response):
    """The lines of the next SSE event; empty at the end of the stream."""
    lines = []
    while line := response.readline().decode():
        if line == "\n":
            break
        lines.append(line.rstrip("\n"))
    return lines


def events(port, body):
    with request(port, "POST", "/run", body) as response:
        expect("status", response.status, 200)
        return list(iter(lambda: read_event(response), []))


def check_headers(port):
    with request(port, "POST", "/run", run_body(1, 1)) as response:
        expect("Content-Type", response.getheader("Content-Type"), "text/event-stream")
        expect("Cache-Control", response.getheader("Cache-Control"), "no-cache")
        response.read()


def check_incremental(port):
    """The report of a job of at least 100 ms comes after the rows that were sent at the start,
    which a buffered response would deliver together with it."""
    with request(port, "POST", "/run", run_body(1, 2000)) as response:
        read_event(response)
        read_event(response)
        started = time.monotonic()
        read_event(response)
        gap = time.monotonic() - started
        response.read()
    log.debug("the report came %.0f ms after the first events", gap * 1000)
    if gap < 0.05:
        raise AssertionError(f"the stream was buffered: {gap * 1000:.0f} ms between events")


def check_disconnect(port):
    """A client that leaves mid-stream must not take the server down with it. The sleep outlasts
    the jobs, which report to a stream that has gone."""
    with request(port, "POST", "/run", run_body(8, 2000)) as response:
        read_event(response)
    time.sleep(2)
    expect("page after a disconnect", status(port, "/"), 200)
    expect("run after a disconnect", len(events(port, run_body(1, 1))), 4)


CHECKS = [
    check_headers,
    check_incremental,
    check_disconnect,
]


def wait_for_server(server, port):
    for _ in range(50):
        if server.poll() is not None:
            raise RuntimeError(f"the server exited with {server.returncode}")
        try:
            status(port, "/")
            return
        except OSError:
            time.sleep(0.2)
    raise RuntimeError("the server did not start")


def build():
    started = time.monotonic()
    result = subprocess.run(
        ["lake", "build", "fan-out"], cwd=PACKAGE, capture_output=True, text=True
    )
    log.debug("%s", result.stdout)
    if result.returncode != 0:
        log.error("%s%s", result.stdout, result.stderr)
        raise RuntimeError("lake build failed")
    log.debug("built in %.1f s", time.monotonic() - started)


def serve_and_check(port, workers):
    command = [PACKAGE / ".lake/build/bin/fan-out", str(port), f"workers={workers}"]
    server = subprocess.Popen(command)
    try:
        wait_for_server(server, port)
        for check in CHECKS:
            started = time.monotonic()
            check(port)
            log.info(
                "workers=%d %s: ok (%.2f s)", workers, check.__name__, time.monotonic() - started
            )
    finally:
        server.terminate()
        server.wait()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--port", type=int, default=7332)
    parser.add_argument(
        "--workers", type=int, nargs="+", default=[1, 4], help="carrier counts to check, in turn"
    )
    parser.add_argument("--debug", action="store_true")
    args = parser.parse_args()
    logging.basicConfig(
        level=logging.DEBUG if args.debug else logging.INFO,
        format="%(levelname)s %(message)s",
    )

    build()
    for workers in args.workers:
        serve_and_check(args.port, workers)
    log.info("fan-out: ok")


if __name__ == "__main__":
    main()
