#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.12"
# dependencies = []
# ///
"""Checks the wire format of the fan-out example."""

import argparse
import http.client
import json
import logging
import re
import subprocess
import time
from contextlib import contextmanager
from pathlib import Path
from urllib.parse import urlencode

HOST = "127.0.0.1"
PACKAGE = Path(__file__).resolve().parent

log = logging.getLogger("smoke")


def expect(name, got, want):
    if got != want:
        raise AssertionError(f"{name}: expected {want!r}, got {got!r}")


def run_path(jobs, deadline):
    signals = json.dumps({"jobs": jobs, "deadline": deadline})
    return "/run?" + urlencode({"datastar": signals})


@contextmanager
def get(port, path):
    connection = http.client.HTTPConnection(HOST, port, timeout=10)
    try:
        connection.request("GET", path)
        response = connection.getresponse()
        yield response
        response.close()
    finally:
        connection.close()


def status(port, path):
    with get(port, path) as response:
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


def events(port, path):
    with get(port, path) as response:
        expect("status", response.status, 200)
        return list(iter(lambda: read_event(response), []))


def data_lines(stream):
    return [line for event in stream for line in event if line.startswith("data: ")]


def check_headers(port):
    with get(port, run_path(1, 1)) as response:
        expect("Content-Type", response.getheader("Content-Type"), "text/event-stream")
        expect("Cache-Control", response.getheader("Cache-Control"), "no-cache")
        response.read()


def check_concurrent(port):
    """32 jobs of at least 100 ms each are 3200 ms of work, and none takes more than 1500 ms, so
    only running them together finishes inside the 3000 ms limit."""
    stream = events(port, run_path(32, 2000))
    expect("events", len(stream), 35)
    expect(
        "event types",
        {event[0] for event in stream},
        {"event: datastar-patch-elements"},
    )

    lines = data_lines(stream)
    reports = [line for line in lines if re.match(r"data: elements <li id='job-.*finished in", line)]
    expect("reports", len(reports), 32)

    summary = re.search(r"(\d+) of 32 finished: (\d+) ms of work in (\d+) ms", lines[-1])
    if summary is None:
        raise AssertionError(f"no summary in {lines[-1]!r}")
    finished, work, elapsed = map(int, summary.groups())
    log.debug("%d finished, %d ms of work in %d ms", finished, work, elapsed)
    expect("finished", finished, 32)
    if work < 3200:
        raise AssertionError(f"only {work} ms of work")
    if elapsed >= 3000:
        raise AssertionError(f"not concurrent: {work} ms of work took {elapsed} ms")


def check_incremental(port):
    """The report of a job of at least 100 ms comes after the rows that were sent at the start,
    which a buffered response would deliver together with it."""
    with get(port, run_path(1, 2000)) as response:
        read_event(response)
        read_event(response)
        started = time.monotonic()
        read_event(response)
        gap = time.monotonic() - started
        response.read()
    log.debug("the report came %.0f ms after the first events", gap * 1000)
    if gap < 0.05:
        raise AssertionError(f"the stream was buffered: {gap * 1000:.0f} ms between events")


def check_timeouts(port):
    """Every job loses to a 1 ms deadline."""
    lines = data_lines(events(port, run_path(4, 1)))
    expect("timeouts", sum("timed out at 1 ms" in line for line in lines), 4)
    if "0 of 4 finished" not in lines[-1]:
        raise AssertionError(f"timeout summary: {lines[-1]!r}")


def check_rejected(port):
    """The browser ignores the body of an error status, so a rejection is an event for the page."""
    for name, path, message in [
        ("missing signals", "/run", "whole numbers"),
        ("negative jobs", run_path(-1, 1), "whole numbers"),
        ("no jobs", run_path(0, 1), "between 1 and 50000"),
        ("too many jobs", run_path(50001, 1), "between 1 and 50000"),
    ]:
        summary = data_lines(events(port, path))[-1]
        if "id='summary'" not in summary or message not in summary:
            raise AssertionError(f"{name}: {summary!r}")


def check_errors(port):
    expect("unknown path", status(port, "/nope"), 404)


def check_disconnect(port):
    """A client that leaves mid-stream must not take the server down with it. The sleep outlasts
    the jobs, which report to a stream that has gone."""
    with get(port, run_path(8, 2000)) as response:
        read_event(response)
    time.sleep(2)
    expect("page after a disconnect", status(port, "/"), 200)
    expect("run after a disconnect", len(events(port, run_path(1, 1))), 4)


CHECKS = [
    check_headers,
    check_concurrent,
    check_incremental,
    check_timeouts,
    check_rejected,
    check_errors,
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


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--port", type=int, default=7332)
    parser.add_argument("--debug", action="store_true")
    args = parser.parse_args()
    logging.basicConfig(
        level=logging.DEBUG if args.debug else logging.INFO,
        format="%(levelname)s %(message)s",
    )

    build()
    server = subprocess.Popen([PACKAGE / ".lake/build/bin/fan-out", str(args.port)])
    try:
        wait_for_server(server, args.port)
        for check in CHECKS:
            started = time.monotonic()
            check(args.port)
            log.info("%s: ok (%.2f s)", check.__name__, time.monotonic() - started)
    finally:
        server.terminate()
        server.wait()
    log.info("fan-out: ok")


if __name__ == "__main__":
    main()
