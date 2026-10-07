import { test, expect } from "@playwright/test";

// Served by datastar-brotli/BrotliTest/E2EServer.lean.
const server = "http://127.0.0.1:3114";

test("Brotli: each event reaches the browser while the stream is still open", async ({
  page,
}) => {
  await page.goto(`${server}/`);
  await expect(page.locator("#ready")).toHaveText("Ready", { timeout: 10_000 });

  const response = page.waitForResponse(
    (r) => new URL(r.url()).pathname === "/sse/stream",
  );
  await page.click("#stream-trigger");
  expect((await response).headers()["content-encoding"]).toBe("br");

  // The server now waits for /release before it sends anything else, so the
  // first event can only be here if it was flushed through the encoder.
  await expect(page.locator("#step")).toHaveText("Event 1");
  await expect(page.locator("#large li")).toHaveCount(0);

  // The next event is larger than the encoder's 64 KiB input block.
  await page.request.get(`${server}/release`);
  await expect(page.locator("#step")).toHaveText("Event 2");
  await expect(page.locator("#large li")).toHaveCount(3000);
  await expect(page.locator("#item-2999")).toHaveText("Item number 2999");

  await page.request.get(`${server}/release`);
  await expect(page.locator("#step")).toHaveText("Event 3");
});
