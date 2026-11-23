// Exercise actual Studio rendering and Transit query disposal on the preview-only QA show.
import assert from "node:assert/strict";
import { mkdir } from "node:fs/promises";
import { chromium } from "playwright-core";

const [origin, output] = process.argv.slice(2);
const browser = await chromium.launch({
  executablePath: process.env.CHROMIUM_PATH || "/usr/bin/chromium",
  args: ["--no-sandbox", "--use-angle=swiftshader", "--enable-unsafe-swiftshader"],
});
try {
  const page = await browser.newPage({ viewport: { width: 1112, height: 834 }, hasTouch: true });
  const packets = [];
  const errors = [];
  page.on("pageerror", error => errors.push(error.message));
  page.on("websocket", socket => {
    if (new URL(socket.url()).pathname === "/api/state/stream") {
      socket.on("framesent", ({ payload }) => { if (payload.includes("~:queries")) packets.push(payload); });
    }
  });
  const waitForQuery = async (predicate, start) => {
    await page.waitForFunction(() => document.readyState === "complete");
    const deadline = Date.now() + 5000;
    while (Date.now() < deadline && !packets.slice(start).some(predicate)) await page.waitForTimeout(30);
    assert.ok(packets.slice(start).some(predicate), JSON.stringify(packets.slice(start)));
  };
  await page.goto(origin + "/perform");
  await page.getByRole("button", { name: "Night studies", exact: true }).click();
  const grid = page.getByRole("region", { name: "Cue performance grid" });
  const ember = grid.locator('[data-cue-id="ember"][data-preview="live"]');
  await ember.waitFor();
  assert.equal(await ember.getAttribute("aria-pressed"), "false");
  const colors = new Set();
  if (output) await mkdir(output, { recursive: true });
  const clip = await grid.boundingBox();
  for (let i = 0; i < 16; i++) {
    colors.add(await ember.evaluate(el => el.style.getPropertyValue("--cue-color")));
    if (output) await page.screenshot({ clip, path: output + "/grid-" + String(i).padStart(2, "0") + ".png" });
    await page.waitForTimeout(80);
  }
  assert.ok(colors.size >= 4, "The idle cue must visibly change without being launched");
  const swap = packets.length;
  await page.getByRole("button", { name: "Accents", exact: true }).click();
  await grid.locator('[data-cue-id="breathe"][data-preview="live"]').waitFor();
  await waitForQuery(p => p.includes("accents") && !p.includes("main"), swap);
  const leave = packets.length;
  await page.locator("nav a[data-view='debug']").click();
  await waitForQuery(p => !p.includes("visualization/cue-pads"), leave);
  assert.deepEqual(errors, []);
  console.log(JSON.stringify({ distinctInactiveColors: colors.size, pageSwapped: true, pageReleased: true }));
} finally {
  await browser.close();
}
