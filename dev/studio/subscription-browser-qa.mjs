// Exercise real mount/disposal against a preview show with an enabled spatial volume.
import assert from "node:assert/strict";
import { chromium } from "playwright-core";

const [origin] = process.argv.slice(2);
const state = await fetch(origin + "/api/state", { headers: { accept: "application/json" } }).then(r => r.json());
assert.equal(state.status.output.type, "preview");
assert.ok(state.settings.spatial?.["enabled?"] && Object.keys(state.settings.spatial.volumes).length);

const browser = await chromium.launch({
  executablePath: process.env.CHROMIUM_PATH || "/usr/bin/chromium",
  args: ["--no-sandbox", "--use-gl=angle", "--use-angle=swiftshader", "--enable-unsafe-swiftshader"],
});
try {
  const page = await browser.newPage({ viewport: { width: 1024, height: 768 }, hasTouch: true });
  const packets = [];
  page.on("websocket", socket => {
    if (new URL(socket.url()).pathname !== "/api/state/stream") return;
    socket.on("framesent", ({ payload }) => {
      if (payload.includes("~:queries")) packets.push(payload);
    });
  });
  await page.goto(origin + "/rig");
  await page.getByRole("region", { name: "Spatial controls" }).getByRole("button", { name: "Remove volume" }).scrollIntoViewIfNeeded();
  const live = page.locator("[aria-label='Live volume']");
  await live.waitFor();
  await live.getByText(/ inside$/).waitFor();
  assert.ok(packets.some(p => p.includes("~:spatial/body") && p.includes("~:spatial/members")), "visible volume has both server queries");
  const before = packets.length;
  await page.locator("nav a[data-view='debug']").click();
  await live.waitFor({ state: "detached" });
  // The last disposer may run in the next Reagent render queue.
  const deadline = Date.now() + 5000;
  while (Date.now() < deadline && !packets.slice(before).some(p => !p.includes("~:spatial/"))) {
    await new Promise(resolve => setTimeout(resolve, 25));
  }
  assert.ok(packets.slice(before).some(p => !p.includes("~:spatial/")), "leaving Rig releases the final query scope");
  console.log("Chromium mounted spatial telemetry, received native Transit data and released its query scope when navigating away.");
} finally {
  await browser.close();
}
