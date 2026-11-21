// Run against a preview Studio host with the compiled production frontend.
const assert = require("node:assert/strict");
const { chromium } = require("playwright-core");

(async () => {
  const browser = await chromium.launch({
    executablePath: process.env.STUDIO_CHROMIUM || "/usr/bin/chromium",
    args: ["--no-sandbox", "--use-gl=angle", "--use-angle=swiftshader", "--enable-unsafe-swiftshader"],
  });
  try {
    const base = process.env.STUDIO_URL || "http://127.0.0.1:8500";
    const context = await browser.newContext({baseURL: base});
    const page = await context.newPage();
    const errors = [];
    page.on("pageerror", error => errors.push(error.message));
    const selected = name => page.locator('nav a[data-view="' + name + '"][aria-current="page"]').waitFor();

    const response = await page.goto("/rig", {waitUntil: "domcontentloaded"});
    assert.equal(response.status(), 200);
    await selected("rig");
    assert.equal(await page.locator('nav a[data-view="compose"]').getAttribute("href"), "/compose");
    await page.evaluate(() => { window.navigationDocumentId = "rig"; });

    await page.locator('nav a[data-view="compose"]').click();
    await page.waitForURL("**/compose");
    await selected("compose");
    assert.equal(await page.evaluate(() => window.navigationDocumentId), "rig");

    await page.keyboard.press("4");
    await page.waitForURL("**/library");
    await selected("library");
    await page.goBack();
    await selected("compose");
    await page.goForward();
    await selected("library");
    assert.equal(await page.evaluate(() => window.navigationDocumentId), "rig");
    await page.reload({waitUntil: "domcontentloaded"});
    await selected("library");

    await page.locator('nav a[data-view="connections"]').click();
    await selected("connections");
    await page.waitForFunction(() => document.querySelector("#connection-dot.connected"));
    await page.waitForFunction(async () => {
      const response = await fetch("/api/settings");
      return (await response.json())?.ui?.view === "connections";
    });
    await page.goto("/", {waitUntil: "domcontentloaded"});
    await page.waitForURL("**/connections");
    await selected("connections");

    const repl = await context.newPage();
    await repl.goto("/repl?session-id=scratch", {waitUntil: "domcontentloaded"});
    await repl.locator("#repl-console.repl-popup").waitFor();
    assert.equal(await repl.locator("nav a[data-view]").count(), 0);
    await repl.close();

    const push = await context.newPage();
    const api = [];
    push.on("request", request => { if (new URL(request.url()).pathname.startsWith("/api/")) api.push(request.url()); });
    push.on("websocket", socket => api.push(socket.url()));
    await push.goto("/push-display", {waitUntil: "load"});
    await push.locator("body.push-display-page .push-brand").waitFor();
    assert.deepEqual(api, []);
    const unknown = await context.request.get("/no-such-studio-page");
    assert.equal(unknown.status(), 404);
    assert.deepEqual(errors, []);
    console.log("Passed Studio deep links, history, shortcuts, preferences, popup REPL and passive Push.");
  } finally {
    await browser.close();
  }
})().catch(error => { console.error(error); process.exitCode = 1; });
