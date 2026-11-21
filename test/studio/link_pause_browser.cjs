// Run against a disposable Studio preview host with the production frontend built.
const assert = require("node:assert/strict");
const { chromium } = require("playwright-core");

(async () => {
  const baseURL = process.env.STUDIO_URL || "http://127.0.0.1:8500";
  const browser = await chromium.launch({
    executablePath: process.env.STUDIO_CHROMIUM || "/usr/bin/chromium",
    args: ["--no-sandbox", "--use-gl=angle", "--use-angle=swiftshader", "--enable-unsafe-swiftshader"],
  });
  try {
    const page = await browser.newPage({baseURL, viewport: {width: 1024, height: 1366}, hasTouch: true});
    const errors = [];
    page.on("pageerror", error => errors.push(error.message));
    await page.goto("/connections");
    await page.locator("#link-port").fill("17175");
    await page.getByLabel("Follow Link play and stop").uncheck();
    await page.locator("#sync-save").click();
    await page.waitForFunction(async () => {
      const {config} = await (await fetch("/api/sync")).json();
      return config.link.port === 17175 && !config.link["follow-transport?"];
    });
    await page.locator("#clock-source").selectOption("ableton-link");
    await page.waitForFunction(async () =>
      (await (await fetch("/api/sync")).json()).config["selected-source"] === "ableton-link");
    await page.locator("#pause-mode").selectOption("ambient");
    await page.locator("#pause-color").fill("#c06030");
    await page.locator("#pause-level").fill("12");
    await page.locator("#pause-save").click();
    await page.waitForFunction(async () =>
      (await (await fetch("/api/settings")).json())?.output?.["pause-look"]?.level === 0.12);
    const settings = await (await page.request.get("/api/settings")).json();
    assert.deepEqual(settings.output["pause-look"].color, [192 / 255, 96 / 255, 48 / 255]);
    await page.reload();
    await page.waitForFunction(() =>
      document.querySelector("#pause-mode")?.value === "ambient" && document.querySelector("#link-port")?.value === "17175");
    assert.equal(await page.locator("#pause-color").inputValue(), "#c06030");
    assert.equal(await page.getByLabel("Follow Link play and stop").isChecked(), false);
    await page.locator("#pause-mode").selectOption("blackout");
    await page.locator("#pause-save").click();
    await page.waitForFunction(async () =>
      (await (await fetch("/api/settings")).json())?.output?.["pause-look"]?.mode === "blackout");
    assert.deepEqual(errors, []);
    console.log("Passed Link settings, clock selection, ambient color and brightness, reload and blackout.");
  } finally {
    await browser.close();
  }
})().catch(error => { console.error(error); process.exitCode = 1; });
