// Run against a disposable preview-only Studio with its production bundle.
const assert = require("node:assert/strict");
const { chromium } = require("playwright-core");

(async () => {
  const browser = await chromium.launch({
    executablePath: process.env.STUDIO_CHROMIUM || "/usr/bin/chromium",
    args: ["--no-sandbox", "--use-angle=swiftshader", "--enable-unsafe-swiftshader"],
  });
  try {
    const page = await browser.newPage({
      baseURL: process.env.STUDIO_URL || "http://127.0.0.1:8500",
      viewport: {width: 834, height: 1112}, hasTouch: true,
    });
    const errors = [];
    page.on("pageerror", error => errors.push(error.message));
    await page.goto("/compose");
    await page.locator("#new-composition").tap();
    await page.locator("#composition-name").fill("Layer test");
    await page.locator("#composition-id").fill("layer-touch-test");
    await page.locator("#composition-mode").selectOption("macro");
    await page.locator("#composition-opacity").fill("0.65");
    await page.locator("#slot-cue option").first().waitFor({state: "attached"});
    await page.locator("#add-slot").tap();
    await page.locator("#add-slot").tap();
    const slot = page.locator(".layer-transforms").nth(1);
    await slot.locator("summary").tap();
    await slot.getByLabel("Include earlier members’ heads").check();
    const select = slot.getByRole("combobox");
    await select.selectOption("beat-hue");
    await select.selectOption("ratio");
    await slot.getByLabel("Denominator").fill("3");
    await slot.getByLabel("Move transform earlier, slot 2, transform 2").tap();
    const saved = page.locator("#composition-library .list-card").filter({hasText: "Layer test"});
    await page.locator("#run-composition").tap();
    await saved.getByText(/playing/).waitFor();
    await page.reload();
    await saved.getByRole("button", {name: "Open"}).tap();
    await slot.locator("summary").tap();
    assert.equal(await page.locator("#composition-opacity").inputValue(), "0.65");
    assert.equal(await slot.getByLabel("Include earlier members’ heads").isChecked(), true);
    assert.deepEqual(await slot.locator(".layer-transform-heading strong").allTextContents(),
      ["Set beat ratio", "Rotate hue by beat ratio"]);
    assert.equal(await slot.getByLabel("Denominator").inputValue(), "3");
    await page.locator("#stop-composition").tap();
    await saved.getByText(/playing/).waitFor({state: "hidden"});
    assert.deepEqual(errors, []);
    console.log("Tablet touch passed compound editing, playback, reload, ordering and opacity.");
  } finally {
    await browser.close();
  }
})().catch(error => { console.error(error); process.exitCode = 1; });
