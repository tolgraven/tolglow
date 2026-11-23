import assert from "node:assert/strict";
import {mkdir} from "node:fs/promises";
import {chromium} from "playwright-core";

const url = process.env.STUDIO_URL || "http://127.0.0.1:8080";
const output = process.env.CAPTURE_DIR;
const width = Number(process.env.VIEWPORT_WIDTH || 1024);
const height = width < 600 ? 844 : 768;
const browser = await chromium.launch({executablePath: process.env.CHROMIUM || "/usr/bin/chromium"});
const id = process.env.MATERIAL_ID || "tour/ripple-" + Date.now();
const errors = [];
let shot = 0;
if (output) await mkdir(output, {recursive: true});

async function picture(page, locator) {
  if (output) {
    await locator.scrollIntoViewIfNeeded();
    if (width >= 740) {
      await page.locator(".material-designer").evaluate(panel => {
        const top = document.querySelector(".topbar").getBoundingClientRect().bottom;
        window.scrollBy({top: panel.getBoundingClientRect().top - top - 12, behavior: "instant"});
      });
    }
    await page.screenshot({path: output + "/" + String(shot++).padStart(2, "0") + ".png"});
  }
}

async function open() {
  const page = await browser.newPage({viewport: {width, height}, hasTouch: true});
  page.on("pageerror", error => errors.push(error.message));
  await page.goto(url + "/library");
  await page.getByRole("button", {name: /Material library/}).tap();
  const panel = page.locator(".material-designer");
  await panel.getByText(/^Output /).waitFor();
  return {page, panel};
}

async function write(page, button, status) {
  const done = page.waitForResponse(r => r.url().endsWith("/api/materials") && r.request().method() === "POST");
  await button.tap();
  assert.equal((await done).status(), status);
}

try {
  const state = await (await fetch(url + "/api/state")).json();
  assert.equal(state.status.output.type, "preview");
  const {page, panel} = await open();
  const capture = locator => picture(page, locator);
  const template = panel.getByLabel("Start from a template");
  await template.selectOption("optics/through-water");
  await panel.getByLabel("Material ID").fill(id);
  await panel.getByLabel("Material name").fill("Tidal glass");
  await panel.getByText(/^Output /).waitFor();
  assert.ok(await panel.evaluate(el => el.scrollWidth <= el.clientWidth + 1), "Material editor must fit the touch viewport");
  await capture(panel.getByLabel("Material preview"));
  const field = panel.locator(".material-field");
  const before = await field.evaluate(canvas => canvas.toDataURL());
  const beat = panel.getByRole("slider", {name: "Sample beat"});
  await beat.focus();
  await beat.press("End");
  await page.waitForFunction(previous => document.querySelector(".material-field").toDataURL() !== previous, before);
  await capture(beat);
  const depth = panel.getByRole("spinbutton", {name: "Depth", exact: true});
  await depth.fill("0.85"); await depth.blur();
  await panel.getByText(/^Output /).waitFor();
  await capture(depth);
  await write(page, panel.getByRole("button", {name: "Save material"}), 200);
  await panel.getByText("Material saved.", {exact:true}).waitFor();
  await capture(panel.getByLabel("Saved material"));

  const other = await open();
  await other.panel.getByLabel("Saved material").selectOption(id);
  await other.panel.getByLabel("Material name").fill("Stale edit");
  await panel.getByLabel("Material name").fill("Tidal glass · deep");
  await write(page, panel.getByRole("button", {name: "Save material"}), 200);
  await write(other.page, other.panel.getByRole("button", {name: "Save material"}), 422);
  await other.panel.getByText(/Another operator changed/).waitFor();
  await other.panel.getByRole("button", {name: "Reload library"}).tap();
  await other.panel.getByLabel("Saved material").selectOption(id);
  await other.panel.getByLabel("Material name").waitFor();
  assert.equal(await other.panel.getByLabel("Material name").inputValue(), "Tidal glass · deep");
  await other.page.close();

  await template.selectOption("shader/custom");
  await panel.getByText("Add an input", {exact: true}).tap();
  await panel.getByLabel("Input name").fill("chroma");
  await panel.getByRole("button", {name:"Add input", exact:true}).tap();
  await panel.getByRole("spinbutton", {name:"Chroma"}).waitFor();
  await panel.getByText("Add an input", {exact: true}).tap();
  await panel.getByLabel("GLSL expression").fill("bad_shader");
  await panel.locator(".material-preview .danger").waitFor();
  assert.equal(await panel.getByRole("button", {name: "Save material"}).isDisabled(), true);
  await panel.getByLabel("Material name").fill("Polar glass");
  await panel.getByLabel("Material ID").fill("venue/polar-glass");
  await panel.getByLabel("GLSL expression").fill("vec3(0.08 + 0.7 * abs(sin(position.y + 1.2 * position.x + phase)), 0.3 + 0.68 * cos(0.7 * position.x - position.y) * cos(position.y), chroma + 0.48 * sin(1.7 * position.x + position.y + phase))");
  await panel.getByText(/^Output /).waitFor();
  await capture(panel.getByLabel("GLSL expression"));
  if (output) {
    const slider = panel.getByRole("slider", {name: "Sample beat"});
    await slider.scrollIntoViewIfNeeded();
    const bounds = await slider.boundingBox();
    for (let sample = 1; sample <= 14; sample++) {
      const response = page.waitForResponse(r => r.url().endsWith("/api/materials/preview") && r.request().method() === "POST");
      await slider.tap({position: {x: 8 + (bounds.width - 16) * sample / 15, y: bounds.height / 2}});
      assert.equal((await response).status(), 200);
      await panel.getByText(/^Output /).waitFor();
      await capture(slider);
    }
  }
  await panel.getByLabel("Saved material").selectOption(id);
  await panel.getByText(/^Output /).waitFor();
  assert.equal(await panel.getByLabel("Material name").inputValue(), "Tidal glass · deep");
  await capture(panel.getByLabel("Material preview"));
  await panel.getByRole("button", {name: "Remove material", exact:true}).tap();
  await write(page, panel.getByRole("button", {name: "Confirm removal"}), 200);
  await panel.getByText("Material removed.", {exact:true}).waitFor();
  assert.equal(await panel.getByLabel("Saved material").locator('option[value="' + id + '"]').count(), 0);
  await panel.getByRole("button", {name: /Material library/}).tap();
  assert.deepEqual(errors, []);
  console.log("Passed draft preview, scrub, edit, stale write, shader feedback and delete at " + width + "px.");
} finally {
  await browser.close();
}
