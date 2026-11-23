import assert from "node:assert/strict";
import {mkdir} from "node:fs/promises";
import {chromium} from "playwright-core";

const url = process.env.STUDIO_URL || "http://127.0.0.1:8080";
const output = process.env.CAPTURE_DIR;
const width = Number(process.env.VIEWPORT_WIDTH || 1024);
const height = width < 600 ? 844 : 768;
const browser = await chromium.launch({executablePath: process.env.CHROMIUM || "/usr/bin/chromium"});
const id = "tour/ripple-" + Date.now();
const errors = [];
let shot = 0;
if (output) await mkdir(output, {recursive: true});

async function picture(page, locator) {
  if (output) {
    await locator.scrollIntoViewIfNeeded();
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
  await capture(panel.getByLabel("Material preview"));
  const field = panel.locator(".material-field");
  const before = await field.innerHTML();
  const beat = panel.getByRole("slider", {name: "Sample beat"});
  await beat.focus();
  await beat.press("End");
  await page.waitForFunction(previous => document.querySelector(".material-field").innerHTML !== previous, before);
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
  await other.page.close();

  await template.selectOption("shader/custom");
  await panel.getByLabel("GLSL expression").fill("bad_shader");
  await panel.locator(".material-preview .danger").waitFor();
  assert.equal(await panel.getByRole("button", {name: "Save material"}).isDisabled(), true);
  await panel.getByLabel("GLSL expression").fill("0.1 + 0.85 * abs(sin(3.0 * position.x + position.y + phase))");
  await panel.getByText(/^Output /).waitFor();
  await capture(panel.getByLabel("GLSL expression"));
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
