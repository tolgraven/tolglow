// Run with lein run -m tolglow.qa.material-signal-browser after npm run studio:build.
// Optional CAPTURE_DIR and VIEWPORT_WIDTH control the release UI capture.
import assert from "node:assert/strict";
import {mkdir} from "node:fs/promises";
import {chromium} from "playwright-core";

const origin = process.argv[2] || process.env.STUDIO_URL;
const directory = process.env.CAPTURE_DIR;
const width = Number(process.env.VIEWPORT_WIDTH || 1024);
const browser = await chromium.launch({executablePath: process.env.CHROMIUM_PATH || "/usr/bin/chromium"});
const page = await browser.newPage({viewport: {width, height: 768}, hasTouch: true});
const errors = [];
let frame = 0;
page.on("pageerror", error => errors.push(error.message));

async function picture(target) {
  if (!directory) return;
  await target.evaluate(el => el.scrollIntoView({block: "center", behavior: "instant"}));
  await page.screenshot({path: directory + "/" + String(frame++).padStart(2, "0") + ".png"});
}
async function publish(label, route) {
  const receipt = page.waitForResponse(r => r.request().method() === "POST" && r.url().endsWith(route));
  await page.getByRole("button", {name:label, exact:true}).tap();
  assert.equal((await receipt).status(), 200);
}
async function signal(panel, control, query, source) {
  const button = panel.getByRole("button", {name:control});
  const group = panel.getByRole("group", {name:"Choose a live signal"});
  await button.tap();
  await group.getByText(/Searching…|No matching signals|audio|controller|wrist/).first().waitFor();
  // Verify a burst of keystrokes produces just the final search request.
  const requests = [];
  const observe = req => {
    if (req.url().endsWith("/api/variable-block/sources")) requests.push(req.postData());
  };
  page.on("request", observe);
  try {
    const box = group.getByRole("searchbox", {name:"Find signal"});
    // Dispatch the burst in one browser task. Separate driver round trips can
    // themselves exceed the debounce window on a busy CI host.
    await box.evaluate((input, text) => {
      const {set} = Object.getOwnPropertyDescriptor(HTMLInputElement.prototype, "value");
      for (const character of text) {
        set.call(input, input.value + character);
        input.dispatchEvent(new InputEvent("input", {bubbles:true, data:character, inputType:"insertText"}));
      }
    }, query);
    const choice = group.getByRole("button", {name:source,exact:true});
    await choice.waitFor();
    await page.waitForFunction(() => !document.querySelector(".signal-choices [role=status]"));
    await picture(group);
    await choice.tap();
    assert.ok(requests.filter(body => body?.includes(query)).length === 1, "Expected one final query");
    assert.ok(requests.filter(body => body?.includes(query.slice(0, 2)) && !body?.includes(query)).length === 0);
  } finally { page.off("request", observe); }
  await button.filter({hasText:source}).waitFor();
  await picture(button);
}
try {
  if (directory) await mkdir(directory,{recursive:true});
  assert.equal((await (await fetch(origin+"/api/state")).json()).status.output.type, "preview");
  await page.goto(origin+"/library");
  await page.locator("#new-definition").tap();
  const id="signal-glass-"+Date.now();
  const source=page.getByLabel("Declarative EDN definition");
  await source.fill('{:id :'+id+' :name "Bass glass" :version 1 :outputs {:color [0.8 0.5 1.0] :dimmer 0.95}}');
  const panel=page.locator(".block-editor:visible");
  await panel.getByRole("button",{name:"Assign blocks"}).tap();
  await panel.getByLabel("Block binding").selectOption({label:"outputs / color"});
  await panel.getByLabel("New block").selectOption({label:"Color gel"});
  await panel.getByRole("button",{name:"Add stage"}).tap();
  await panel.locator(".block-stage summary").filter({hasText:"Inputs"}).tap();
  await signal(panel,/transmission live signal/i,"wrist",'controller / "wrist" / tint');
  await signal(panel,/wet amount live signal/i,"low-rms","audio / features / low-rms");
  const wet=panel.getByRole("button",{name:/wet amount live signal/i});
  await wet.tap();
  const group=panel.getByRole("group",{name:"Choose a live signal"});
  await group.getByText("Enter a path").tap();
  await group.getByRole("textbox",{name:"Signal path"}).fill('[:devices "tour" 0]');
  await group.getByRole("button",{name:"Use path"}).tap();
  await wet.filter({hasText:'devices / "tour" / 0'}).waitFor();
  await signal(panel,/wet amount live signal/i,"low-rms","audio / features / low-rms");
  await panel.getByRole("button",{name:"Write stages to EDN"}).tap();
  await page.waitForFunction(()=>document.querySelector("#definition-source").value.includes(":blocks"));
  const edn=await source.inputValue();
  assert.ok(edn.includes('[:controller "wrist" :tint]') && edn.includes(":vec3"));
  assert.ok(edn.includes("[:audio :features :low-rms]") && edn.includes(":amount"));
  await publish("Validate & publish","/api/definition");
  await picture(source);
  await page.getByRole("button",{name:/Material library/}).tap();
  const designer=page.locator(".material-designer");
  await designer.getByLabel("Start from a template").selectOption("optics/through-water");
  await designer.getByLabel("Material name").fill("Bass water");
  await designer.getByLabel("Material ID").fill("tour/bass-water-"+Date.now());
  await signal(designer,/depth live signal/i,"low-rms","audio / features / low-rms");
  await designer.getByText(/^Output /).waitFor();
  await picture(designer.getByRole("button",{name:"Save material"}));
  await publish("Save material","/api/materials");
  await designer.getByText("Material saved.",{exact:true}).waitFor();
  await picture(designer.getByRole("button",{name:"Save material"}));
  assert.deepEqual(errors,[]);
  console.log("Published typed controller and audio mappings on an effect and saved an audio-mapped material at",width);
} finally {
  await browser.close();
}
