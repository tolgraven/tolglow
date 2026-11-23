// Enroll through the production browser UI. Only the response-loss switch is injected.
const { chromium } = require("playwright-core");
const { existsSync, writeFileSync } = require("node:fs");
const { join } = require("node:path");
const delay = ms => new Promise(resolve => setTimeout(resolve, ms));

async function waitForMarker(directory, name) {
  const until = Date.now() + 135000;
  while (!existsSync(join(directory, name))) {
    if (name !== "finish" && existsSync(join(directory, "finish"))) return false;
    if (Date.now() > until) throw Error("Host did not signal " + name);
    await delay(50);
  }
  return true;
}

async function instrument(worker) {
  await worker.evaluate(() => {
    const send = WebSocket.prototype.send;
    self.proof = { prepared: 0, results: 0, dropped: 0, errors: 0, firstResultMs: null, programIds: [] };
    WebSocket.prototype.send = function (data) {
      const msg = typeof data === "string" ? data : new TextDecoder().decode(data);
      const fields = JSON.parse(msg);
      const arg = name => fields[fields.indexOf("~:" + name) + 1];
      switch (arg("op")) {
        case "~:ready":
          self.proof.prepared++;
          self.proof.programIds.push(arg("id"));
          break;
        case "~:error":
          self.proof.errors++;
          break;
        case "~:result":
          self.proof.results++;
          self.proof.firstResultMs ??= performance.now();
          if (self.proof.dropNext) {
            self.proof.dropped++;
            self.proof.dropNext = false;
            console.info("compute-proof:" + JSON.stringify(self.proof));
            this.close(1000, "QA peer left during readback");
            return;
          }
          break;
      }
      return send.call(this, data);
    };
  });
}

async function joinPeer(browser, url) {
  const context = await browser.newContext();
  const page = await context.newPage();
  const result = { phase: "joining" };
  context.on("console", msg => {
    if (msg.text().startsWith("compute-proof:")) {
      result.counts = JSON.parse(msg.text().slice("compute-proof:".length));
    }
  });
  await page.goto(url + "/connections");
  await page.getByLabel("Compute token").fill(process.env.TOLGLOW_COMPUTE_TOKEN);
  const started = page.waitForEvent("worker", { predicate: w => w.url().includes("/compute/runtime/transport.js") });
  await page.getByRole("button", { name: "Contribute this GPU" }).click();
  const worker = await started;
  await page.getByText("Contributing GPU time", { exact: false }).first().waitFor();
  if (await page.evaluate(() => document.hidden)) throw Error("Worker joined from a hidden page");
  await instrument(worker);
  result.phase = "active";
  return { worker, page, result };
}

async function main([url, directory, executablePath]) {
  const browsers = [];
  try {
    for (let i = 0; i < 2; i++) {
      browsers.push(await chromium.launch({
        executablePath, headless: true,
        args: ["--no-sandbox", "--use-angle=swiftshader", "--enable-unsafe-swiftshader"]
      }));
    }
    const peers = await Promise.all(browsers.map(browser => joinPeer(browser, url)));
    writeFileSync(join(directory, "connected"), "2");
    if (await waitForMarker(directory, "lose-peer")) {
      const first = peers[0];
      first.result.counts = await first.worker.evaluate(() => {
        self.proof.dropNext = true;
        return self.proof;
      });
      await waitForMarker(directory, "finish");
    }
    for (const p of peers) {
      const counts = await p.worker.evaluate(() => self.proof).catch(() => null);
      if (counts) p.result.counts = counts;
    }
    writeFileSync(join(directory, "browser.json"), JSON.stringify(peers.map(p => p.result), null, 2));
    await peers[1].page.screenshot({ path: join(directory, "connections.png") });
  } finally {
    await Promise.all(browsers.map(browser => browser.close()));
  }
}

main(process.argv.slice(2)).catch(error => { console.error(error); process.exitCode = 1; });
