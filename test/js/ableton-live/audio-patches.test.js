const { test } = require("node:test");
const assert = require("node:assert/strict");
const fs = require("node:fs");
const path = require("node:path");

for (const role of ["send", "receive"]) {
  test(`${role} routes one saved Live bus with the expected signal connections`, () => {
    const patch = JSON.parse(fs.readFileSync(path.join(__dirname, "../../../resources/devices/ableton-live", `tolglow-bus-${role}.maxpat`))).patcher;
    const boxes = new Map(patch.boxes.map(({box}) => [box.id, box]));
    const lines = patch.lines.map(({patchline: {source, destination}}) => {
      assert.ok(boxes.has(source[0]) && boxes.has(destination[0]));
      return [...source, ...destination].join(" ");
    });
    assert.equal(boxes.get("route").text, role + "~ ---inactive");
    assert.equal(boxes.get("bus").parameter_enable, 1);
    assert.equal(boxes.get("bus-name").text, "sprintf tolglow-audio-%ld");
    for (const edge of ["ready 0 bus 0", "bus 0 bus-name 0", "bus-name 0 rename 0", "rename 0 route 0"]) {
      assert.ok(lines.includes(edge));
    }
    if (role === "send") {
      assert.ok(lines.includes("input 0 route 0"));
      assert.equal(lines.some(edge => edge.includes(" output ")), false);
    } else {
      assert.ok(lines.includes("route 0 output 0") && lines.includes("route 0 output 1"));
    }
  });
}
