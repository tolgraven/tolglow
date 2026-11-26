// Capture the personality icon contact sheet written by lein run -m tolglow.qa.personality-icons.
import { chromium } from 'playwright-core';
import { pathToFileURL } from 'node:url';

const [page, output, profile] = process.argv.slice(2);
if (!page || !output || !profile || !process.env.CHROMIUM_PATH) {
  console.error('Usage: CHROMIUM_PATH=… node dev/studio/personality-contact-sheet.mjs <page.html> <output.png> <profile-dir>');
  process.exit(2);
}
const context = await chromium.launchPersistentContext(profile, {
  executablePath: process.env.CHROMIUM_PATH,
  headless: true,
  args: ['--headless=new', '--use-angle=metal'],
  viewport: { width: 916, height: 400 },
  deviceScaleFactor: 2,
});
try {
  const tab = await context.newPage();
  const problems = [];
  tab.on('pageerror', error => problems.push(error.message));
  await tab.goto(pathToFileURL(page).href);
  const broken = await tab.evaluate(async () => {
    const images = [...document.images];
    await Promise.all(images.map(image => image.decode().catch(() => null)));
    return images.filter(image => !image.naturalWidth).length;
  });
  if (broken || problems.length) throw new Error(`${broken} icons failed to decode. ${problems.join(' ')}`);
  await tab.screenshot({ path: output, fullPage: true });
} finally {
  await context.close();
}
