const { Builder, By } = require('selenium-webdriver');
const { Options } = require('selenium-webdriver/chrome');

async function hello(el) {
  let driver;
  try {
    let builder = new Builder().forBrowser('chrome');
    let options = new Options();
    options.headless();
    options.excludeSwitches(['enable-logging']);
    options.addArguments(['--no-sandbox']);
    driver = await builder.setChromeOptions(options).build();


    await driver.get('https://www.wikipedia.org/');
    await driver.findElement(By.xpath('//*[@id="js-link-box-' + el + '"]')).click();

    const t = await driver.findElement(By.xpath('//*[@class="mw-headline"]')).getText();
    let d = new Date(Date.now());
    console.log('run on: ' +
      d.toDateString() + ' ' + d.getHours() + ':' + d.getMinutes() + ':' + d.getSeconds());
    console.log('Wikipedia page for :' + el);
    console.log(`text: ${t}`);
  } catch (e) {
    console.log(e);
  } finally {
    if (driver) {
      await sleep(5000);
      await driver.close();
      await driver.quit();
    }
  }
}

function sleep(ms) {
  return new Promise((o) => { setTimeout(o, ms); });
}
rows = ['de', 'fr', 'ru'];
rows.forEach(async function(el) {
  // iife ?
  await hello(el);
  await sleep(5000);
});
