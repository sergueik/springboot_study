const { Builder, By } = require('selenium-webdriver');
const { Options } = require('selenium-webdriver/chrome');

(async function hello() {
    let driver;

    try {
         // NOTE: do not use 'chromium' on alpine:
         // Error: Do not know how to build driver: chromium; did you forget to call usingServer(url)?
        let builder = new Builder().forBrowser('chrome');
        let options = new Options();
        options.headless();           
        options.excludeSwitches(['enable-logging']);
        options.addArguments(['--no-sandbox']);    
        driver = await builder.setChromeOptions(options).build();

        await driver.get('https://www.google.com');
        const b = await driver.findElement(By.name('btnK')).getAttribute('value');
        console.log(`Google Search button text: ${b}`);
    } catch (e) {
        console.log(e);
    } finally {
        if (driver) {
            await driver.close();
            await driver.quit();
        }
    }
})();
