#!/usr/bin/env python3

# based on answering the topic https://qna.habr.com/q/732307
# see also: http://www.programmersought.com/article/34791573956/

import getopt
import json, base64
from os import getenv
from os.path import exists
import re
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.common.desired_capabilities import DesiredCapabilities
from selenium.webdriver.support.ui import WebDriverWait
from selenium.common.exceptions import TimeoutException
from selenium.webdriver.support import expected_conditions
from selenium.webdriver.chrome.service import Service
import sys
from selenium.webdriver.remote.webdriver import WebDriver
from selenium.webdriver.remote.webelement import WebElement
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC

class Wait:
    def __init__(self, driver: WebDriver, timeout: int = 10):
        self.driver = driver
        self.wait = WebDriverWait(driver, timeout)

    def for_element(self, by: By, value: str) -> WebElement:
        # NOTE: no meaningful logging
        return self.wait.until(
            EC.presence_of_element_located((by, value))
        )

    def for_clickable(self, by: By, value: str) -> WebElement:
        return self.wait.until(
            EC.element_to_be_clickable((by, value))
        )

def element_screenshot(driver,params):
  return driver.execute_cdp_cmd( 'Page.captureScreenshot', params )

try:
  opts, args = getopt.getopt(sys.argv[1:], 'hds:', ['help', 'debug'])
except getopt.GetoptError as err:
  print('Usage: simple_clipped_screenshot.py --debug')
  print(str(err))
  exit()

max_cnt = 10
global debug
debug = False
for option, argument in opts:
  if option == '-d':
    debug = True
  else:
    assert False, 'unhandled option: {}'.format(option)

if getenv('OS') != None :
  homedir = getenv('USERPROFILE').replace('\\', '/')
else:
  homedir = getenv('HOME')

capabilities = DesiredCapabilities.CHROME.copy()
capabilities['acceptSslCerts'] = True
capabilities['acceptInsecureCerts'] = True
# https://www.programcreek.com/python/example/96012/selenium.webdriver.common.desired_capabilities.DesiredCapabilities.CHROME
options = webdriver.ChromeOptions()
# user_agent = 'Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/65.0.3325.181 Safari/537.36'
# option.add_argument('--proxy-server={}'.format(proxy))

options.add_argument('--disable-dev-shm-usage')
options.add_argument('--disable-gpu')
options.add_argument('--disable-gpu')
options.add_argument('--headless')
options.add_argument('--headless=new')
options.add_argument('--no-sandbox')
options.add_argument('--window-size=1920,1080')


# for local tests, check if the driver is in "Downloads"
driver_path =  homedir + '/' + 'Downloads' + '/' + 'chromedriver'
# for Docker tests
if not exists(driver_path):
  driver_path = '/usr/bin/chromedriver'

options.set_capability('acceptInsecureCerts', True)

service = Service(executable_path=driver_path)

driver = webdriver.Chrome( service=service, options=options)

url = 'https://www.wikipedia.org/'
if debug:
  print('Loading url: "{}"'.format(url), file = sys.stderr)
driver.get(url)
print(driver.title)
print(driver.current_url)
print(driver.page_source[:2000])
wait = Wait(driver)
selector =  '#www-wikipedia-org > main img.central-featured-logo'
logo = wait.for_element(By.CSS_SELECTOR, selector) 

logo = driver.find_element(By.CSS_SELECTOR, selector)
params = {'clip': {
  'x': logo.location['x'],
  'y': logo.location['y'],
  'width': logo.size['width'],
  'height': logo.size['height'],
  'scale': 1
}}
result = element_screenshot(driver, params)
output_file = 'wikipedia_screenshot.png'
try:
  with open(output_file, 'wb') as f:
    f.write(result)
  # NOTE: this existed for a while
  # https://www.geeksforgeeks.org/screenshot_as_png-element-method-selenium-python/
except TypeError as e:
  # TypeError: a bytes-like object is required, not 'dict'
  print('exception (ignored): {e}')
with open(file = f'wikipedia_screenshot_2.png', mode = 'wb') as f:
  f.write(logo.screenshot_as_png)

driver.close()
driver.quit()


