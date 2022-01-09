#!/usr/bin/env python3

from selenium import webdriver
from selenium.webdriver.common.desired_capabilities import DesiredCapabilities
from selenium.webdriver.firefox.options import Options

from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.action_chains import ActionChains
from selenium.webdriver.support.ui import Select
from selenium.common.exceptions import NoSuchElementException

import sys, time, datetime, os

host = 'selenium'
hub = 'http://{}:4444/wd/hub'.format(host)
# hub = 'http://{}:4444'.format(host)
options = webdriver.ChromeOptions()
options.add_argument('headless')
options.add_argument('--ignore-certificate-errors')
options.add_argument('--ignore-ssl-errors')
# options.add_argument('--verbose')
options.add_argument('--ignore-gpu-blacklist')
options.add_argument('--use-gl')
options.add_argument("--no-sandbox")
options.add_argument('--disable-web-security')
options.add_experimental_option("excludeSwitches", ['enable-logging'])
# options.add_argument('--user-agent={}'.format(random.choice(headers.headers)))
driver = webdriver.Remote(hub, options=options, desired_capabilities=DesiredCapabilities.CHROME)


driver.get('https://www.wikipedia.org')

time.sleep(2)
print(driver.page_source)

# Selenium 3.x calling method semantics
element = driver.find_element_by_class_name('svg-search-icon')
print('{}'.format(element.get_attribute('innerHTML') ))

# Selenium 4.x calling method semantics
driver.find_element(By.CLASS_NAME,'svg-search-icon')
print('{}'.format(element.get_attribute('innerHTML') ))

print('Take screenshot')
driver.save_screenshot('firstpagescreenshot.png')
# close driver - not happening automatically
driver.close()
driver.quit()
