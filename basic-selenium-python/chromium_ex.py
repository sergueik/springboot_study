#!/usr/bin/env python3

from selenium import webdriver
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support.wait import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC

from selenium.common.exceptions import TimeoutException

import getopt
from os import getenv
import sys, time, datetime
import json, base64




run_headless = False
debug = True
# https://stackoverflow.com/questions/47023842/selenium-chromedriver-printtopdf
# https://www.python-course.eu/python3_formatted_output.php
def send_command_and_get_result(driver, cmd, params = {}):
  post_url = driver.command_executor._url + '/session/{0:s}/chromium/send_command_and_get_result'.format( driver.session_id)
  if debug:
    print ('POST to {}'.format(post_url))
    print('params: {}'.format(json.dumps({'cmd': cmd, 'params': params})))

  response = driver.command_executor._request('POST', post_url, json.dumps({'cmd': cmd, 'params': params}))
  if debug:
    print( response.keys())
  # NOTE: 'has_key()' is even removed from P 3.x
  # see also: https://stackoverflow.com/questions/1323410/should-i-use-has-key-or-in-on-python-dicts
  # NOTE: KeyError: 'status'
  # early imlementation returns JSON with ['status', 'sessionId', 'value'] keys
  # with recent versions of chrome response contains only has ['value']['data']
  # print( response.keys())
  if ('status' in response ) and response['status']:
    raise Exception(response.get('value'))

  return response.get('value')
  # NOTE: on Windows 7 node occationally seeing commctl32.dll warning:
  # 'A program running on this computer is trying to display a message'
  # no meaningful message shown when 'View the Message' is chosen - repeated multiple times

if getenv('OS') != None :
  homedir = getenv('USERPROFILE').replace('\\', '/')
  chromedriver = 'chromedriver.exe'
  # https://www.chromium.org/getting-involved/download-chromium
  # https://commondatastorage.googleapis.com/chromium-browser-snapshots/index.html?prefix=Win/98796/
  browser = (r'{}\AppData\Local\Chromium\Application\chromium.exe'.format(getenv('USERPROFILE'))) # per-user
  executable_path = r'{}\Downloads\{}'.format(getenv('USERPROFILE'), chromedriver)
else:
  homedir = getenv('HOME')
  chromedriver = 'chromedriver'
  # see also: smapshot directory
  # https://commondatastorage.googleapis.com/chromium-browser-snapshots/index.html?prefix=Linux_x64/97974/
  browser = '/usr/bin/chromium-browser' # this is a soft link
  browser = '/usr/lib/chromium/chrome'
  executable_path = '/usr/bin/chromedriver'

options = Options()
options.add_argument('start-maximized')
options.binary_location = browser

# additional options
options.add_argument('--allow-insecure-localhost')
options.add_argument('--allow-running-insecure-content')
options.add_argument('--disable-blink-features=AutomationControlled')
options.add_argument('--disable-browser-side-navigation')
options.add_argument('--disable-dev-shm-usage')
options.add_argument('--disable-extensions')
options.add_argument('--disable-gpu')
options.add_argument('disable-infobars')
options.add_argument('--enable-javascript')
if getenv('DISPLAY') is None:
  run_headless = True
if run_headless == True:
  options.add_argument('--headless')
options.add_argument('--ignore-certificate-errors')
options.add_argument('--no-sandbox')
options.add_argument('start-maximized')
user_agent = 'Chromium 95.0.4638.69'
# https://stackoverflow.com/questions/64992087/webdriverexception-unknown-error-devtoolsactiveport-file-doesnt-exist-while-t
# ChromeDriver uses the /tmp directory to communicate with Chromium, but Snap remaps /tmp directory to a different location (specifically, to /tmp/snap.chomium/tmp). This causes errors because ChromeDriver can't find files created by Chromium. ChromeDriver is designed and tested with Google Chrome, and it may have compatibility issues with third-party distributions.

options.add_argument('--user-data-dir={}'.format(homedir))
options.add_argument('--remote-debugging-port=9222')

# options.add_argument('--user-agent="Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:72.0) Gecko/20100101 Firefox/72.0"')
options.add_argument('--user-agent="{}"'.format(user_agent))
options.add_argument('--window-size=1920, 1080')

driver = webdriver.Chrome( executable_path = executable_path, options = options)

try:
  params = { }
  print('Browser.getVersion:'.format(params))
  result = send_command_and_get_result(driver, 'Browser.getVersion', params)
  print( result.keys())
  result_keys = ['jsVersion', 'product', 'revision', 'userAgent' ]
  # print the subset of result keys
  for data in result_keys:
    print('{}: {}'.format(data, result[data]))
 
except Exception as e:
  print(e)
 
finally:
  driver.close()
  driver.quit()


