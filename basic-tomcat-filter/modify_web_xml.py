#!/usr/bin/env python

from __future__ import print_function
import re
import time
from os import getenv, path
import sys
import json, base64
from xml.dom import minidom
from xml.dom.minidom import getDOMImplementation
import argparse

def replace_placeholders(dom_fragment_data, replacers):
  for key in replacers:
    if args.debug:
      print('key: "{}"'.format(key))
    search_pattern = '\${{{}}}'.format(key)
    if args.debug:
      print('replacement search: "{}"'.format(search_pattern))
    dom_fragment_data = re.sub(search_pattern, replacers[key], dom_fragment_data)
  if args.debug:
    print(dom_fragment_data)
  return dom_fragment_data

# https://docs.python.org/2/library/xml.dom.minidom.html
# https://stackoverflow.com/questions/10499534/xml-python-parsing-get-parent-node-name-minidom
def add_node(parent_document, parent_element, nodeData = None):
  if nodeData == None:
    impl = getDOMImplementation()
    newdoc = impl.createDocument(None, "some_tag", None)
    top_element = parent_document.documentElement
    child_node = newdoc.createTextNode('Some textual content.')
  else:
    child_node = minidom.parseString(nodeData).documentElement
  parent_element.appendChild(child_node)

# fragment of catalina web.xml modified to feature node attribute
filter_data = """
<filter>
    <filter-name>${filter_name}</filter-name>
    <filter-class>${java_class}</filter-class>
    <init-param>
      <param-name>Expires</param-name>
      <param-value>0</param-value>
    </init-param>
  </filter>
"""
filter_mapping_data = """
  <filter-mapping>
    <filter-name>${filter_name}</filter-name>
    <url-pattern>${url_pattern}</url-pattern>
  </filter-mapping>
"""
# https://docs.python.org/3/library/argparse.html
parser = argparse.ArgumentParser(prog = 'modify_web_xml')
parser.add_argument('--inputfile', '-i', help = 'input file')
parser.add_argument('--outputfile', '-o', help = 'output file', type = str, action = 'store')
parser.add_argument('--filter_name', '-f', help = 'filter name', type = str, action = 'store')
parser.add_argument('--java_class', '-c', help = 'java class', type = str, action = 'store')
parser.add_argument('--url_pattern', '-u', help = 'url pattern', type = str, action = 'store')
parser.add_argument('--debug', '-d', help = 'debug', action = 'store_const', const = True)
#
# TODO: load filter param via argument parse somehow

args = parser.parse_args()
if args.debug:
  print('running debug mode')
  print('input file: "{}"'.format(args.inputfile))
  print('output file: "{}"'.format(args.inputfile))

if args.inputfile == None or args.outputfile == None:
  parser.print_help()
  exit(1)
if args.filter_name == None :
  args.filter_name = 'responseHeadersFilter'
if args.url_pattern == None :
  args.url_pattern = '/*'
if args.java_class == None:
  args.java_class = 'example.responseHeadersFilter'

# http://zetcode.com/python/create-dictionary/
replacers = dict()
replacers.update([('java_class', args.java_class),('filter_name',args.filter_name),('url_pattern', args.url_pattern)])
xmldoc = minidom.parse(args.inputfile)
dom_fragment_data = re.sub(r' +', ' ', re.sub(r'(\n+)', r' ', filter_data.strip()))
child_node = minidom.parseString(replace_placeholders(dom_fragment_data, replacers)).documentElement
xmldoc.documentElement.appendChild(child_node)
dom_fragment_data = re.sub(r' +', ' ', re.sub(r'(\n+)', r' ', filter_mapping_data.strip()))
child_node = minidom.parseString(replace_placeholders(dom_fragment_data, replacers)).documentElement
xmldoc.documentElement.appendChild(child_node)

file2 = open(args.outputfile,'w+')
xmldoc.writexml(file2)
