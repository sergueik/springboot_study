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

# unused
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

# fragment of catalina web.xml required to Java 8 in servlet (static data)
class_configurations = [
"""
<init-param>
  <param-name>compilerSourceVM</param-name>
  <param-value>1.8</param-value>
</init-param>
"""
,
"""
<init-param>
  <param-name>compilerTargetVM</param-name>
  <param-value>1.8</param-value>
</init-param>
"""
]
# https://docs.python.org/3/library/argparse.html
parser = argparse.ArgumentParser(prog = 'modify_web_xml')
parser.add_argument('--inputfile', '-i', help = 'input file')
parser.add_argument('--outputfile', '-o', help = 'output file', type = str, action = 'store')
parser.add_argument('--java_class', '-c', help = 'java class', type = str, action = 'store')
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
if args.java_class == None:
  args.java_class = 'org.apache.jasper.servlet.JspServlet'

# http://zetcode.com/python/create-dictionary/
replacers = dict()
replacers.update([('java_class', args.java_class)])
xmldoc = minidom.parse(args.inputfile)
new_child_nodes = []

for class_configuration in class_configurations:
  dom_fragment_data = re.sub(r' +', ' ', re.sub(r'(\n+)', r' ', class_configuration.strip()))
  new_child_node = minidom.parseString(replace_placeholders(dom_fragment_data, replacers)).documentElement
  new_child_nodes.append(new_child_node)
nodelist = xmldoc.documentElement.getElementsByTagName('servlet')
for node in nodelist:
  child_nodelist = node.getElementsByTagName('servlet-class')
  for child_node in child_nodelist:
    if child_node.childNodes[0].data == args.java_class:
      for new_child_node in new_child_nodes:
        node.appendChild(new_child_node)

file2 = open(args.outputfile,'w+')
xmldoc.writexml(file2)

