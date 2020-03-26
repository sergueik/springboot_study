#!/usr/bin/env python

from __future__ import print_function
import re
import time
from os import getenv, path
import sys
import json, base64
from xml.dom import minidom
from xml.dom.minidom import getDOMImplementation

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
    <filter-name>responseHeadersFilter</filter-name>
    <filter-class>example.ResponseHeadersFilter</filter-class>
    <init-param>
      <param-name>Expires</param-name>
      <param-value>0</param-value>
    </init-param>
  </filter>
"""
filter_mapping_data = """
  <filter-mapping>
    <filter-name>responseHeadersFilter</filter-name>
    <url-pattern>/*</url-pattern>
  </filter-mapping>
"""
xmldoc = minidom.parse(sys.argv[1])
child_node = minidom.parseString(re.sub(r' +', ' ', re.sub(r'(\n+)', r' ',  filter_data.strip()))).documentElement
xmldoc.documentElement.appendChild(child_node)
child_node = minidom.parseString(re.sub(r' +', ' ', re.sub(r'(\n+)', r' ',  filter_mapping_data.strip()))).documentElement
xmldoc.documentElement.appendChild(child_node)
file2 = open(sys.argv[2],'w+')
xmldoc.writexml(file2)


