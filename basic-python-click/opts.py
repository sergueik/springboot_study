#!/usr/bin/env python3 
import time, datetime, os,sys
import getopt
def show_options():
  print(f'search={text}\ndebug={debug}\noutput={output_file}') 
  print(f'args={", ".join(args)}') 

try:
  opts, args = getopt.getopt(sys.argv[1:], 'hds:o:', ['help', 'debug', 'search=', 'output='])
  usage_help_message = 'usage: print_pdf.py --search <text> --output <output file> [<arg1>,<arg2>,...]'
  # NOTE: when indentation is wrong the error message is misleading:
  #   usage_help_message = 'usage: print_pdf.py --search <text> --output <output file> [<arg1>,<arg2>,...]'
  #                     ^ SyntaxError: invalid syntax

except getopt.GetoptError as err:
  print(usage_help_message)
text = None
outpu_file = None
global debug
debug = False
for option, argument in opts:
  if option == '-d':
    debug = True
  elif option in ('-h', '--help'):
    print(usage_help_message)
    exit()
  elif option in ('-s', '--search'):
    text = argument
  elif option in ('-o', '--output'):
    output_file = argument
  else:
    assert False, 'unhandled option: {}'.format(option)
if text == None or output_file == None:
  print(usage_help_message)
  exit()
show_options()
