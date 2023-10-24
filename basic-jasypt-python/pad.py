#-*- coding: utf-8 -*-

import base64
import hashlib
import os
import argparse

BS = 16
pad = lambda s: s + (BS - len(s) % BS) * chr(BS - len(s) % BS).encode()
unpad = lambda s: s[:-ord(s[len(s)-1:])]

def main():

  parser = argparse.ArgumentParser(prog = 'pad.py')
  parser.add_argument('--operation', '-o', help = 'operation', type = str, action = 'store')
  parser.add_argument('--value', '-v', help = 'value', type = str, action = 'store')
  parser.add_argument('--debug', '-d', help = 'debug', action = 'store_const', const = True)
  args = parser.parse_args()
  if args.debug:
    print('running debug mode')

  if args.operation == None :
    args.operation = 'pad'
  if args.value == None:
    args.value = 'test'

  if args.operation == 'unpad':
    raw = bytes(bytearray.fromhex(args.value))
    print('raw: {}'.format(raw.hex()))
    result = unpad(raw)
    print('raw result: {}'.format(result.hex()))
    print('result: "{}"'.format(result.decode('utf-8')))
  else:
    message = args.value.encode()
    raw = pad(message)
    print('pad: {}'.format(raw.hex()))

if __name__ == '__main__':
  main()
