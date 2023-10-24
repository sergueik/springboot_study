#-*- coding: utf-8 -*-
# origin:
from __future__ import print_function

import base64
import hashlib
import re
import os
import argparse
from Crypto import Random
from Crypto.Cipher import AES

from cryptography.hazmat.primitives import hashes
from cryptography.hazmat.backends import default_backend
from cryptography.hazmat.primitives.kdf.pbkdf2 import PBKDF2HMAC
BS = 16
pad = lambda s: s + (BS - len(s) % BS) * chr(BS - len(s) % BS).encode()

def encrypt(value, password, salt = None) :
  if salt == None:
    salt_bytes = os.urandom(16)
  else:
    salt_bytes = bytes(bytearray.fromhex(salt))

  print('salt: {}'.format(str(salt_bytes.hex())))
  kdf = PBKDF2HMAC(
    algorithm = hashes.SHA512(),
    length = 48,
    salt = salt_bytes,
    iterations = 1000,
     backend = default_backend
  )
  password = 'password'
  password_bytes = bytearray()
  password_bytes.extend(map(ord, password))

  derivedbytes = kdf.derive(password_bytes)
  key = derivedbytes[:32]
  iv = derivedbytes[32:48]
  print('key: {}'.format(str(key.hex())))
  print('iv: {}'.format(str(iv.hex())))
  value_bytes = value.encode()

  padded_value_bytes = pad(value_bytes)

  print('len(raw): {}'.format(len(padded_value_bytes)))
  cipher = AES.new(key, AES.MODE_CBC, iv)
  cipher.block_size = 32
  encrypted_bytes = cipher.encrypt(padded_value_bytes)
  print('enc: {}'.format(str(encrypted_bytes.hex())))
  return base64.b64encode(salt_bytes + iv + encrypted_bytes).decode('utf-8')

def main():

  parser = argparse.ArgumentParser(prog = 'app.py')
  parser.add_argument('--operation', '-o', help = 'operation', type = str, action = 'store')
  parser.add_argument('--password', '-p', help = 'password', type = str, action = 'store')
  parser.add_argument('--value', '-v', help = 'value', type = str, action = 'store')
  parser.add_argument('--salt', '-s', help = 'salt', type = str, action = 'store')
  parser.add_argument('--debug', '-d', help = 'debug', action = 'store_const', const = True)
  args = parser.parse_args()
  if args.debug:
    print('running debug mode')

  if args.operation == None :
    args.operation = 'encrypt'
  if args.password == None :
    args.password = 'password'
  if args.value == None:
    args.value = 'test'

  if args.operation == 'decrypt':
    if args.salt == None:
      result = AESCipher(args.password).decrypt(args.value)
    else:
      result = AESCipher(args.password,args.salt).decrypt(args.value)
    print('decrypted: {}'.format(result))
  else:
    if args.salt == None :
      result = encrypt(args.value,args.password)
    else:
      result = encrypt(args.value,args.password,args.salt)
    print('encrypted: {}'.format(result))

if __name__ == '__main__':
  main()
