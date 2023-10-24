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

# origin: https://gist.github.com/wowkin2/a2b234c87290f6959c815d3c21336278
BS = 16
pad = lambda s: s + (BS - len(s) % BS) * chr(BS - len(s) % BS).encode()
unpad = lambda s: s[:-ord(s[len(s)-1:])]


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

def decrypt(value, password, salt = None):
  data = base64.b64decode(value)
  if salt == None:
    salt_bytes = data[:16]
  else:
    salt_bytes = bytes(bytearray.fromhex(salt))

  print('salt: {}'.format(salt_bytes.hex()))
  iv = data[16:32]
  kdf = PBKDF2HMAC(
    algorithm = hashes.SHA512(),
    length = 48,
    salt = salt_bytes,
    iterations = 1000,
    backend = default_backend
  )
  password_bytes = bytearray()
  password_bytes.extend(map(ord, password))

  derivedbytes = kdf.derive(password_bytes)
  key = derivedbytes[:32]
  iv = derivedbytes[32:48]

  print('key: {}'.format(key.hex()))
  print('iv: {}'.format(iv.hex()))
  encrypted_bytes = data[32:]
  print('enc: {}'.format(str(encrypted_bytes.hex())))

  cipher = AES.new(key, AES.MODE_CBC, iv)
  cipher.block_size = 32
  decrypted_bytes = cipher.decrypt(encrypted_bytes)
  print('dec: {}'.format(str(decrypted_bytes.hex())))
  print('dec(2): "{}"'.format(decrypted_bytes.decode('utf-8')))
  return unpad(decrypted_bytes).decode('utf-8')


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
      result = decrypt(args.value,args.password)
    else:
      result = decrypt(args.value,args.password,args.salt)
    print('decrypted: {}'.format(result))
  else:
    if args.salt == None :
      result = encrypt(args.value,args.password)
    else:
      result = encrypt(args.value,args.password,args.salt)
    print('encrypted: {}'.format(result))

if __name__ == '__main__':
  main()

