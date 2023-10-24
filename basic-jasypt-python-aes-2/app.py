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
salt = os.urandom(16)
print('salt: {}'.format(str(salt.hex())))
kdf = PBKDF2HMAC(
  algorithm = hashes.SHA512(),
  length = 48, 
  salt = salt,
  iterations = 1000,
   backend = default_backend
)
password = 'password'
password_bytes = bytearray()
password_bytes.extend(map(ord, password))

# password_bytes = bytes(bytearray.fromhex(password))
derivedbytes = kdf.derive(password_bytes)
# NOTE: 
# cryptography.exceptions.AlreadyFinalized: PBKDF2 instances can only be used once
kdf = PBKDF2HMAC(
  algorithm = hashes.SHA512(),
  length = 48, 
  salt = salt,
  iterations = 1000,
   backend = default_backend
)

kdf.verify(password_bytes,derivedbytes)
key = derivedbytes[:32]
iv = derivedbytes[32:48]
print('key: {}'.format(str(key.hex())))
print('iv: {}'.format(str(iv.hex())))
message = 'test'
message_bytes = message.encode()
# message_bytes = bytearray()
# message_bytes .extend(map(ord, message))

raw = pad(message_bytes)

print('len(raw): {}'.format(len(raw)))
cipher = AES.new(key, AES.MODE_CBC, iv)
cipher.block_size = 32
enc = cipher.encrypt(raw)
print('enc: {}'.format(str(enc.hex())))
print ('encrypted: ' + base64.b64encode(salt + iv + enc).decode('utf-8'))

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
      result = AESCipher(args.password).encrypt(args.value)
    else:
      result = AESCipher(args.password,args.salt).encrypt(args.value)
    print('encrypted: {}'.format(result))
  
  
# if __name__ == '__main__':
#  main()
