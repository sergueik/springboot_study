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
# from cryptography.hazmat.backends.interfaces import PBKDF2HMACBackend 
# NOTE: uncommenting the following line leads to the error:
#  ModuleNotFoundError: No module named 'cryptography.hazmat.backends.interfaces'  
# with unclear remediation steps:
# https://stackoverflow.com/questions/31569339/importerror-no-module-named-cryptography-hazmat-backends-boxsdk-on-mac

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
# print( dir(kdf))
password = b'password'
derivedbytes = kdf.derive(password)
# NOTE: 
# cryptography.exceptions.AlreadyFinalized: PBKDF2 instances can only be used once
kdf = PBKDF2HMAC(
  algorithm = hashes.SHA512(),
  length = 48, 
  salt = salt,
  iterations = 1000,
   backend = default_backend
)

kdf.verify(password,derivedbytes)
key = derivedbytes[:32]
iv = derivedbytes[32:48]
print('key: {}'.format(str(key.hex())))
print('iv: {}'.format(str(iv.hex())))
message = 'test'
message = pad(str(message))
raw = bytearray()
raw.extend(map(ord, message))
print('len(raw): {}'.format(len(raw)))
cipher = AES.new(key, AES.MODE_CBC, iv)
cipher.block_size = 32
# TODO: TypeError: can only concatenate str (not "bytes") to str
# raw = pad(message)
raw = message
enc = cipher.encrypt(raw)
print('enc: {}'.format(str(enc.hex())))
print ('encrypted: ' + base64.b64encode(salt + iv + enc).decode('utf-8'))

# TODO: almost identical to the verified example (key,salt,iv identical)
# but encrypted value appears truncated
