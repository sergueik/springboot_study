#-*- coding: utf-8 -*-

# origin: https://gist.github.com/wowkin2/a2b234c87290f6959c815d3c21336278
# origin: https://cryptobook.nakov.com/symmetric-key-ciphers/aes-encrypt-decrypt-examples

import base64
import hashlib
from Crypto import Random
from Crypto.Cipher import AES
from Crypto.Hash import SHA512
import pbkdf2
import os
import argparse


BS = 16
pad = lambda s: s + (BS - len(s) % BS) * chr(BS - len(s) % BS).encode()
unpad = lambda s: s[:-ord(s[len(s)-1:])]

class AESCipher(object):

  # unused
  @staticmethod
  def pkcs12_password_to_bytes(password):
    """
    Converts a password string to a PKCS12 v1.0 compliant byte array.
      :param password: byte[] - the password as simple string
      :return: The unsigned byte array holding the password
    """
    pkcs12_bytes = [0x00] * (len(password) + 1) * 2

    for i in range(0, len(password)):
      digit = ord(password[i])
      pkcs12_bytes[i * 2] = digit >> 8
      pkcs12_bytes[i * 2 + 1] = digit

    return bytearray(pkcs12_bytes)

  def __init__(self, password, salt = None ):
    self.password = password
    # self.password = AESCipher.pkcs12_password_to_bytes(password)
    # TypeError: passphrase must be str or unicode
    if salt == None:
      self.salt_bytes = os.urandom(16)
    else:
      self.salt_bytes = bytes(bytearray.fromhex(salt))
      # self.salt = bytes(salt, 'utf-8')
    # print(type(self.salt))

  def encrypt(self, value):
    """
    It is assumed that you use Python 3.0+
    and plaintext's type must be str type(== unicode).
    """
    value_bytes = value.encode()
    padded_value_bytes = pad(value_bytes)
    print('salt (encrypt): {}'.format(self.salt_bytes.hex()))
    # https://cryptobook.nakov.com/mac-and-key-derivation/pbkdf2
    derived_bytes = pbkdf2.PBKDF2(self.password, self.salt_bytes,1000,SHA512)
    self.key = derived_bytes.read(32)
    # print(len(self.key))
    print('key (encrypt): {}'.format(self.key.hex()))        
    self.iv = derived_bytes.read(16)
    print('iv (encrypt): {}'.format(self.iv.hex()))        
    # print('salt (1): {}'.format(base64.b64encode(self.salt ).decode('utf-8')))
    # https://www.dlitz.net/software/pycrypto/api/2.6/Crypto.Cipher.AES-module.html

    cipher = AES.new(self.key, AES.MODE_CBC, self.iv)
    cipher.block_size = 32
    encrypted_bytes = cipher.encrypt(padded_value_bytes)
    return base64.b64encode(self.salt_bytes + self.iv + encrypted_bytes).decode('utf-8')

  def decrypt(self, value):
    data = base64.b64decode(value)
    self.salt_bytes = data[:16]
    print('salt (decrypt): {}'.format(self.salt_bytes.hex()))
    iv = data[16:32]
    encrypted_bytes = data[32:]
    print('enc: {}'.format(str(encrypted_bytes.hex())))
    derived_bytes = pbkdf2.PBKDF2(self.password, self.salt_bytes, 1000, SHA512)
    self.key = derived_bytes.read(32)
    print('key (decrypt): {}'.format(self.key.hex()))        
    self.iv = derived_bytes.read(16)
    print('iv (decrypt): {}'.format(self.iv.hex()))        

    cipher = AES.new(self.key, AES.MODE_CBC, self.iv)
    cipher.block_size = 32
    decrypted_bytes = cipher.decrypt(encrypted_bytes)
    print('dec: {}'.format(str(decrypted_bytes.hex())))
    # print('dec(2): "{}"'.format(decrypted_bytes.decode('utf-8')))
    # print('salt: {}'.format(base64.b64encode(self.salt ).decode('utf-8')))        
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
  
  
if __name__ == '__main__':
  main()
