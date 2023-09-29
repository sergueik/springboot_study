#-*- coding: utf-8 -*-

# origin: https://gist.github.com/wowkin2/a2b234c87290f6959c815d3c21336278
# origin: https://cryptobook.nakov.com/symmetric-key-ciphers/aes-encrypt-decrypt-examples
# References
# http://www.imcore.net/encrypt-decrypt-aes256-c-objective-ios-iphone-ipad-php-java-android-perl-javascript/
# http://stackoverflow.com/questions/12562021/aes-decryption-padding-with-pkcs5-python
# http://stackoverflow.com/questions/12524994/encrypt-decrypt-using-pycrypto-aes-256
# http://www.di-mgt.com.au/cryptopad.html
# https://github.com/dlitz/pycrypto

import base64
import hashlib
from Crypto import Random
from Crypto.Cipher import AES
from Crypto.Hash import SHA512
import pbkdf2
import os


BS = 16
pad = lambda s: s + (BS - len(s) % BS) * chr(BS - len(s) % BS).encode()
unpad = lambda s: s[:-ord(s[len(s)-1:])]

class AESCipher(object):
    """
    https://github.com/dlitz/pycrypto
    """

    @staticmethod
    def pkcs12_password_to_bytes(password):
        """
        Converts a password string to a PKCS12 v1.0 compliant byte array.

        :param password: byte[] - the password as simple string
        :return: The unsigned byte array holding the password
        """
        pkcs12_pwd = [0x00] * (len(password) + 1) * 2

        for i in range(0, len(password)):
            digit = ord(password[i])
            pkcs12_pwd[i * 2] = digit >> 8
            pkcs12_pwd[i * 2 + 1] = digit

        return bytearray(pkcs12_pwd)

    def __init__(self, password ):
      self.salt = os.urandom(16) 
      # TypeError: passphrase must be str or unicode
      # self.password = AESCipher.pkcs12_password_to_bytes(password)
      self.password = password

    def encrypt(self, value):
        """
        It is assumed that you use Python 3.0+
        , so plaintext's type must be str type(== unicode).
        """
        message = value.encode()
        raw = pad(message)
        print('salt (encrypt): {}'.format(self.salt.hex()))        
        # https://cryptobook.nakov.com/mac-and-key-derivation/pbkdf2
        derivedbytes = pbkdf2.PBKDF2(self.password, self.salt,1000,SHA512)
        self.key = derivedbytes.read(32)
        self.iv = derivedbytes.read(16)
        print('iv (encrypt): {}'.format(self.iv.hex()))        
        # print('salt (1): {}'.format(base64.b64encode(self.salt ).decode('utf-8')))
        cipher = AES.new(self.key, AES.MODE_CBC, self.iv)
        enc = cipher.encrypt(raw)
        return base64.b64encode(self.salt + enc).decode('utf-8')

    def decrypt(self, value):
        data = base64.b64decode(value)
        self.salt = data[:16]
        print('salt (decrypt): {}'.format(self.salt.hex()))        
        enc = data[16:]
        derivedbytes = pbkdf2.PBKDF2(self.password, self.salt,1000,SHA512)
        self.key = derivedbytes.read(32)
        self.iv = derivedbytes.read(16)
        print('iv (decrypt): {}'.format(self.iv.hex()))        

        cipher = AES.new(self.key, AES.MODE_CBC, self.iv)
        dec = cipher.decrypt(enc)
        # print('salt: {}'.format(base64.b64encode(self.salt ).decode('utf-8')))        
        return unpad(dec).decode('utf-8')


password = 'password'
message = 'test'

enc = AESCipher(password).encrypt(message)
dec = AESCipher(password).decrypt(enc)

print('encrypted: {}'.format(enc))
print('decrypted: {}'.format(dec))

