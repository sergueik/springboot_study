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
import pbkdf2
import os


BS = 16
pad = lambda s: s + (BS - len(s) % BS) * chr(BS - len(s) % BS).encode()
unpad = lambda s: s[:-ord(s[len(s)-1:])]

def iv():
    """
    The initialization vector to use for encryption or decryption.
    It is ignored for MODE_ECB and MODE_CTR.
    """
    return chr(0) * 16

class AESCipher(object):
    """
    https://github.com/dlitz/pycrypto
    """

    def __init__(self, password ):
      self.salt = os.urandom(16)
      key = pbkdf2.PBKDF2(password, self.salt).read(32)
      self.key = key
      print('salt (1): {}'.format(base64.b64encode(self.salt ).decode('utf-8')))

    def encrypt(self, value):
        """
        It is assumed that you use Python 3.0+
        , so plaintext's type must be str type(== unicode).
        """
        message = value.encode()
        raw = pad(message)
        cipher = AES.new(self.key, AES.MODE_CBC, iv())
        enc = cipher.encrypt(raw)
        return base64.b64encode(self.salt + enc).decode('utf-8')

    def decrypt(self, value):
        data = base64.b64decode(value)
        self.salt = data[:16]
        print('salt (2): {}'.format(base64.b64encode(self.salt ).decode('utf-8')))        
        enc = data[16:]
        self.key = pbkdf2.PBKDF2(password, self.salt).read(32)

        cipher = AES.new(self.key, AES.MODE_CBC, iv())
        dec = cipher.decrypt(enc)
        # print('salt: {}'.format(base64.b64encode(self.salt ).decode('utf-8')))        
        return unpad(dec).decode('utf-8')


password = 'password'
message = 'test'

enc = AESCipher(password).encrypt(message)
dec = AESCipher(password).decrypt(enc)

print('encrypted: {}'.format(enc))
print('decrypted: {}'.format(dec))

