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

    def __init__(self, key):
        self.key = key
        #self.key = hashlib.sha256(key.encode()).digest()

    def encrypt(self, message):
        """
        It is assumed that you use Python 3.0+
        , so plaintext's type must be str type(== unicode).
        """
        message = message.encode()
        raw = pad(message)
        cipher = AES.new(self.key, AES.MODE_CBC, iv())
        enc = cipher.encrypt(raw)
        return base64.b64encode(enc).decode('utf-8')

    def decrypt(self, enc):
        enc = base64.b64decode(enc)
        cipher = AES.new(self.key, AES.MODE_CBC, iv())
        dec = cipher.decrypt(enc)
        return unpad(dec).decode('utf-8')


# NOTE :pbkdf call is not used yet
# leading to
# key = 'password'
# ValueError: AES key must be either 16, 24, or 32 bytes long
# key = 'abcdefghijklmnopqrstuvwxyz123456'
password = 'password'
salt = os.urandom(16)
key = pbkdf2.PBKDF2(password, salt).read(32)

message = 'test'

enc = AESCipher(key).encrypt(message)
dec = AESCipher(key).decrypt(enc)

print(enc)
print(dec)
