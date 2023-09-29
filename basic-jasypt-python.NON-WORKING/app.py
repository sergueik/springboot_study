from encryptor import StandardPBEStringEncryptor

cryptor = StandardPBEStringEncryptor('PBEWITHSHA256AND256BITAES-CBC')

value1 = cryptor.encrypt('secret', 'test', 1000)
print(value1)

value2 = cryptor.decrypt('secret', value1, 1000)
print(value2)

cryptor = StandardPBEStringEncryptor('PBEWITHSHA512AND256BITAES-CBC')

value1 = cryptor.encrypt('secret', 'test', 1000)
print(value1)

value2 = cryptor.decrypt('secret', value1, 1000)
print(value2)
