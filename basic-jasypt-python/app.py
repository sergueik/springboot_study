from encryptor import StandardPBEStringEncryptor

cryptor = StandardPBEStringEncryptor('PBEWITHSHA256AND256BITAES-CBC')

cryptor.encrypt('secret', 'test', 1000)
