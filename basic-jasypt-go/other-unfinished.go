package jasypt
// origin: https://github.com/Mystery00/go-jasypt/blob/master/encryption/pbe_hmacsha512_aes256.go

import (
	"bytes"
	"crypto/aes"
	"crypto/cipher"
	"crypto/des"
	"crypto/md5"
	"encoding/base64"
	"regexp"
	"crypto/sha512"
	"encoding/base64"
	"golang.org/x/crypto/pbkdf2"
	"regexp"
	
  "fmt"
  "log"
  "os"
  "os/exec"
  "time"
)

type PBEWithMD5AndDES struct {
	config EncryptorConfig
}

func NewPBEWithMD5AndDES(config EncryptorConfig) *PBEWithMD5AndDES {
	return &PBEWithMD5AndDES{
		config: config,
	}
}

func (enc *PBEWithMD5AndDES) Encrypt(message string) (string, error) {
	saltGenerator := enc.config.SaltGenerator
	ivGenerator := enc.config.IvGenerator
	password := enc.config.Password
	algorithmBlockSize := 8
	keyObtentionIterations := 1000

	salt, err := saltGenerator.GenerateSalt(algorithmBlockSize)
	if err != nil {
		return "", err
	}
	iv, err := ivGenerator.GenerateIv(algorithmBlockSize)
	if err != nil {
		return "", err
	}

	dk, iv := getMd5DerivedKey(password, salt, keyObtentionIterations)
	encText, err := desEncrypt([]byte(message), dk, iv)
	if err != nil {
		return "", err
	}
	result := encText
	if ivGenerator.IncludePlainIvInEncryptionResults() {
		result = append(iv, result...)
	}
	if saltGenerator.IncludePlainSaltInEncryptionResults() {
		result = append(salt, result...)
	}
	//执行Base64编码
	encodeString := base64.StdEncoding.EncodeToString(result)
	return encodeString, nil
}

func (enc *PBEWithMD5AndDES) Decrypt(message string) (string, error) {
	saltGenerator := enc.config.SaltGenerator
	ivGenerator := enc.config.IvGenerator
	password := enc.config.Password
	algorithmBlockSize := 8
	keyObtentionIterations := 1000

	//Base64解码
	encrypted, err := base64.StdEncoding.DecodeString(message)
	if err != nil {
		return "", err
	}
	var salt []byte
	var iv []byte
	if saltGenerator.IncludePlainSaltInEncryptionResults() {
		salt = encrypted[:algorithmBlockSize]
		encrypted = encrypted[algorithmBlockSize:]
	}
	if ivGenerator.IncludePlainIvInEncryptionResults() {
		iv = encrypted[:algorithmBlockSize]
		encrypted = encrypted[algorithmBlockSize:]
	}
	dk, iv := getMd5DerivedKey(password, salt, keyObtentionIterations)
	text, err := desDecrypt(encrypted, dk, iv)
	if err != nil {
		return "", err
	}
	p := regexp.MustCompile(`[\x01-\x08]`)
	return p.ReplaceAllString(string(text), ""), nil
}
func getMd5DerivedKey(password string, salt []byte, count int) ([]byte, []byte) {
	key := md5.Sum([]byte(password + string(salt)))
	for i := 0; i < count-1; i++ {
		key = md5.Sum(key[:])
	}
	return key[:8], key[8:]
}

func pKCS5Padding(cipherText []byte, blockSize int) []byte {
	padding := blockSize - len(cipherText)%blockSize
	padText := bytes.Repeat([]byte{byte(padding)}, padding)
	return append(cipherText, padText...)
}

func pKCS5UnPadding(origData []byte) []byte {
	length := len(origData)
	unPadding := int(origData[length-1])
	return origData[:(length - unPadding)]
}

func desEncrypt(origData, key, iv []byte) ([]byte, error) {
	block, err := des.NewCipher(key)
	if err != nil {
		return nil, err
	}
	origData = pKCS5Padding(origData, block.BlockSize())
	blockMode := cipher.NewCBCEncrypter(block, iv)
	encrypted := make([]byte, len(origData))
	blockMode.CryptBlocks(encrypted, origData)
	return encrypted, nil
}

func desDecrypt(encrypted, key, iv []byte) ([]byte, error) {
	block, err := des.NewCipher(key)
	if err != nil {
		return nil, err
	}

	blockMode := cipher.NewCBCDecrypter(block, iv)
	origData := make([]byte, len(encrypted))
	blockMode.CryptBlocks(origData, encrypted)
	origData = pKCS5UnPadding(origData)
	return origData, nil
}

func aes256Encrypt(origData, key, iv []byte) ([]byte, error) {
	block, err := aes.NewCipher(key)
	if err != nil {
		return nil, err
	}
	blockSize := block.BlockSize()
	origData = pKCS5Padding(origData, blockSize)
	encrypted := make([]byte, len(origData))
	blockMode := cipher.NewCBCEncrypter(block, iv)
	blockMode.CryptBlocks(encrypted, origData)
	return encrypted, nil
}

func aes256Decrypt(encrypted, key, iv []byte) ([]byte, error) {
	block, err := aes.NewCipher(key)
	if err != nil {
		return nil, err
	}
	blockMode := cipher.NewCBCDecrypter(block, iv)
	origData := make([]byte, len(encrypted))
	blockMode.CryptBlocks(origData, encrypted)
	origData = pKCS5UnPadding(origData)
	return origData, nil
}
type PBEWithHMACSHA512AndAES_256 struct {
	config EncryptorConfig
}

func NewPBEWithHMACSHA512AndAES_256(config EncryptorConfig) *PBEWithHMACSHA512AndAES_256 {
	return &PBEWithHMACSHA512AndAES_256{
		config: config,
	}
}

func (enc *PBEWithHMACSHA512AndAES_256) Encrypt(message string) (string, error) {
	saltGenerator := enc.config.SaltGenerator
	ivGenerator := enc.config.IvGenerator
	password := enc.config.Password
	algorithmBlockSize := 16
	keyObtentionIterations := 1000

	salt, err := saltGenerator.GenerateSalt(algorithmBlockSize)
	if err != nil {
		return "", err
	}
	iv, err := ivGenerator.GenerateIv(algorithmBlockSize)
	if err != nil {
		return "", err
	}

	dk := pbkdf2.Key([]byte(password), salt, keyObtentionIterations, 32, sha512.New)
	encText, err := aes256Encrypt([]byte(message), dk, iv)
	if err != nil {
		return "", err
	}
	result := encText
	if ivGenerator.IncludePlainIvInEncryptionResults() {
		result = append(iv, result...)
	}
	if saltGenerator.IncludePlainSaltInEncryptionResults() {
		result = append(salt, result...)
	}
	//执行Base64编码
	encodeString := base64.StdEncoding.EncodeToString(result)
	return encodeString, nil
}

func (enc *PBEWithHMACSHA512AndAES_256) Decrypt(message string) (string, error) {
	saltGenerator := enc.config.SaltGenerator
	ivGenerator := enc.config.IvGenerator
	password := enc.config.Password
	algorithmBlockSize := 16
	keyObtentionIterations := 1000

	//Base64解码
	encrypted, err := base64.StdEncoding.DecodeString(message)
	if err != nil {
		return "", err
	}
	var salt []byte
	var iv []byte
	if saltGenerator.IncludePlainSaltInEncryptionResults() {
		salt = encrypted[:algorithmBlockSize]
		encrypted = encrypted[algorithmBlockSize:]
	}
	if ivGenerator.IncludePlainIvInEncryptionResults() {
		iv = encrypted[:algorithmBlockSize]
		encrypted = encrypted[algorithmBlockSize:]
	}
	dk := pbkdf2.Key([]byte(password), salt, keyObtentionIterations, 32, sha512.New)
	text, err := aes256Decrypt(encrypted, dk, iv)
	if err != nil {
		return "", err
	}
	p := regexp.MustCompile(`[\x01-\x08]`)
	return p.ReplaceAllString(string(text), ""), nil
}
