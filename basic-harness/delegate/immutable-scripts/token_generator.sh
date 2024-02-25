# Copyright 2022 Harness Inc. All rights reserved.
# Use of this source code is governed by the PolyForm Free Trial 1.0.0 license
# that can be found in the licenses directory at the root of this repository, also available at
# https://polyformproject.org/wp-content/uploads/2020/05/PolyForm-Free-Trial-1.0.0.txt.

# Based on https://willhaley.com/blog/generate-jwt-with-bash/
# JWT Encoder Bash Script

secret=$DELEGATE_TOKEN
accountId=$ACCOUNT_ID
issuer=$ISSUER

  if [ -z "$secret" ]
  then
    echo "Missing DELEGATE_TOKEN in env"
    exit 1
  fi
  if [ -z "$accountId" ]
  then
    echo "Missing ACCOUNT_ID in env"
    exit 1
  fi
  if [ -z "$issuer" ]
  then
    echo "Missing ISSUER in env"
    exit 1
  fi
# Static header fields.
header='{
        "typ": "JWT",
        "alg":"HS256"
}'

payload_template='{"sub":"%s",
                  "issuer":"%s",
                  "iat":%s,
                  "exp":%s}'

# `iat` is set to now, and `exp` is now + 600 seconds.
iat=$(date +%s)
exp=$(($iat + 600))

payload=$(printf "$payload_template" "$accountId" "$issuer" "$iat" "$exp")

base64_encode()
{
        declare input=${1:-$(</dev/stdin)}
        # Use `tr` to URL encode the output from base64.
        printf '%s' "${input}" | base64 | tr -d '=' | tr '/+' '_-' | tr -d '\n'
}

json() {
        declare input=${1:-$(</dev/stdin)}
        printf '%s' "${input}" | tr -d [:space:]
}

hmacsha256_sign()
{
        declare input=${1:-$(</dev/stdin)}
        printf '%s' "${input}" | openssl dgst -binary -sha256 -hmac "${secret}"
}

header_base64=$(echo "${header}" | json | base64_encode)
payload_base64=$(echo "${payload}" | json | base64_encode)

header_payload=$(echo "${header_base64}.${payload_base64}")
signature=$(echo "${header_payload}" | hmacsha256_sign | base64_encode)

JWT_TOKEN="${header_payload}.${signature}"

echo $JWT_TOKEN
