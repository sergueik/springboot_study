#!/bin/bash

PROJECT_ID="your-project-id"
TOO_PRIVILEGED_ROLES=("roles/owner" "roles/editor")
ALLOWED_USERS=("alloweduser@example.com")

# Fetch IAM policy
IAM_POLICY=$(gcloud projects get-iam-policy $PROJECT_ID --format=json)

for ROLE in "${TOO_PRIVILEGED_ROLES[@]}"; do
  echo "Checking role: $ROLE"
  MEMBERS=$(echo "$IAM_POLICY" | jq -r --arg ROLE "$ROLE" '.bindings[] | select(.role==$ROLE) | .members[]')

  for MEMBER in $MEMBERS; do
    if [[ ! " ${ALLOWED_USERS[@]} " =~ " ${MEMBER} " ]]; then
      echo "Violation found: $MEMBER has $ROLE"
    else
      echo "$MEMBER is allowed for $ROLE"
    fi
  done
done

