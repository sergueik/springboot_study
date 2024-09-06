#!/bin/bash

cat <<'EOF' > /config.properties
db.user=${db_user}
db.password=${db_password}
db.name=${db_name}
db.ip=${db_ip}
EOF

