#! /bin/bash
apt update
apt -y install apache2
HOST=$(hostname -i)
cat <<EOF > /var/www/html/index.html
<html><body><p>Hello $HOST World!</p></body></html>
EOF