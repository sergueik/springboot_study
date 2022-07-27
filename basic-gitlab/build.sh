#!/bin/sh

USER=git
DIR=/var/opt/gitlab

# show execution, stop on error
set -xe

apk upgrade --no-cache

# busybox contains bug in env command preventing gitaly setup, upgrade it
apk add busybox=1.28.4-r0 --no-cache --repository=https://nl.alpinelinux.org/alpine/edge/main

# install runtime deps
apk add --no-cache openssh-server git nginx postgresql redis nodejs-current icu-libs libre2
apk add --no-cache postgresql-contrib # required for extensions
apk add --no-cache ruby ruby-irb ruby-io-console ruby-bigdecimal ruby-json
apk add --no-cache sudo # considered bad practice but we really need it
apk add --no-cache procps # to replace busybox pkill

# install build deps
apk add --no-cache --virtual .build-deps \
gcc g++ make cmake linux-headers \
icu-dev ruby-dev musl-dev postgresql-dev zlib-dev libffi-dev libre2-dev \
python2 go yarn

# generate server keys (replace them in production!)
ssh-keygen -A

# create gitlab user
adduser -D -g 'GitLab' $USER
# $DIR is main mountpoint for gitlab data volume
mkdir $DIR && cd $DIR && mkdir data repo config
chown -R $USER:$USER $DIR
# openssh daemon does not allow locked user to login, change ! to *
sed --in-place "s/$USER:!/$USER:*/" /etc/shadow
# sudo no tty fix
echo "$USER ALL=(ALL) NOPASSWD: ALL" >>/etc/sudoers

# configure nginx
mkdir /run/nginx
CONFIG=$DIR/config/nginx
mv /etc/nginx $CONFIG
ln -s $CONFIG /etc/nginx
DEFAULT=/etc/nginx/conf.d/default.conf
mv $DEFAULT $DEFAULT.bak

# configure postgres
mkdir /run/postgresql
chown postgres:postgres $DIR/data /run/postgresql
sudo -u postgres pg_ctl initdb --pgdata $DIR/data
sudo -u postgres pg_ctl start --pgdata $DIR/data
sleep 5 # wait postgres starting
sudo -u postgres psql -d template1 -c "CREATE USER $USER CREATEDB;"
sudo -u postgres psql -d template1 -c "CREATE EXTENSION IF NOT EXISTS pg_trgm;"
sudo -u postgres psql -d template1 -c "CREATE DATABASE gitlabhq_production OWNER $USER;"
# install extension
sudo -u $USER -H psql --dbname gitlabhq_production <<CMD
SELECT true AS enabled
FROM pg_available_extensions
WHERE name = 'pg_trgm'
AND installed_version IS NOT NULL;
CMD

# configure redis
CONFIG=$DIR/config/redis.conf
SOCKET=/var/run/redis/redis.sock
cp /etc/redis.conf $CONFIG
sed --in-place "s/^port .*/port 0/" $CONFIG
echo "unixsocket $SOCKET" >>$CONFIG
echo "unixsocketperm 770" >>$CONFIG
# !--following 3 lines not needed, socket dir set up correctly by alpine redis package
# !--mkdir /var/run/redis
# !--chown redis:redis /var/run/redis
# !--chmod 755 /var/run/redis
sed --in-place "s/^redis:.*/&,git/" /etc/group # add git user to redis group
sudo -u redis redis-server $CONFIG # start redis

# adjust git settings
sudo -u $USER -H git config --global gc.auto 0
sudo -u $USER -H git config --global core.autocrlf input
sudo -u $USER -H git config --global repack.writeBitmaps true
sudo -u $USER -H git config --global receive.advertisePushOptions true

# pull gitlab
cd /home/$USER
sudo -u $USER -H git clone --depth 1 --branch v$VERSION https://gitlab.com/gitlab-org/gitlab-ce.git gitlab

# configure gitlab
CONFIG=$DIR/config/gitlab
sudo -u $USER -H mkdir $CONFIG
cd /home/$USER/gitlab
sudo -u $USER -H cp config/gitlab.yml.example $CONFIG/gitlab.yml
sudo -u $USER -H cp config/unicorn.rb.example $CONFIG/unicorn.rb
sudo -u $USER -H cp config/resque.yml.example $CONFIG/resque.yml
sudo -u $USER -H cp config/secrets.yml.example $CONFIG/secrets.yml
sudo -u $USER -H cp config/database.yml.postgresql $CONFIG/database.yml
sudo -u $USER -H cp config/initializers/rack_attack.rb.example $CONFIG/rack_attack.rb

sudo -u $USER -H ln -s $CONFIG/* config
sudo -u $USER -H mv config/rack_attack.rb config/initializers

sed --in-place "s/# user:.*/user: $USER/" config/gitlab.yml
sed --in-place "s/host: localhost/host: $DOMAIN/" config/gitlab.yml
sed --in-place "s:/home/git/repositories:$DIR/repo:" config/gitlab.yml
sed --in-place "s:/home/git:/home/$USER:g" config/unicorn.rb
sed --in-place "s/YOUR_SERVER_FQDN/$DOMAIN/" lib/support/nginx/gitlab

# move log dir to /var/log data volume mount point
mv log /var/log/gitlab
sudo -u $USER -H ln -s /var/log/gitlab log

# set permissions
chmod o-rwx config/database.yml
chmod 0600 config/secrets.yml
chown -R $USER log/
chown -R $USER tmp/
chmod -R u+rwX,go-w log/
chmod -R u+rwX tmp/
chmod -R u+rwX tmp/pids/
chmod -R u+rwX tmp/sockets/
chmod -R u+rwX builds/
chmod -R u+rwX shared/artifacts/
chmod -R ug+rwX shared/pages/

# set repo permissions
chmod -R ug+rwX,o-rwx $DIR/repo
chmod -R ug-s $DIR/repo
find $DIR/repo -type d -print0 | sudo xargs -0 chmod g+s

# create uploads dir
sudo -u $USER -H mkdir public/uploads
chmod 0700 public/uploads

# install bundler
gem install bundler --no-ri --no-rdoc

# to parallelize bundler jobs
CPU_COUNT=`awk '/^processor/{n+=1}END{print n}' /proc/cpuinfo`

# install gems
sudo -u $USER -H bundle install --jobs=$CPU_COUNT --deployment --without development test mysql aws kerberos

# install gitlab shell
sudo -u $USER -H bundle exec rake gitlab:shell:install REDIS_URL=unix:$SOCKET RAILS_ENV=production SKIP_STORAGE_VALIDATION=true

# install gitlab-workhorse
sudo -u $USER -H bundle exec rake "gitlab:workhorse:install[/home/$USER/gitlab-workhorse]" RAILS_ENV=production

# install gitaly
sudo -u $USER -H bundle exec rake "gitlab:gitaly:install[/home/$USER/gitaly]" RAILS_ENV=production
chmod 0700 /home/$USER/gitlab/tmp/sockets/private
chown $USER /home/$USER/gitlab/tmp/sockets/private
# run gitaly
sudo -u $USER /home/$USER/gitaly/gitaly /home/$USER/gitaly/config.toml &>/dev/null &
sleep 5

# initialize database
echo yes | sudo -u $USER -H bundle exec rake gitlab:setup RAILS_ENV=production

# compile GetText PO files
sudo -u $USER -H bundle exec rake gettext:compile RAILS_ENV=production

# compile assets
sudo -u $USER -H yarn install --production --pure-lockfile
sudo -u $USER -H bundle exec rake gitlab:assets:compile RAILS_ENV=production NODE_ENV=production

# busybox pkill has no support of -u option, replace with procps pkill
rm /usr/bin/pkill
ln -s /bin/pkill /usr/bin/pkill

# install support scripts
cp lib/support/init.d/gitlab /etc/init.d/gitlab
cp lib/support/logrotate/gitlab /etc/logrotate.d/gitlab
cp lib/support/nginx/gitlab /etc/nginx/conf.d/gitlab.conf

# create defaults file
DEFAULTS=/etc/default/gitlab
mkdir /etc/default && touch $DEFAULTS
# for entrypoint script
echo "DOMAIN=$DOMAIN" >>$DEFAULTS
echo "USER=$USER" >>$DEFAULTS
echo "DIR=$DIR" >>$DEFAULTS
echo "SOCKET=$SOCKET" >>$DEFAULTS
# for gitlab init script
echo "app_user=$USER" >>$DEFAULTS
echo "shell_path=/bin/sh" >>$DEFAULTS

# cleanup build deps
apk del .build-deps

# these dirs waste a lot of space and not needed in runtime, remove them
rm -rf node_modules .git
rm -rf /home/$USER/.cache/yarn

# cleanup sudo no tty fix
sed --in-place "/$USER.*/d" /etc/sudoers

# stop services
sudo -u $USER pkill gitaly
sudo -u postgres pg_ctl stop --mode smart --pgdata $DIR/data
sudo -u redis redis-cli -s $SOCKET shutdown
sleep 5 # wait services stopping
