# NOTE: pinned versions are specific to 3.12
FROM alpine:3.12

# Lock versions of critical packages for more predictable container behavior.
ENV CACTI_VERSION 1.2.14
ENV APACHE_VERSION 2.4.53-r0
ENV PHP_VERSION 7.3.33-r0
ENV MARIADB_VERSION 10.4.24-r0
ENV SNMP_VERSION 5.8-r3

# Install all the things we need, to do everything.
RUN /sbin/apk --quiet --no-cache add \
    apache2=${APACHE_VERSION} \
    argon2-libs \
    autoconf \
    automake \
    bash \
    binutils \
    brotli-libs \
    cairo \
    coreutils \
    curl \
    db \
    distcc \
    encodings \
    expat \
    file \
    font-alias \
    fontconfig \
    font-sony-misc \
    freetype \
    fribidi \
    g++ \
    gawk \
    gcc \
    gd \
    glib \
    gmp \
    graphite2 \
    harfbuzz \
    help2man \
    libacl \
    libattr \
    libblkid \
    libbsd \
    libbz2 \
    libedit \
    libffi \
    libfontenc \
    libgcc \
    libice \
    libintl \
    libjpeg-turbo \
    libldap \
    libmount \
    libpng \
    libressl-dev \
    librrd \
    libsasl \
    libsm \
    libstdc++ \
    libtool \
    libuuid \
    libwebp \
    libx11 \
    libxau \
    libxcb \
    libxdmcp \
    libxext \
    libxft \
    libxml2 \
    libxpm \
    libxrender \
    libxt \
    make \
    mariadb=${MARIADB_VERSION} \
    mariadb-client=${MARIADB_VERSION} \
    mariadb-client=${MARIADB_VERSION} \
    mariadb-common=${MARIADB_VERSION} \
    mariadb-dev=${MARIADB_VERSION} \
    mkfontscale \
    ncurses-libs \
    ncurses-terminfo-base \
    net-snmp=${SNMP_VERSION} \
    net-snmp-agent-libs=${SNMP_VERSION} \
    net-snmp-dev=${SNMP_VERSION} \
    net-snmp-libs=${SNMP_VERSION} \
    net-snmp-tools=${SNMP_VERSION} \
    net-snmp-perl=${SNMP_VERSION} \
    net-snmp-openrc=${SNMP_VERSION} \
    openrc=0.42.1-r13 \
    pango \
    patch \
    pcre \
    pcre2 \
    perl \
    php7=${PHP_VERSION} \
    php7-apache2=${PHP_VERSION} \
    php7-common=${PHP_VERSION} \
    php7-ctype=${PHP_VERSION} \
    php7-dom=${PHP_VERSION} \
    php7-gd=${PHP_VERSION} \
    php7-gettext=${PHP_VERSION} \
    php7-gmp=${PHP_VERSION} \
    php7-json=${PHP_VERSION} \
    php7-ldap=${PHP_VERSION} \
    php7-mbstring=${PHP_VERSION} \
    php7-mysqlnd=${PHP_VERSION} \
    php7-openssl=${PHP_VERSION} \
    php7-pdo_mysql=${PHP_VERSION} \
    php7-pdo=${PHP_VERSION} \
    php7-posix=${PHP_VERSION} \
    php7-session=${PHP_VERSION} \
    php7-simplexml=${PHP_VERSION} \
    php7-snmp=${PHP_VERSION} \
    php7-sockets=${PHP_VERSION} \
    php7-xml=${PHP_VERSION} \
    pixman \
    readline \
    rrdtool \
    sudo \
    ttf-dejavu \
    tzdata \
    vim \
    wget \
    xz-libs

# Move all the default configs into a backup location,
# from where they _might_ be restored later in the container startup process
# Init script checks for existing resources and copies in defaults if none are found or recognized
RUN BACKUPDIR="/root/default-configs" && \
    mkdir -p ${BACKUPDIR}/mysql ${BACKUPDIR}/apache ${BACKUPDIR}/php7 /run/apache2 /run/openrc /var/backups && \
    mv /etc/mysql /etc/my.cnf /etc/my.cnf.d ${BACKUPDIR}/mysql/ && \
    mv /etc/apache2/* ${BACKUPDIR}/apache/ && \
    mv /etc/php7/* ${BACKUPDIR}/php7/ && \
    touch /run/openrc/softlevel

# Install Cacti
RUN wget -q https://www.cacti.net/downloads/cacti-${CACTI_VERSION}.tar.gz -O /opt/cacti-${CACTI_VERSION}.tar.gz && \
    tar -xf /opt/cacti-${CACTI_VERSION}.tar.gz -C /opt/ && \
    rm -f /opt/cacti-${CACTI_VERSION}.tar.gz && \
    mv /opt/cacti-${CACTI_VERSION} /opt/cacti && \
    ln -s /opt/cacti /var/www/localhost/htdocs/cacti && \
    chown -R apache:apache /opt/cacti
ADD cacti-templates/* /opt/cacti/install/templates/

# Install spine.
# Naturally, spine's version is locked to cacti's version, because versioning.
# https://www.cacti.net/downloads/docs/html/unix_configure_spine.html
RUN wget -q http://www.cacti.net/downloads/spine/cacti-spine-${CACTI_VERSION}.tar.gz -O /opt/cacti-spine-${CACTI_VERSION}.tar.gz && \
    tar -zxf /opt/cacti-spine-${CACTI_VERSION}.tar.gz -C /opt/ && \
    cd /opt/cacti-spine-${CACTI_VERSION} && \
    /usr/bin/aclocal && \
    /usr/bin/libtoolize --force && \
    /usr/bin/autoheader && \
    /usr/bin/autoconf && \
    /usr/bin/automake --add-missing && \
    /usr/bin/aclocal && \
    /usr/bin/automake && \
    ./configure && \
    /usr/bin/make && \
    /usr/bin/make install && \
    chown root:root /usr/local/spine/bin/spine && \
    chmod u+s /usr/local/spine/bin/spine && \
    rm -rf /opt/cacti-spine-${CACTI_VERSION}*

# Install a zillion MIBs
RUN mkdir -p /usr/share/snmp/mibs
ADD snmp-mibs.tgz /usr/share/snmp/mibs/
ADD snmp-mibs2.tgz /usr/share/snmp/mibs/
# Make sure all MIBs are active
RUN echo "mibs +ALL" >> /etc/snmp/snmpd.conf && \
    echo "mibs +ALL" >> /etc/snmp/snmp.conf

# Download and install hddtemp
# https://wiki.archlinux.org/index.php/Hddtemp
# https://www.cyberciti.biz/tips/howto-monitor-hard-drive-temperature.html
#RUN cd /tmp && \
#    wget http://download-mirror.savannah.gnu.org/releases/hddtemp/hddtemp-0.3-beta15.tar.bz2 && \
#    tar -jxf hddtemp-0.3-beta15.tar.bz2 && \
#    cd hddtemp-0.3-beta15 && \
#    ./configure && \
#    make && \
#    make install && \
#    rm -rf /tmp/hddtemp-0.3-beta15 /tmp/hddtemp-0.3-beta15.tar.bz2 && \
#    mkdir -p /usr/share/misc && \
#    wget -O /usr/share/misc/hddtemp.db http://download.savannah.nongnu.org/releases/hddtemp/hddtemp.db

# Add our stuff
ADD bin /
ADD cacti-templates /
RUN chmod +x /init
# ignore cannot create directory ‘/var/log/apache2’: File exists
RUN mkdir -p /var/log/apache2 && chown apache:apache /var/log/apache2

# Error: starting container process caused: exec: "/init": permission denied: unknown.

ENTRYPOINT ["/init"]
