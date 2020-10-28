FROM nginx:alpine
# Based on: https://hub.docker.com/r/yobasystems/alpine-nginx/dockerfile
ENV UID=1000
# remove default configuration
RUN  rm -f /etc/nginx/conf.d/default.conf
# TODO:
ARG USER_NAME=myuser
ARG GROUP_NAME=myuser
RUN echo "@edge http://nl.alpinelinux.org/alpine/edge/main" >> /etc/apk/repositories \
    && apk update && apk add --no-cache nano \
    && addgroup -S $GROUP_NAME \ 
    && adduser -h /home/$USER_NAME -u $UID -s /bin/sh -D -G $GROUP_NAME $USER_NAME \
    && mkdir -p /logs \
    && chown -R $USER_NAME:$GROUP_NAME /logs

# TODO: cannot run apk - need to save intermediate image
# USER $USER_NAME
RUN \
   BUILD_PACKAGES="build-base linux-headers openssl-dev pcre-dev wget zlib-dev" \
   && runtime_pkgs="ca-certificates openssl pcre zlib" \
   && apk --update add ${BUILD_PACKAGES} ${runtime_pkgs} \
   && cd /tmp \
   && wget -q http://nginx.org/download/nginx-1.15.8.tar.gz \
   && tar xzf nginx-1.15.8.tar.gz \
   &&  cd /tmp/nginx-1.15.8\
   && ./configure \
    --prefix=/etc/nginx \
    --sbin-path=/usr/sbin/nginx \
    --conf-path=/etc/nginx/conf.d/default.conf \
    --error-log-path=/logs/error.log \
    --http-log-path=/logs/access.log \
    --pid-path=/var/run/nginx.pid \
    --lock-path=/var/run/nginx.lock \
    --http-client-body-temp-path=/var/cache/nginx/client_temp \
    --http-proxy-temp-path=/var/cache/nginx/proxy_temp \
    --http-fastcgi-temp-path=/var/cache/nginx/fastcgi_temp \
    --http-uwsgi-temp-path=/var/cache/nginx/uwsgi_temp \
    --http-scgi-temp-path=/var/cache/nginx/scgi_temp \
    --user=$USER_NAME \
    --group=$GROUP_NAME \
    --with-http_ssl_module \
    --with-http_realip_module \
    --with-http_addition_module \
    --with-http_sub_module \
    --with-http_dav_module \
    --with-http_flv_module \
    --with-http_mp4_module \
    --with-http_gunzip_module \
    --with-http_gzip_static_module \
    --with-http_random_index_module \
    --with-http_secure_link_module \
    --with-http_stub_status_module \
    --with-http_auth_request_module \
    --with-mail \
    --with-mail_ssl_module \
    --with-file-aio \
    --with-ipv6 \
    --with-threads \
    --with-stream \
    --with-stream_ssl_module \
    --with-http_slice_module \
    --with-http_v2_module \
    && make \
    && make install \
    && sed -i -e 's/#access_log  logs\/access.log  main;/access_log \/dev\/stdout;/' -e 's/#error_log  logs\/error.log  notice;/error_log stderr notice;/' /etc/nginx/nginx.conf \
  && rm -rf /tmp/* \
  && apk del ${BUILD_PACKAGES} \
  && rm -rf /var/cache/apk/*

EXPOSE 80 443
COPY ./app.conf /etc/nginx/conf.d/default.conf

# COPY files/index.html /etc/nginx/html/
# COPY files/nginx.conf /etc/nginx/nginx.conf
# CMD ["nginx", "-g 'daemon off;'"]
CMD ["nginx", "-g", "daemon off;"]
