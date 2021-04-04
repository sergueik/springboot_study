FROM alpine:3.9

RUN apk update

RUN adduser -D -g users user
RUN mkdir -p /home/user \
	&& chown user:users /home/user

RUN sh -c 'test -d /opt || mkdir -p /opt'
RUN test -d /opt || mkdir -p /opt
COPY bootstrap.sh /opt
RUN apk add xvfb x11vnc fluxbox xdpyinfo st vim terminus-font \
	&& rm -vrf /var/cache/apk/*
RUN sed -r -i "s/\[exec\] \(xterm\) \{xterm\}/\[exec\] \(st\) \{st -f 'xos4 Terminus-14'\}/" /usr/share/fluxbox/menu

USER user
ENV HOME /home/user
WORKDIR /home/user
CMD ["/opt/bootstrap.sh"]

