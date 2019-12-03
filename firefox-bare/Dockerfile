FROM openjdk:8
ARG firefox_ver="47.0"
# This is archaic version of Mozilla Firefox before introduction of gecko driver
ARG firefox_ver="70.0"
# This version of Mozilla Firefox when healess was introduced
ARG geckodriver_ver=0.26.0
RUN apt-get update && apt-get -q -y install curl
RUN mkdir /opt/docker && chmod a=rwX /opt/docker

RUN curl -fL -o /tmp/firefox.tar.bz2 https://ftp.mozilla.org/pub/firefox/releases/${firefox_ver}/linux-x86_64/en-GB/firefox-${firefox_ver}.tar.bz2 \
&& tar -xjf /tmp/firefox.tar.bz2 -C /tmp/ && mv /tmp/firefox /opt/docker

RUN curl -fL -o /tmp/geckodriver.tar.gz \
https://github.com/mozilla/geckodriver/releases/download/v${geckodriver_ver}/geckodriver-v${geckodriver_ver}-linux64.tar.gz \
&& tar -xzf /tmp/geckodriver.tar.gz -C /tmp/ && chmod +x /tmp/geckodriver && mv /tmp/geckodriver /opt/docker

ENV PATH="/opt/docker:/opt/docker/firefox/${PATH}"
EXPOSE 4444
CMD [ "geckodriver", "--host", "0.0.0.0", "--binary" , "/opt/docker/firefox/firefox" ]
