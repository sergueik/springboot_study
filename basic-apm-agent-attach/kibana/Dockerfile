FROM kibana:7.17.7
ADD config/kibana.yml /usr/share/kibana/config/kibana.yml
USER root
RUN apt-get update && apt-get install -qy jq
USER kibana
