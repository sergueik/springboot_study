FROM kibana:8.17.8
ADD config/kibana.yml /usr/share/kibana/config/kibana.yml
USER root
RUN apt-get update && apt-get install -qy jq wget
# RUN wget -O jq https://github.com/stedolan/jq/releases/download/jq-1.6/jq-linux64
# RUN  chmod +x jq && mv jq /usr/local/bin

USER kibana
