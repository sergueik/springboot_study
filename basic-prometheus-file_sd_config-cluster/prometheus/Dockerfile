# NOTE: need pin version
FROM prom/prometheus:latest
USER root
WORKDIR /
COPY entrypoint.sh update_targets.sh prometheus.yml /
RUN sed -i 's/\r$//' entrypoint.sh update_targets.sh && \
    chmod +x entrypoint.sh update_targets.sh

RUN mkdir -p /etc/prometheus
# added dummy dynamic_targets.json
COPY dynamic_targets.json /etc/prometheus/
EXPOSE 9090
ENTRYPOINT ["./entrypoint.sh"]
