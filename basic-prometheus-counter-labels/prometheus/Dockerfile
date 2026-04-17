FROM prom/prometheus:v3.5.0
USER root
WORKDIR /
RUN mkdir -p /etc/prometheus
COPY prometheus.yml /etc/prometheus/
EXPOSE 9090
CMD [ "--config.file=/etc/prometheus/prometheus.yml", "--web.enable-lifecycle", "--storage.tsdb.path=/prometheus"]
