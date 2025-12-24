FROM alpine:3.9.5 

ADD crontab.txt /crontab.txt
ADD ./script.sh /usr/local/bin/script.sh
COPY entry.sh /entry.sh
RUN sed -i 's|\r||g' entry.sh /usr/local/bin/script.sh /crontab.txt \
    && chmod 755 /usr/local/bin/script.sh /entry.sh
RUN /usr/bin/crontab /crontab.txt

CMD ["/entry.sh"]
