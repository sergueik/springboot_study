FROM alpine:3.9.5 

# save the intended schedule in file
ADD crontab.txt /crontab.txt
# copy appplication script that scheduled to run
ADD ./script.sh /usr/local/bin/script.sh
# Generate enrrypoint script
COPY entry.sh /entry.sh
RUN chmod 755 /usr/local/bin/script.sh /entry.sh
RUN /usr/bin/crontab /crontab.txt

CMD ["/entry.sh"]
