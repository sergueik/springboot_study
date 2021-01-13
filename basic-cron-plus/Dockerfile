FROM alpine:3.9.5 

# Copy script which should be run
COPY ./script.sh /usr/local/bin/script.sh
# Run the cron every minute
RUN echo '* * * * * /usr/local/bin/script.sh' > /etc/crontabs/root
CMD ["/usr/sbin/crond", "-f", "-l", "8"]
