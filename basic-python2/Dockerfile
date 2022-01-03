FROM python:3.8.2-alpine
ARG FILENAME_ARG
# NOTE: equal sign is optional in the following line
ENV FILENAME=$FILENAME_ARG
# NOTE: cannot use exec format
# need variable expansion
# /usr/local/bin/python: can't open file 'temp/${FILENAME}': [Errno 2] No such file or directory 
# CMD ["/usr/local/bin/python", "temp/${FILENAME}"]
CMD python temp/${FILENAME}
