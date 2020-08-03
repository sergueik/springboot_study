FROM openjdk:8-jre-alpine3.9
ARG apps_dir="/opt/apps"
ARG project="springboot_study"
EXPOSE 8085
RUN mkdir ${apps_dir}
RUN git --version &>/dev/null; if [ $? -ne 0 ] ; then \
apk update && apk upgrade && \
apk add --no-cache git ; fi
# execute command which will lead to rebuilding 
RUN if [ -d ${apps_dir}/${project}.git ] ; then echo $(git --git-dir= ${apps_dir}/${project}.git rev-parse --short HEAD); else echo 'No workspace' ;fi

RUN cd ${apps_dir} && git clone https://github.com/sergueik/${project}
CMD tail -f /dev/null

#
# entrypoint etc.
