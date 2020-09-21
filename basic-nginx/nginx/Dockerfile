FROM nginx:alpine
# remove default configuration
RUN  rm -f /etc/nginx/conf.d/default.conf
COPY ./app.conf /etc/nginx/conf.d/
