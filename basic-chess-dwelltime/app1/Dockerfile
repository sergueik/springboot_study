FROM nginx:1.16.1-alpine


# Replace default nginx static content with our single page
RUN rm -rf /usr/share/nginx/html/*

COPY html/page.html /usr/share/nginx/html/index.html

EXPOSE 80

CMD ["nginx", "-g", "daemon off;"]
