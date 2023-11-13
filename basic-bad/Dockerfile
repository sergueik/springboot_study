FROM alpine:3.9.5

WORKDIR /code

COPY entrypoint.sh ./
RUN sed -i 's|\r||g' ./entrypoint.sh && chmod +x ./entrypoint.sh 

# ENTRYPOINT /code/entrypoint.sh
ENTRYPOINT [ "./entrypoint.sh" ]

