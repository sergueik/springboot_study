FROM alpine:latest AS jsonnet-buider

RUN apk -U add build-base

WORKDIR /opt

COPY jsonnet /opt/jsonnet

RUN cd jsonnet && \
    make

# FROM alpine:3.9.5
FROM alpine:latest
# TODO: change jsonnet-builder to 3.9.5 too
# Error relocating /usr/local/bin/jsonnet: _ZSt28__throw_bad_array_new_lengthv: symbol not found
# Error relocating /usr/local/bin/jsonnet: _ZNSt7__cxx1118basic_stringstreamIcSt11char_traitsIcESaIcEEC1Ev: symbol not found
#
RUN apk add --no-cache libstdc++ 

COPY --from=jsonnet-buider /opt/jsonnet/jsonnet /usr/local/bin
COPY --from=jsonnet-buider /opt/jsonnet/jsonnetfmt /usr/local/bin
COPY grafonnet-lib opt

ENTRYPOINT ["/usr/local/bin/jsonnet", "-J", "/opt/"]
