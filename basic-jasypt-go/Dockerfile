FROM golang:alpine3.9
WORKDIR /jasypt
ENV GOOS=linux
ENV GOARCH=amd64
ENV CGO_ENABLED=0
COPY jasypt.go ./
RUN GOOS=$GOOS GOARCH=$GOARCH go build -ldflags "-s -w" -o jasypt jasypt.go
RUN chmod +x ./jasypt
#  TODO: fix the compiled binary problem
CMD ["./jasypt"]
# can use absolute paths or relative to wORKSPACE


