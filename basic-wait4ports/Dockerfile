FROM alpine:3.9.5

RUN apk add --no-cache wait4ports

# WRONG:
# will lead to the error:
# ENTRYPOINT ["wait4ports", "-s" ,"$TIMEOUT","${NAME}=tcp://${NODE}:${PORT}"]
# NOTE: one can dynamically set quiet/loud using shell syntax of ENTRYPOINT command
# ENTRYPOINT ["sh", "-c", "wait4ports ${QUIETFLAG} -s $TIMEOUT ${NAME}=tcp://${NODE}:${PORT}"]

ENTRYPOINT ["sh", "-c", "wait4ports -s $TIMEOUT ${NAME}=tcp://${NODE}:${PORT}"]
