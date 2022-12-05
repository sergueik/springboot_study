FROM mcr.microsoft.com/dotnet/aspnet:6.0
WORKDIR /app
# NOTE: apparenly cannot pass the contaier name as a build arg.
# ARG BUILD_CONTAINER=basic-aspnetapp-serilog-build
# COPY --from=${BUILD_CONTAINER} /app ./

COPY --from=basic-aspnetapp-serilog-build /app .

ENTRYPOINT ["./AspNetCoreSerilogExample.Web"]
