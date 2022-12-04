FROM mcr.microsoft.com/dotnet/aspnet:6.0
WORKDIR /app/bin
COPY --from=build /app/bin /app/bin

ENTRYPOINT ["dotnet", "Api.dll"]
