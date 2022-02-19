### Info

This directory contains a minimally modified [project](https://github.com/mcasperson/WelcomeBot) from codeproject 
[article](https://www.codeproject.com/Articles/5324026/Creating-a-Teams-Conversation-Bot-with-SSO)

on using MS recommended tools to develop Java based cloud hosted apps.

but without cloud deployment part, only interested in [bot emulator](https://github.com/microsoft/botframework-emulator)

```cmd
mvn clean spring-boot:run
```
```cmd
"%USERPROFILE%\AppData\Local\Programs\Bot Framework Emulator\Bot Framework Emulator.exe"
```

### NOTE

https://github.com/microsoft/BotFramework-Emulator/releases is a bloated electron app -
use e.g. release [v4.13.0](https://github.com/microsoft/BotFramework-Emulator/releases/download/v4.13.0/BotFramework-Emulator-4.13.0-windows-setup.exe) and uninstall afterwards

### See Also
   
   * part2 [article]([article](https://www.codeproject.com/Articles/5324101/Creating-a-File-Upload-Bot) ) article 

   * https://en.wikipedia.org/wiki/Yeoman_(software) - about skeleton project code generator

   *  https://www.baeldung.com/spring-http-logging