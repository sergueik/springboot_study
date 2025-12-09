#### Info



replica of  the [native-messaging](https://github.com/vakho10/Native-Messaging)
[Native Messaging](https://developer.chrome.com/docs/extensions/develop/concepts/native-messaging)


Project structure:
  - App - contains chrome's extension for native messaging example.
  - App_Native - contains native application (java project).
  - Host - native messaging host (+ registry file).
  - Web - contains a simple website which send and receives message from native app.


### About Native Messaging
  - https://developer.chrome.com/docs/extensions/develop/concepts/native-messaging
### Integration Testing with Powershell Script

Observed the OOM that is not due to the java host nor payload itself, but to how PowerShell 5 reads the stream. If the stream returned 0 or fewer than 4 bytes (race / timing issue), `$respLenBytes` stays partially filled. Then `[BitConverter]::ToInt32` interprets the garbage bytes as `1314013527`.

```text
DEBUG: Response (read safely) length = 1314013527
Exception: Response too big (1314013527 bytes)
```

```text
Exception: Failed to read response length. Only read 0 bytes.
```

```text
Exception calling "GetString" with "1" argument(s): "Exception of type 'System.OutOfMemoryException' was thrown."
```

while in the java log everything looks normal:
```text
[2025-12-08 23:13:01] INFO  ge.vakho.native_messaging.main.Main - Length prefix bytes: 1E 00 00 00
[2025-12-08 23:13:01] INFO  ge.vakho.native_messaging.main.Main - Payload bytes: 7B 22 6D 65 73 73 61 67 65 22 3A 22 48 65 6C 6C 6F 2C 20 73 74 72 61 6E 67 65 72 21 22 7D
[2025-12-08 23:13:01] INFO  ge.vakho.native_messaging.main.Main - Response JSON {"message":"Hello, stranger!"}
```


### See Also:
  * https://habr.com/ru/articles/974124
  * Similar projects (need to be ported backwards to .Net Framework, which is likely trivial)
  - https://github.com/acandylevey/NativeMessaging
  - https://github.com/ba32107/dotnet-chrome-native-messaging
  - https://github.com/alexwiese/Lyre

---

### Author

[Serguei Kouzmine](mailto:kouzmine_serguei@yahoo.com)

