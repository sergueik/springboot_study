C:\\developer\\sergueik\\springboot\_study\\basic-emojimaker-javafx>java -cp target\\EmojiMaker-1.0-SNAPSHOT.jar com.mycompany.emojimaker.App


Error: A JNI error has occurred, please check your installation and try again

Exception in thread "main" java.lang.UnsupportedClassVersionError: com/mycompany/emojimaker/App has been compiled by a more recent version of the Java Runtime (class file version 55.0), this version of the Java Runtime only recognizes class file versions up to 52.0

&#x20;       at java.lang.ClassLoader.defineClass1(Native Me

[ERROR]   reason: '<>' with anonymous inner classes is not supported in -source 8
[ERROR]     (use -source 9 or higher to enable '<>' with anonymous inner classes)


java -cp target\com.mycompany.EmojiMaker.jar com.mycompany.emojimaker.App
Error: Could not find or load main class com.mycompany.emojimaker.App
Caused by: java.lang.NoClassDefFoundError: javafx/application/Application

C:\developer\sergueik\springboot_study\basic-emojimaker-javafx>unzip -ql target\com.mycompany.EmojiMaker.jar | grep com/mycompany/emojimaker
        0  2026-09-25 08:11   com/mycompany/emojimaker/
     2441  2026-09-25 08:11   com/mycompany/emojimaker/App.class
     4620  2026-09-25 08:11   com/mycompany/emojimaker/EmojiLienzo.fxml
    15982  2026-09-25 08:11   com/mycompany/emojimaker/EmojiLienzoController.class
     1859  2026-09-25 08:11   com/mycompany/emojimaker/galleryWindow.fxml
     4817  2026-09-25 08:11   com/mycompany/emojimaker/GalleryWindowController.class
     1196  2026-09-25 08:11   com/mycompany/emojimaker/welcomeWindow.fxml
     2620  2026-09-25 08:11   com/mycompany/emojimaker/WelcomeWindowController.class
