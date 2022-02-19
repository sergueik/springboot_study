package example.bot.handler;


import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.stereotype.Component;
import org.telegram.telegrambots.meta.api.methods.send.SendMessage;
import org.telegram.telegrambots.meta.api.objects.Update;
import org.telegram.telegrambots.meta.api.objects.replykeyboard.ReplyKeyboardMarkup;
import org.telegram.telegrambots.meta.api.objects.replykeyboard.buttons.KeyboardRow;

import example.bot.builder.MessageBuilder;
import example.model.User;
import example.service.UserService;

import java.util.ArrayList;

@Component
@Slf4j
@ComponentScan(basePackages = "application.yaml")
public class StartHandler {

    private UserService userService;
    private ReplyKeyboardMarkup replyKeyboardMarkup = new ReplyKeyboardMarkup();

    @Autowired
    public StartHandler(UserService userService) {
        this.userService = userService;
    }

    public StartHandler() {
    }

    @Value("${bot.name}")
    private String botName;

    @Value("${bot.chat}")
    private String chatUrl;

    public SendMessage start(Update update) {

        replyKeyboardMarkup.setSelective(true);
        replyKeyboardMarkup.setResizeKeyboard(true);
        replyKeyboardMarkup.setOneTimeKeyboard(true);

        String userId = String.valueOf(update.getMessage().getFrom().getId());
        MessageBuilder messageBuilder = MessageBuilder.create(userId);
        messageBuilder
                .line()
                .line("Добро пожаловать в \uD83D\uDD30CapitalistGame\uD83D\uDD30\n" +
                        "\n" +
                        "В этой игре Вы можете раскрыть свой потенциал бизнесмена.\n" +
                        "\n" +
                        "Создайте свою компанию \uD83C\uDFED\n" +
                        "Покупайте фабрики ⛽️\uD83D\uDD0C\n" +
                        "Торгуйте  акциями на бирже \uD83D\uDCC8\n" +
                        "Производите ресурсы и торгуй ими \uD83D\uDCE6\n" +
                        "Развивайтесь и станьте лучшим\n" +
                        " \n" +
                        "Давайте не будем тянуть время, а сразу же перейдем к обучению! " +
                        "Нажмите \uD83D\uDDDE, чтобы перейти к обучению или \uD83D\uDCF0, чтобы пропустить его");
        createStartMenu();

        SendMessage sendMessage = messageBuilder.build();
        sendMessage.setReplyMarkup(replyKeyboardMarkup);
        return sendMessage;
    }

    public SendMessage callbackStart(Update update) {

        replyKeyboardMarkup.setSelective(true);
        replyKeyboardMarkup.setResizeKeyboard(true);
        replyKeyboardMarkup.setOneTimeKeyboard(true);

        String userId = String.valueOf(update.getCallbackQuery().getFrom().getId());
        MessageBuilder messageBuilder = MessageBuilder.create(userId);
        messageBuilder
                .line()
                .line("Добро пожаловать в \uD83D\uDD30CapitalistGame\uD83D\uDD30\n" +
                        "\n" +
                        "В этой игре Вы можете раскрыть свой потенциал бизнесмена.\n" +
                        "\n" +
                        "Создайте свою компанию \uD83C\uDFED\n" +
                        "Покупайте фабрики ⛽️\uD83D\uDD0C\n" +
                        "Торгуйте  акциями на бирже \uD83D\uDCC8\n" +
                        "Производите ресурсы и торгуй ими \uD83D\uDCE6\n" +
                        "Развивайтесь и станьте лучшим\n" +
                        " \n" +
                        "Давайте не будем тянуть время, а сразу же перейдем к обучению! " +
                        "Нажмите \uD83D\uDDDE, чтобы перейти к обучению или \uD83D\uDCF0, чтобы пропустить его");
        createStartMenu();

        SendMessage sendMessage = messageBuilder.build();
        sendMessage.setReplyMarkup(replyKeyboardMarkup);
        return sendMessage;
    }

    public void createStartMenu() {
        ArrayList<KeyboardRow> rows = new ArrayList<>();
        KeyboardRow keyboardRow = new KeyboardRow();
        keyboardRow.add("\uD83D\uDDDE");
        KeyboardRow keyboardRow1 = new KeyboardRow();
        keyboardRow1.add("\uD83D\uDCF0");

        rows.add(keyboardRow);
        rows.add(keyboardRow1);

        replyKeyboardMarkup.setKeyboard(rows);

    }

    public void createStartMenuTwo() {
        ArrayList<KeyboardRow> rows = new ArrayList<>();
        KeyboardRow keyboardRow = new KeyboardRow();
        keyboardRow.add("\uD83D\uDCC3");

        rows.add(keyboardRow);
        replyKeyboardMarkup.setKeyboard(rows);
    }

    public SendMessage stepTwo(Update update) {
        replyKeyboardMarkup.setSelective(true);
        replyKeyboardMarkup.setResizeKeyboard(true);
        replyKeyboardMarkup.setOneTimeKeyboard(true);

        int userId = update.getMessage().getFrom().getId();
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));
        messageBuilder
                .line()
                .line("Начнем❗️\n" +
                        "\n" +
                        "От маленькой компании до огромной монополии!!!\n" +
                        "Вас ждет бизнес, торговля и обмен ресурсами, покупка и продажа акций \uD83D\uDCC8 \n" +
                        " \n" +
                        "На данный момент в игре существует два типа фабрик:\n" +
                        " 1. ⛽️ Нефтяные насосы.\n" +
                        " 2. \uD83D\uDD0C Электростанции.\n" +
                        " \n" +
                        "Нажимайте \uD83D\uDCC3, чтобы узнать больше\n");
        createStartMenuTwo();

        User user = userService.getOrCreate(userId);
        user.setPositions("train_1");
        userService.update(user);
        SendMessage sendMessage = messageBuilder.build();
        sendMessage.setReplyMarkup(replyKeyboardMarkup);
        return sendMessage;
    }

    public SendMessage stepThree(Update update) {
        replyKeyboardMarkup.setSelective(true);
        replyKeyboardMarkup.setResizeKeyboard(true);
        replyKeyboardMarkup.setOneTimeKeyboard(true);

        int userId = update.getMessage().getFrom().getId();
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));
        messageBuilder
                .line()
                .line("⛽️ Нефтяные насосы производят \uD83D\uDEE2 баррели нефти, которые можно продать за \uD83C\uDF11 OilCoin и \uD83D\uDCB0 Gold. \n" +
                        "\uD83D\uDD0C Электростанции производят \uD83D\uDD0B киловатты энергии, которые можно продать за \uD83C\uDF15ECoin и ⚡️ ECrypt. \n" +
                        "\n" +
                        "За \uD83C\uDF11 OilCoin Вы можете покупать ⛽️ нефтяные насосы, а за \uD83C\uDF15 ECoin – \uD83D\uDD0C электростанции.\n" +
                        "\n" +
                        "Тогда как Gold и ECrypt можно выводить как реальные деньги.\n" +
                        "\n" +
                        "На данный момент курс обмена такой: \n" +
                        "100 Gold = 60 руб.\n" +
                        "100 ECrypt = 300 руб. \n" +
                        "\n" +
                        "Если Вы готовы двигаться дальше, жмите \uD83D\uDCC4. Осталось совсем чуток!");

        createStartMenuThree();

        User user = userService.getOrCreate(userId);
        user.setPositions("train_2");
        userService.update(user);

        SendMessage sendMessage = messageBuilder.build();
        sendMessage.setReplyMarkup(replyKeyboardMarkup);

        return sendMessage;
    }

    public void createStartMenuThree() {
        ArrayList<KeyboardRow> rows = new ArrayList<>();
        KeyboardRow keyboardRow = new KeyboardRow();
        keyboardRow.add("\uD83D\uDCC4");

        rows.add(keyboardRow);
        replyKeyboardMarkup.setKeyboard(rows);
    }

    public SendMessage stepFour(Update update) {
        replyKeyboardMarkup.setSelective(true);
        replyKeyboardMarkup.setResizeKeyboard(true);
        replyKeyboardMarkup.setOneTimeKeyboard(true);

        int userId = update.getMessage().getFrom().getId();
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));
        messageBuilder
                .line()
                .line("Все фабрики разделены по уровням: от 1️⃣ до 6️⃣. Чем выше уровень, тем больше ресурсов фабрика может добывать, однако, вместе с количеством добываемых ресурсов растет и цена фабрики.\n" +
                        "  \n" +
                        "Например, ⛽️ нефтяной насос 1 уровня стоит 100 \uD83C\uDF11 OilCoin и добывает 16 \uD83D\uDEE2 баррелей нефти в час, тогда как нефтяной насос 6 уровня стоит 90000 \uD83C\uDF11 OilCoin и добывает 31250 \uD83D\uDEE2 баррелей в час. \n" +
                        "\n" +
                        "Чувствуешь разницу? Да она просто огромна! Как раз для тех, кто хочет развиваться быстро, мы создали \uD83C\uDFE6 Банк. Там Вы можете купить OilCoin \uD83C\uDF11 и другую валюту для покупки фабрик, используя реальные деньги. Не забывайте, что все вложенные средства обязательно возвратятся в виде валюты \uD83D\uDCB0⚡️, которую можно выводить как реальные деньги $\n" +
                        "\n" +
                        "Нажимай \uD83D\uDDD2, чтобы узнать больше!");

        createStartMenuFour();

        User user = userService.getOrCreate(userId);
        user.setPositions("train_3");
        userService.update(user);

        SendMessage sendMessage = messageBuilder.build();
        sendMessage.setReplyMarkup(replyKeyboardMarkup);
        return sendMessage;
    }

    public SendMessage stepFive(Update update) {
        replyKeyboardMarkup.setSelective(true);
        replyKeyboardMarkup.setResizeKeyboard(true);
        replyKeyboardMarkup.setOneTimeKeyboard(true);

        int userId = update.getMessage().getFrom().getId();
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));
        messageBuilder
                .line()
                .line("Пополнение счета далеко не единственный способ быстро получить внутриигровую валюту. У нас также реализована умная реферальная система!\n" +
                        "  \n" +
                        "За каждого приглашенного \uD83D\uDC65 друга Вы будете единовременно получать 40 \uD83C\uDF11 OilCoin и 20 \uD83C\uDF15" +
                        " ECoin просто так! Также Вы будете получать % от пополнения его счета и % добываемых им ресурсов.\n" +
                        "Вдобавок к этому, Вы сможете получать % от пополнения \uD83D\uDC65 друзьями Вашего реферала. \n" +
                        "\n" +
                        "Но об этом чуть позже.\n" +
                        "\n" +
                        "Нажимайте \uD83C\uDFC1, чтобы завершить теорию и приступить к практике!");
        createStartMenuFive();

        User user = userService.getOrCreate(userId);
        user.setPositions("train_4");
        userService.update(user);

        SendMessage sendMessage = messageBuilder.build();
        sendMessage.setReplyMarkup(replyKeyboardMarkup);
        return sendMessage;
    }

    public void createStartMenuFour(){
        ArrayList<KeyboardRow> rows = new ArrayList<>();
        KeyboardRow keyboardRow = new KeyboardRow();
        keyboardRow.add("\uD83D\uDDD2");

        rows.add(keyboardRow);
        replyKeyboardMarkup.setKeyboard(rows);
    }

    public void createStartMenuFive(){
        ArrayList<KeyboardRow> rows = new ArrayList<>();
        KeyboardRow keyboardRow = new KeyboardRow();
        keyboardRow.add("\uD83C\uDFC1");
        replyKeyboardMarkup.setResizeKeyboard(true);

        rows.add(keyboardRow);
        replyKeyboardMarkup.setKeyboard(rows);
    }

    public SendMessage stepSix(Update update) {
        int userId = update.getMessage().getFrom().getId();
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));
        messageBuilder
                .line()
                .line("Вижу, что Вы устали читать столько текста. Давайте приступим к самой игре! Внизу Вы видите меню с кнопками. Нажмите на кнопку " +
                        "\uD83C\uDFED Моя компания и потом откройте \uD83D\uDCDC  Задания, чтобы узнать текущее задание и перейти к его выполнению!");

        User user = userService.getOrCreate(userId);
        user.setPositions("back");
        userService.update(user);

        return messageBuilder.build();
    }

    public void createStartMenuSix(){
        ArrayList<KeyboardRow> rows = new ArrayList<>();
        KeyboardRow keyboardRow = new KeyboardRow();
        replyKeyboardMarkup.setResizeKeyboard(true);

        keyboardRow.add("\uD83C\uDFED Моя компания");
        keyboardRow.add("\uD83C\uDFEB Биржа");

        KeyboardRow keyboardRow1 = new KeyboardRow();
        keyboardRow1.add("\uD83D\uDED2 Рынок");
        keyboardRow1.add("\uD83C\uDFE6 Банк");

        KeyboardRow keyboardRow2 = new KeyboardRow();
        keyboardRow2.add("\uD83C\uDF81 Ежедневный бонус");
        keyboardRow2.add("\uD83D\uDCA1 Дополнительно");

        rows.add(keyboardRow);
        rows.add(keyboardRow1);
        rows.add(keyboardRow2);
        replyKeyboardMarkup.setKeyboard(rows);
    }


    public SendMessage endTrain(Update update){
        replyKeyboardMarkup.setSelective(true);
        replyKeyboardMarkup.setResizeKeyboard(true);
        replyKeyboardMarkup.setOneTimeKeyboard(true);

        int userId = update.getMessage().getFrom().getId();
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));
        messageBuilder
                .line()
                .line("\uD83D\uDCDC Доска объявлений")
                .row()
                .button("\uD83D\uDCAC Игровой ЧАТ", "/chat")
                .row()
                .button("Акционер \uD83E\uDD1D Реферал", "/action")
                .row()
                .button("\uD83D\uDCC6 РЕКЛАМА В БОТЕ", "/advert");

        return messageBuilder.build();
    }

    public SendMessage endTrainMenu(Update update){
        replyKeyboardMarkup.setSelective(true);
        replyKeyboardMarkup.setResizeKeyboard(true);
        replyKeyboardMarkup.setOneTimeKeyboard(false);

        int userId = update.getMessage().getFrom().getId();
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));
        messageBuilder
                .line()
                .line("\uD83D\uDCC3 Главное меню");

        createStartMenuSix();

        SendMessage sendMessage = messageBuilder.build();
        sendMessage.setReplyMarkup(replyKeyboardMarkup);

        return sendMessage;
    }
}