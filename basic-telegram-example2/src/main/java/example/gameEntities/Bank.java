package example.gameEntities;

import lombok.RequiredArgsConstructor;
import lombok.SneakyThrows;
import org.apache.commons.codec.digest.DigestUtils;
import org.apache.commons.math3.util.Precision;
import org.springframework.stereotype.Component;
import org.telegram.telegrambots.meta.api.methods.send.SendMessage;
import org.telegram.telegrambots.meta.api.objects.Update;
import org.telegram.telegrambots.meta.api.objects.replykeyboard.ReplyKeyboardMarkup;
import org.telegram.telegrambots.meta.api.objects.replykeyboard.buttons.KeyboardRow;

import example.bot.builder.MessageBuilder;
import example.model.Payment;
import example.model.User;
import example.service.PaymentService;
import example.service.UserService;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;

@Component
@RequiredArgsConstructor
public class Bank {

    private final UserService userService;
    private final PaymentService paymentService;
    private final ReplyKeyboardMarkup keyboard = new ReplyKeyboardMarkup();

    int merchantId = 0; // id магазина
    String type = ""; // тип валюты

    public SendMessage pay(Update update) {
        int userId = update.getMessage().getFrom().getId();
        User user = userService.getOrCreate(userId);

        int gold = user.getGold();
        int oilCoin = user.getOilCoin();
        int eCoin = user.getECoin();
        int eCrypt = user.getECrypt();
        double ballsOne = Precision.round(user.getBallsOne(), 4);
        double ballsTwo = Precision.round(user.getBallsTwo(), 4);

        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));
        messageBuilder
                .line()
                .line("\uD83C\uDFE6 Банк\n" +
                        " \n" +
                        "Добро пожаловать в банк!\n" +
                        "Здесь Вы можете купить, обменять и вывести валюту.\n" +
                        "\n" +
                        "На вашем счету:\n" +
                        oilCoin + " \uD83C\uDF11 OilCoin\n" +
                        eCoin + " \uD83C\uDF15 ECoin\n" +
                        gold + " \uD83D\uDCB0 Gold\n" +
                        eCrypt + " ⚡️ ECrypt\n" +
                        ballsOne + " \uD83D\uDD36 Баллы\n" +
                        ballsTwo + " \uD83D\uDD37 Баллы")
                .row()
                .button("\uD83D\uDCB8 Пополнение баланса(\uD83D\uDD36 \uD83D\uDD37 \uD83C\uDF15 \uD83C\uDF11)", "/payment")
                .row()
                .button("\uD83D\uDCB1 Обмен валют", "/changeMoney")
                .row()
                .button("\uD83D\uDCE4 Вывод баланса", "/output");

        return messageBuilder.build();
    }

    public SendMessage payment(Update update) {
        int userId = update.getCallbackQuery().getFrom().getId();
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));

        messageBuilder
                .line("\uD83D\uDCB8 Пополнение баланса\n" +
                        "\n" +
                        "Здесь Вы можете купить OilCoin и ECoin за рубли, доллары и биткоины.\n" +
                        "⏱ Срок зачисления средств может занимать до 12-ти часов.")
                .row()
                .button("Купить \uD83C\uDF11 OilCoin и \uD83D\uDD37", "/buy_oilCoin")
                .row()
                .button("Купить \uD83C\uDF15 ECoin \uD83D\uDD36", "/buy_eCoin");
        return messageBuilder.build();
    }

    public SendMessage paymentOilCoin(Update update) {
        int userId = update.getCallbackQuery().getFrom().getId();
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));
        type = "oilcoin";

        messageBuilder
                .line("\uD83D\uDCB8 Покупка \uD83C\uDF11 OilCoin\n" +
                        "\n" +
                        "Курс покупки:\n" +
                        "60 руб. = 100 \uD83C\uDF11 OilCoin\n" +
                        "За каждые 200 купленных \uD83C\uDF11OilCoin, Вы получите 1 \uD83D\uDD37 балл.\n" +
                        "\n" +
                        "\n" +
                        "2️⃣ Выберите нужное количество \uD83C\uDF11OilCoin, которое вы хотите приобрести:")
                .line()
                .row()
                .buttonWithUrl("100 \uD83C\uDF11 OilCoin", "/buy_oilCoin100", "https://www.free-kassa.ru/merchant/cash.php?m="+merchantId+
                        "&oa="+100+"&o="+userId+"&s="+getMd5(100, userId)+"&lang=ru&us_type="+type+"&type=json")
                .row()
                .buttonWithUrl("200 \uD83C\uDF11 OilCoin", "/buy_oilCoin200", "https://www.free-kassa.ru/merchant/cash.php?m="+merchantId+
                        "&oa="+200+"&o="+userId+"&s="+getMd5(200, userId)+"&lang=ru&us_type="+type+"&type=json")
                .row()
                .buttonWithUrl("400 \uD83C\uDF11 OilCoin", "/buy_oilCoin400", "https://www.free-kassa.ru/merchant/cash.php?m="+merchantId+
                        "&oa="+400+"&o="+userId+"&s="+getMd5(400, userId)+"&lang=ru&us_type="+type+"&type=json")
                .row()
                .buttonWithUrl("800 \uD83C\uDF11 OilCoin", "/buy_oilCoin800", "https://www.free-kassa.ru/merchant/cash.php?m="+merchantId+
                        "&oa="+800+"&o="+userId+"&s="+getMd5(800, userId)+"&lang=ru&us_type="+type+"&type=json")
                .row()
                .buttonWithUrl("1600 \uD83C\uDF11 OilCoin", "/buy_oilCoin1600", "https://www.free-kassa.ru/merchant/cash.php?m="+merchantId+
                        "&oa="+1600+"&o="+userId+"&s="+getMd5(1600, userId)+"&lang=ru&us_type="+type+"&type=json")
                .row()
                .buttonWithUrl("3200 \uD83C\uDF11 OilCoin", "/buy_oilCoin3200", "https://www.free-kassa.ru/merchant/cash.php?m="+merchantId+
                        "&oa="+3200+"&o="+userId+"&s="+getMd5(3200, userId)+"&lang=ru&us_type="+type+"&type=json")
                .row()
                .buttonWithUrl("6400 \uD83C\uDF11 OilCoin", "/buy_oilCoin6400", "https://www.free-kassa.ru/merchant/cash.php?m="+merchantId+
                        "&oa="+6400+"&o="+userId+"&s="+getMd5(6400, userId)+"&lang=ru&us_type="+type+"&type=json");

        return messageBuilder.build();
    }

    public SendMessage paymentECoin(Update update) {
        int userId = update.getCallbackQuery().getFrom().getId();
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));

        type = "ecoin";

        messageBuilder
                .line("\uD83D\uDCB8 Покупка \uD83C\uDF15 ECoin\n" +
                        "\n" +
                        "Курс покупки:\n" +
                        "300 руб. = 100 \uD83C\uDF15 ECoin\n" +
                        "За каждые 200 купленных \uD83C\uDF15 ECoin, Вы получите 1 \uD83D\uDD36 балл.\n" +
                        "\n" +
                        "\n" +
                        "2️⃣ Выберите нужное количество \uD83C\uDF15 ECoin, которое вы хотите приобрести:")
                .line()
                .row()
                .buttonWithUrl("100 \uD83C\uDF15 ECoin", "/buy_eCoin100", "https://www.free-kassa.ru/merchant/cash.php?m="+merchantId+
                        "&oa="+100+"&o="+userId+"&s="+getMd5(100, userId)+"&lang=ru&us_type="+type+"&type=json")
                .row()
                .buttonWithUrl("200 \uD83C\uDF15 ECoin", "/buy_eCoin200", "https://www.free-kassa.ru/merchant/cash.php?m="+merchantId+
                        "&oa="+200+"&o="+userId+"&s="+getMd5(200, userId)+"&lang=ru&us_type="+type+"&type=json")
                .row()
                .buttonWithUrl("400 \uD83C\uDF15 ECoin", "/buy_eCoin400", "https://www.free-kassa.ru/merchant/cash.php?m="+merchantId+
                        "&oa="+400+"&o="+userId+"&s="+getMd5(400, userId)+"&lang=ru&us_type="+type+"&type=json")
                .row()
                .buttonWithUrl("800 \uD83C\uDF15 ECoin", "/buy_eCoin800", "https://www.free-kassa.ru/merchant/cash.php?m="+merchantId+
                        "&oa="+800+"&o="+userId+"&s="+getMd5(800, userId)+"&lang=ru&us_type="+type+"&type=json")
                .row()
                .buttonWithUrl("1600 \uD83C\uDF15 ECoin", "/buy_eCoin1600", "https://www.free-kassa.ru/merchant/cash.php?m="+merchantId+
                        "&oa="+1600+"&o="+userId+"&s="+getMd5(1600, userId)+"&lang=ru&us_type="+type+"&type=json")
                .row()
                .buttonWithUrl("3200 \uD83C\uDF15 ECoin", "/buy_eCoin3200", "https://www.free-kassa.ru/merchant/cash.php?m="+merchantId+
                        "&oa="+3200+"&o="+userId+"&s="+getMd5(3200, userId)+"&lang=ru&us_type="+type+"&type=json")
                .row()
                .buttonWithUrl("6400 \uD83C\uDF15 ECoin", "/buy_eCoin6400", "https://www.free-kassa.ru/merchant/cash.php?m="+merchantId+
                        "&oa="+6400+"&o="+userId+"&s="+getMd5(6400, userId)+"&lang=ru&us_type="+type+"&type=json");

        return messageBuilder.build();
    }

    // обработка подписи freeKassa
    @SneakyThrows
    private String getMd5(int sum, int userId){
        String secretWord = "";
        String label = merchantId + ":" + sum +":" + secretWord + ":" + userId;
        return DigestUtils.md5Hex(label);
    }

    public SendMessage change(Update update) {
        int userId = update.getCallbackQuery().getFrom().getId();
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));

        messageBuilder
                .line("\uD83D\uDCB1 Обмен валют\n" +
                        "\n" +
                        "Выберете валюту для обмена.\n" +
                        "Курс обмена:\n" +
                        "1 ⚡️ ECrypt ➡️ 1 \uD83C\uDF15 ECoin\n" +
                        "1 \uD83D\uDCB0 Gold ➡️ 1 \uD83C\uDF11 OilCoin\n" +
                        "1 \uD83D\uDD36 балл ➡️ 2 \uD83D\uDD37 балла")
                .row()
                .button("Обменять ⚡️ECrypt", "/change_ecrypt")
                .row()
                .button("Обменять \uD83D\uDCB0 Gold", "/change_gold")
                .row()
                .button("Обменять \uD83D\uDD36 баллы", "/change_balls");

        return messageBuilder.build();
    }

    public SendMessage changeEcrypt(Update update) {
        int userId = update.getCallbackQuery().getFrom().getId();
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));
        User user = userService.getOrCreate(userId);

        createCancel();
        messageBuilder
                .line("\uD83C\uDFE6 Банк\n" +
                        "\n" +
                        "Введите количество ⚡️ ECrypt для обмена на \uD83C\uDF15 ECoin.\n" +
                        "Курс обмена 1 к 1.\n" +
                        "Баланс: 0 ECrypt.");

        user.setPositions("change_ecrypt");
        userService.update(user);

        SendMessage sendMessage = messageBuilder.build();
        sendMessage.setReplyMarkup(keyboard);

        return sendMessage;
    }
    // вызов при позиции change_ecrypt
    public SendMessage changeEcryptImpl(Update update) {
        int userId = update.getMessage().getFrom().getId();
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));

        int quantity = 0;
        try{
            quantity = Integer.parseInt(update.getMessage().getText());
        }catch (Exception e){
            return messageBuilder
                    .line("Вы ввели не число")
                    .build();
        }

        User user = userService.getOrCreate(userId);
        int eCrypt = user.getECrypt();
        createCancel();
        if (eCrypt < quantity || eCrypt < 1){
            messageBuilder
                    .line("У вас недостаточно ECrypt или вы ввели кол-во меньше 1\n")
                    .line("Введите другое число");
            SendMessage sendMessage = messageBuilder.build();
            sendMessage.setReplyMarkup(keyboard);
            return sendMessage;
        } else {
            int eCoin = user.getECoin();

            eCrypt -= quantity;
            eCoin += quantity;
            user.setECoin(eCoin);
            user.setECrypt(eCrypt);
            userService.update(user);

            createCancel();
            messageBuilder
                    .line("\uD83C\uDFE6 Банк\n" +
                            "\n" +
                            "Обмен прошел успешно \n" +
                            "Ваш баланс:\n" +
                            ""+ eCrypt +" ECrypt.\n" +
                            ""+ eCoin+" ECoin.");

            user.setPositions("back");
            userService.update(user);

            SendMessage sendMessage = messageBuilder.build();
            sendMessage.setReplyMarkup(keyboard);

            return sendMessage;
        }
    }

    public SendMessage changeGold(Update update){
        int userId = update.getCallbackQuery().getFrom().getId();
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));
        User user = userService.getOrCreate(userId);

        createCancel();
        messageBuilder
                .line("\uD83C\uDFE6 Банк\n" +
                        "\n" +
                        "Введите количество \uD83D\uDCB0 Gold для обмена на \uD83C\uDF11 OilCoin.\n" +
                        "Курс обмена 1 к 1.\n" +
                        "Баланс: 0 Gold.");

        user.setPositions("change_gold");
        userService.update(user);

        SendMessage sendMessage = messageBuilder.build();
        sendMessage.setReplyMarkup(keyboard);

        return sendMessage;
    }
    // вызов при позиции change_gold
    public SendMessage changeGoldImpl(Update update) {
        int userId = update.getMessage().getFrom().getId();
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));

        int quantity = 0;
        try{
            quantity = Integer.parseInt(update.getMessage().getText());
        }catch (Exception e){
            return messageBuilder
                    .line("Вы ввели не число")
                    .build();
        }

        User user = userService.getOrCreate(userId);
        int gold = user.getGold();
        createCancel();
        if (gold < quantity || gold < 1){
            messageBuilder
                    .line("У вас недостаточно \uD83D\uDCB0 Gold или вы ввели кол-во меньше 1\n")
                    .line("Введите другое число");
            SendMessage sendMessage = messageBuilder.build();
            sendMessage.setReplyMarkup(keyboard);
            return sendMessage;
        } else {
            int oilCoin = user.getOilCoin();

            gold -= quantity;
            oilCoin += quantity;
            user.setOilCoin(oilCoin);
            user.setGold(gold);
            userService.update(user);

            createCancel();
            messageBuilder
                    .line("\uD83C\uDFE6 Банк\n" +
                            "\n" +
                            "Обмен прошел успешно \n" +
                            "Ваш баланс:\n" +
                            ""+ gold +" Gold.\n" +
                            ""+ oilCoin+" OilCoin.");

            user.setPositions("back");
            userService.update(user);

            SendMessage sendMessage = messageBuilder.build();
            sendMessage.setReplyMarkup(keyboard);

            return sendMessage;
        }
    }

    public SendMessage changeBalls(Update update){
        int userId = update.getCallbackQuery().getFrom().getId();
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));
        User user = userService.getOrCreate(userId);

        createCancel();
        messageBuilder
                .line("\uD83C\uDFE6 Банк\n" +
                        "\n" +
                        "Введите количество \uD83D\uDD36 баллов для обмена на \uD83D\uDD37 баллы.\n" +
                        "Курс обмена 1 к 2.\n" +
                        "Баланс: 0 \uD83D\uDD36 баллов.");

        user.setPositions("change_balls");
        userService.update(user);

        SendMessage sendMessage = messageBuilder.build();
        sendMessage.setReplyMarkup(keyboard);

        return sendMessage;
    }
    // вызов при позиции change_balls
    public SendMessage changeBallsImpl(Update update) {
        int userId = update.getMessage().getFrom().getId();
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));

        int quantity = 0;
        try{
            quantity = Integer.parseInt(update.getMessage().getText());
        }catch (Exception e){
            return messageBuilder
                    .line("Вы ввели не число")
                    .build();
        }

        User user = userService.getOrCreate(userId);
        double ball1 = user.getBallsOne();
        createCancel();
        if (ball1 < quantity || ball1 < 1){
            messageBuilder
                    .line("У вас недостаточно \uD83D\uDD36 баллов или вы ввели кол-во меньше 1\n")
                    .line("Введите другое число");
            SendMessage sendMessage = messageBuilder.build();
            sendMessage.setReplyMarkup(keyboard);
            return sendMessage;
        } else {
            double ball2 = user.getBallsTwo();

            ball1 -= quantity;
            ball2 += quantity*2;
            user.setBallsTwo(ball2);
            user.setBallsOne(ball1);
            userService.update(user);

            createCancel();
            messageBuilder
                    .line("\uD83C\uDFE6 Банк\n" +
                            "\n" +
                            "Обмен прошел успешно \n" +
                            "Ваш баланс:\n" +
                            ""+ ball1 +" \uD83D\uDD36 баллов.\n" +
                            ""+ ball2+" \uD83D\uDD37 баллов.");

            user.setPositions("back");
            userService.update(user);

            SendMessage sendMessage = messageBuilder.build();
            sendMessage.setReplyMarkup(keyboard);

            return sendMessage;
        }
    }

    public SendMessage withdraw(Update update){
        int userId = update.getCallbackQuery().getFrom().getId();
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));
        User user = userService.getOrCreate(userId);

        int eCrypt = user.getECrypt();
        int gold = user.getGold();
        messageBuilder
                .line("\uD83D\uDCE4 Вывод баланса\n" +
                        "\n" +
                        "\uD83C\uDFE6 Баланс:\n" +
                        gold+ " \uD83D\uDCB0 Gold\n" +
                        eCrypt +" ⚡️ ECrypt\n" +
                        "\n" +
                        "\uD83D\uDD12 Минимум для вывода:\n" +
                        "1000 Gold\n" +
                        "1000 ECrypt\n" +
                        "\n" +
                        "Курс вывода:\n" +
                        "100 Gold = 60 руб.\n" +
                        "100 ECrypt = 300 руб.")
                .row()
                .button("Вывести Gold \uD83D\uDCB0", "/withdraw_gold")
                .row()
                .button("Вывести ECrypt ⚡️","/withdraw_ecrypt");

        return messageBuilder.build();
    }

    public SendMessage withdrawGold(Update update){
        int userId = update.getCallbackQuery().getFrom().getId();
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));
        User user = userService.getOrCreate(userId);

        messageBuilder
                .line("Введите количество \uD83D\uDCB0 Gold для вывода.\n" +
                        "Необходимо иметь 1 \uD83D\uDD37 балл на каждые 100 Gold.");

        user.setPositions("withdraw_gold");
        userService.update(user);
        createCancel();

        SendMessage sendMessage = messageBuilder.build();
        sendMessage.setReplyMarkup(keyboard);

        return sendMessage;
    }
    // ввести голду для вывода
    public SendMessage withdrawGoldImpl(Update update){
        int userId = update.getMessage().getFrom().getId();
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));
        User user = userService.getOrCreate(userId);

        int quantity = 0;

        try{
            quantity = Integer.parseInt(update.getMessage().getText());
        }catch (Exception e){
            return messageBuilder
                    .line("Вы ввели не число")
                    .build();
        }
        int userGold = quantity;
        double userBalls2 = user.getBallsTwo();

        if (quantity < user.getGold()){
            return messageBuilder
                    .line("У вас недостаточно \uD83D\uDCB0 Gold на счете.\n")
                    .build();
        }

        if (userGold < 1000){
            return messageBuilder
                    .line("У вас недостаточно \uD83D\uDCB0 Gold на счете.\n" +
                            "Необходимо иметь 1000 \uD83D\uDCB0 Gold.")
                    .build();
        }
        int needBalls = userGold / 100;

        if (userBalls2 < needBalls){
            return messageBuilder
                    .line("У вас недостаточно \uD83D\uDD37 баллов на счете.\n" +
                            "Необходимо иметь 1\uD83D\uDD37 балл на 100 Gold.")
                    .build();
        }

        Payment payment = new Payment();
        int roubles = (userGold / 100) * 60;
        payment.setSum(roubles);
        payment.setUserId(userId);
        payment.setTime(new SimpleDateFormat("dd.MM.yyyy").format(new Date()));
        payment.setPTime(new SimpleDateFormat("H:m").format(new Date()));
        payment.setSuccess(false);
        paymentService.update(payment);

        user.setGold(user.getGold() - quantity);
        user.setBallsTwo(user.getBallsTwo() - needBalls);
        user.setPositions("back");
        userService.update(user);

        messageBuilder
                .line("Вы сделали запрос на вывод " + roubles + "\n")
                .line("Вывод обрабатывается вручную, время ожидания 1-3 суток");
        return messageBuilder.build();
    }

    public SendMessage withdrawECrypt(Update update){
        int userId = update.getCallbackQuery().getFrom().getId();
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));
        User user = userService.getOrCreate(userId);

        messageBuilder
                .line("Введите количество ⚡️ Ecrypt для вывода.\n" +
                        "Необходимо иметь 1 \uD83D\uDD36 балл на каждые 100 ECrypt.");

        user.setPositions("withdraw_ecrypt");
        userService.update(user);
        createCancel();

        SendMessage sendMessage = messageBuilder.build();
        sendMessage.setReplyMarkup(keyboard);

        return sendMessage;
    }

    // ввести ECrypt для вывода
    public SendMessage withdrawECryptImpl(Update update){
        int userId = update.getMessage().getFrom().getId();
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));
        User user = userService.getOrCreate(userId);

        int quantity = 0;

        try{
            quantity = Integer.parseInt(update.getMessage().getText());
        }catch (Exception e){
            return messageBuilder
                    .line("Вы ввели не число")
                    .build();
        }
        int userECrypt = quantity;
        double userBalls1 = user.getBallsOne();

        if (quantity < user.getGold()){
            return messageBuilder
                    .line("У вас недостаточно ⚡️ ECrypt на счете.\n")
                    .build();
        }

        if (userECrypt < 1000){
            return messageBuilder
                    .line("У вас недостаточно ⚡️ ECrypt на счете.\n" +
                            "Необходимо иметь 1000 ⚡️ ECrypt.")
                    .build();
        }
        int needBalls = userECrypt / 100;

        if (userBalls1 < needBalls){
            return messageBuilder
                    .line("У вас недостаточно \uD83D\uDD36 баллов на счете.\n" +
                            "Необходимо иметь 1 \uD83D\uDD36 балл на 100 ⚡️ ECrypt.")
                    .build();
        }

        Payment payment = new Payment();
        int roubles = (userECrypt / 100) * 300;
        payment.setSum(roubles);
        payment.setUserId(userId);
        payment.setTime(new SimpleDateFormat("dd.MM.yyyy").format(new Date()));
        payment.setPTime(new SimpleDateFormat("H:m").format(new Date()));
        payment.setSuccess(false);
        paymentService.update(payment);

        user.setECrypt(user.getECrypt() - quantity);
        user.setBallsOne(user.getBallsOne() - needBalls);
        user.setPositions("back");
        userService.update(user);

        messageBuilder
                .line("Вы сделали запрос на вывод " + roubles + "\n")
                .line("Вывод обрабатывается вручную, время ожидания 1-3 суток");
        return messageBuilder.build();
    }

    public void createCancel(){
        ArrayList<KeyboardRow> rows = new ArrayList<>();
        KeyboardRow keyboardRow = new KeyboardRow();
        keyboardRow.add("✖️ Отмена");
        rows.add(keyboardRow);
        keyboard.setResizeKeyboard(true);
        keyboard.setKeyboard(rows);
    }
}
