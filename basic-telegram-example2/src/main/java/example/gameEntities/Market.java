package example.gameEntities;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import org.telegram.telegrambots.meta.api.methods.send.SendMessage;
import org.telegram.telegrambots.meta.api.objects.Update;
import org.telegram.telegrambots.meta.api.objects.replykeyboard.ReplyKeyboardMarkup;
import org.telegram.telegrambots.meta.api.objects.replykeyboard.buttons.KeyboardRow;

import example.bot.builder.MessageBuilder;
import example.model.User;
import example.service.UserService;

import java.util.ArrayList;
import java.util.List;

@Component
@RequiredArgsConstructor
public class Market {

    private final UserService userService;
    private final ReplyKeyboardMarkup keyboard = new ReplyKeyboardMarkup();

    public SendMessage mark(Update update){
        int userId = update.getMessage().getFrom().getId();

        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));
        User user = userService.getOrCreate(userId);
        messageBuilder
                .line()
                .line("\uD83D\uDED2 Рынок\n" +
                        "  \n" +
                        "Добро пожаловать на рынок!\n" +
                        "Здесь Вы можете продать ресурсы со склада.\n" +
                        "\n" +
                        "\uD83D\uDCE6 На складе:\n" +
                        user.getOilProducted() + " \uD83D\uDEE2 баррелей нефти\n" +
                        user.getElectricProducted() + " \uD83D\uDD0B киловатт энергии\n" +
                        "\n" +
                        "Расценки продажи:\n" +
                        "500 баррелей нефти = 1 \uD83D\uDCB0 Gold и 2 \uD83C\uDF11 OilCoin.\n" +
                        "500 киловатт энергии = 1 ⚡️ ECrypt  и 2 \uD83C\uDF15 ECoin\n" +
                        "\n" +
                        "Минимум для продажи: 500 \uD83D\uDEE2 баррелей нефти / 500 \uD83D\uDD0B киловатт энергии")
                .row()
                .button("Продать \uD83D\uDEE2 баррели нефти", "/sell_oil")
                .row()
                .button("Продать \uD83D\uDD0B киловатт энергии", "/sell_electric");
        return messageBuilder.build();
    }

    public SendMessage sellOil(Update update) {
        int userId = update.getCallbackQuery().getFrom().getId();
        User user = userService.getOrCreate(userId);
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));

        user.setPositions("sell_oil");
        userService.update(user);

        createCancelMenu();

        SendMessage sendMessage =  messageBuilder
                .line("Введите кол-во \uD83D\uDEE2 баррелей нефти для продажи")
                .build();
        sendMessage.setReplyMarkup(keyboard);
        return sendMessage;
    }
     // ввести кол-во нефти для продажи
    public SendMessage sellOilImpl(Update update) {
        int userId = update.getMessage().getFrom().getId();
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));
        int quantity = 0;
        int gold = 0;
        int oilCoin = 0;
        User user = userService.getOrCreate(userId);

        if ("back".equalsIgnoreCase(user.getPositions())){
            return new SendMessage();
        }

        try {
            quantity = Integer.parseInt(update.getMessage().getText());
        }
        catch (Exception e){
            return messageBuilder
                    .line("Вы ввели не число")
                    .build();
        }

        if (user.getOilProducted() < quantity){
            return messageBuilder
                    .line("Недостаточно нефти на складе!")
                    .build();
        }

        gold = quantity / 500;
        oilCoin = quantity / 250;
        user.setOilProducted(user.getOilProducted() - quantity);
        userService.update(user);

        user.setPositions("back");
        user.setGold(user.getGold() + gold);
        user.setOilCoin(user.getOilCoin() + oilCoin);
        userService.update(user);

        return messageBuilder
                .line("Вы продали " + " \uD83D\uDEE2 баррелей нефти\n")
                .line("Получили: " + gold + "\uD83D\uDCB0 Gold и " + oilCoin + " \uD83C\uDF11 OilCoin.")
                .build();
    }

    public SendMessage sellElectric(Update update) {
        int userId = update.getCallbackQuery().getFrom().getId();
        User user = userService.getOrCreate(userId);
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));

        user.setPositions("sell_electric");
        userService.update(user);
        createCancelMenu();

        SendMessage sendMessage =  messageBuilder
                .line("Введите кол-во \uD83D\uDD0B киловатт энергии")
                .build();
        sendMessage.setReplyMarkup(keyboard);
        return sendMessage;
    }

    // ввести кол-во энергии для продажи
    public SendMessage sellElectricImpl(Update update) {
        int userId = update.getMessage().getFrom().getId();
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));
        int quantity = 0;
        int eCrypt = 0;
        int eCoin = 0;
        User user = userService.getOrCreate(userId);

        if ("back".equalsIgnoreCase(user.getPositions())){
            return new SendMessage();
        }

        try {
            quantity = Integer.parseInt(update.getMessage().getText());
        }
        catch (Exception e){
            return messageBuilder
                    .line("Вы ввели не число")
                    .build();
        }

        if (user.getElectricProducted() < quantity){
            return messageBuilder
                    .line("Недостаточно энергии на складе!")
                    .build();
        }

        eCrypt = quantity / 500;
        eCoin = quantity / 250;
        user.setElectricProducted(user.getElectricProducted() - quantity);
        userService.update(user);

        user.setECrypt(user.getECrypt() + eCrypt);
        user.setECoin(user.getECoin() + eCoin);
        user.setPositions("back");
        userService.update(user);

        return messageBuilder
                .line("Вы продали " + " \uD83D\uDD0B киловатт энергии\n")
                .line("Получили: " + eCrypt + "⚡️ ECrypt и " + eCoin + " \uD83C\uDF15 ECoin.")
                .build();
    }


    public boolean enoughOil(int id){
        User user = userService.getOrCreate(id);
        return user.getOilProducted() >=500;
    }

    public boolean enoughElectric(int id){
        User user = userService.getOrCreate(id);
        return user.getElectricProducted() >=500;
    }


    public void createCancelMenu(){
        List<KeyboardRow> rowList = new ArrayList<>();

        KeyboardRow keyboardRow2 = new KeyboardRow();
        keyboardRow2.add("⬅️ Назад");

        rowList.add(keyboardRow2);
        keyboard.setResizeKeyboard(true);
        keyboard.setKeyboard(rowList);
    }
}
