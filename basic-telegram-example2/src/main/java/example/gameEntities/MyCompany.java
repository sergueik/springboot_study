package example.gameEntities;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import org.telegram.telegrambots.meta.api.methods.send.SendMessage;
import org.telegram.telegrambots.meta.api.objects.Update;
import org.telegram.telegrambots.meta.api.objects.replykeyboard.ReplyKeyboardMarkup;
import org.telegram.telegrambots.meta.api.objects.replykeyboard.buttons.KeyboardRow;

import example.bot.builder.MessageBuilder;
import example.model.*;
import example.service.*;

import java.util.ArrayList;
import java.util.List;

@Component
@RequiredArgsConstructor
public class MyCompany {

    private final UserService userService;
    private final OilPumpService oilPumpService;
    private final PowerhouseService powerhouseService;
    private final ActionsService actionsService;

    private final ReplyKeyboardMarkup keyboardMarkups = new ReplyKeyboardMarkup();

    public SendMessage main(Update update){
        String userId = String.valueOf(update.getMessage().getFrom().getId());
        MessageBuilder messageBuilder = MessageBuilder.create(userId);
        messageBuilder
                .line()
                .line("\uD83C\uDFED Моя компания\n" +
                        "  \n" +
                        "Здесь Вы найдете основную информацию по Вашей компании.");

        createMenu();

        SendMessage sendMessage = messageBuilder.build();
        sendMessage.setReplyMarkup(keyboardMarkups);

        return sendMessage;
    }

    public void createMenu(){
        List<KeyboardRow> rowList = new ArrayList<>();
        keyboardMarkups.setResizeKeyboard(true);

        KeyboardRow keyboardRow = new KeyboardRow();
        keyboardRow.add("⛽️ Нефтяные насосы");
        keyboardRow.add("\uD83D\uDD0C Электростанции");


        KeyboardRow keyboardRow2 = new KeyboardRow();
        keyboardRow2.add("\uD83D\uDCCA Статистика");
        keyboardRow2.add("\uD83D\uDC65 Рефералы");

        KeyboardRow keyboardRow3 = new KeyboardRow();
        keyboardRow3.add("\uD83D\uDCDC Задания");
        keyboardRow3.add("⬅️ Назад");

        rowList.add(keyboardRow);
        rowList.add(keyboardRow2);
        rowList.add(keyboardRow3);

        keyboardMarkups.setKeyboard(rowList);
    }

    public SendMessage oilPump(Update update){
        int userId = update.getMessage().getFrom().getId();
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));
        User user = userService.get(userId).get();

        messageBuilder
                .line()
                .line("⛽️ Нефтяные насосы\n" +
                        "  \n" +
                        "Здесь Вы можете купить различные нефтяные насосы. Нефтяные насосы добывают \uD83D\uDEE2 баррели нефти, которые Вы впоследствии можете отправить на склад и продать на рынке за \uD83C\uDF11 OilCoin и \uD83D\uDCB0 Gold и впоследствии можете вывести как реальные деньги!\n" +
                        "\n" +
                        "Ваши нефтяные насосы:\n" +
                        "⛽️1️⃣ Деревянный ручной насос\n" +
                        "Количество: "+ levelCount(1, userId) +"\n" +
                        "Добыто: "+ productCount(1, userId) +" \uD83D\uDEE2 баррелей нефти\n" +
                        "\n" +
                        "⛽️2️⃣ Металлический насос\n" +
                        "Количество: "+ levelCount(2,userId) +"\n" +
                        "Добыто: "+ productCount(2, userId) +" \uD83D\uDEE2 баррелей нефти\n" +
                        "\n" +
                        "⛽️3️⃣ Фабричный  насос\n" +
                        "Количество: "+ levelCount(3, userId) +"\n" +
                        "Добыто: "+ productCount(3, userId) +" \uD83D\uDEE2 баррелей нефти\n" +
                        "\n" +
                        "⛽️4️⃣ Профессиональный насос\n" +
                        "Количество: "+ levelCount( 4, userId) +"\n" +
                        "Добыто: "+ productCount( 4, userId) +" \uD83D\uDEE2 баррелей нефти\n" +
                        "\n" +
                        "⛽️5️⃣ Насос с ионным двигателем\n" +
                        "Количество: "+ levelCount(5, userId) +"\n" +
                        "Добыто: "+ productCount( 5, userId) +" \uD83D\uDEE2 баррелей нефти\n" +
                        "\n" +
                        "⛽️6️⃣ Насос на квантовой тяге\n" +
                        "Количество: "+ levelCount( 6, userId) +"\n" +
                        "Добыто: "+ productCount(6, userId) +" \uD83D\uDEE2 баррелей нефти\n" +
                        "\n" +
                        "\uD83D\uDCE6 Баррелей нефти на складе: "+ user.getOilProducted() +"\n" +
                        "\n" +
                        "Вы отдаете 30% всей добываемой Вами нефти акционеру.");

        messageBuilder
                .row()
                .button("⛽ Купить насосы", "/buy_oilPump")
                .row()
                .button("\uD83D\uDCE6 Отправить ресурсы на склад", "/send_toBoxOil");


        return messageBuilder.build();
    }


    public SendMessage electric(Update update){

        int userId = update.getMessage().getFrom().getId();
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));
        User user = userService.get(userId).get();

        messageBuilder
                .line()
                .line("\uD83D\uDD0C Электростанции\n" +
                        "  \n" +
                        "Здесь Вы можете купить электростанции разных видов. Электростанции вырабатывают \uD83D\uDD0B киловатты энергии, которые Вы впоследствии можете запасать в аккумуляторах и  продавать на рынке за \uD83C\uDF15 ECoin и ⚡️ ECrypt и впоследствиии можете вывести как реальные деньги!\n" +
                        "\n" +
                        "Ваши электростанции:\n" +
                        "\uD83D\uDD0C1️⃣ Солнечная электростанция\n" +
                        "Количество: "+ levelCountElectric( 1, userId) +"\n" +
                        "Добыто: "+ productCountElectric( 1, userId) +" \uD83D\uDD0B киловатт энергии\n" +
                        "\n" +
                        "\uD83D\uDD0C2️⃣ Ветряная электростанция\n" +
                        "Количество: "+ levelCountElectric( 2, userId) +"\n" +
                        "Добыто: "+ productCountElectric(2, userId) +" \uD83D\uDD0B киловатт энергии\n" +
                        "\n" +
                        "\uD83D\uDD0C3️⃣ Тепловая электростанция\n" +
                        "Количество: "+ levelCountElectric(3, userId) +"\n" +
                        "Добыто: "+ productCountElectric( 3, userId) +" \uD83D\uDD0B киловатт энергии\n" +
                        "\n" +
                        "\uD83D\uDD0C4️⃣ Гидроэлектростанция\n" +
                        "Количество: "+ levelCountElectric( 4, userId) +"\n" +
                        "Добыто: "+ productCountElectric( 4, userId) +" \uD83D\uDD0B киловатт энергии\n" +
                        "\n" +
                        "\uD83D\uDD0C5️⃣ Атомная электростанция\n" +
                        "Количество: "+ levelCountElectric( 5, userId) +"\n" +
                        "Добыто: "+ productCountElectric( 5, userId) +" \uD83D\uDD0B киловатт энергии\n" +
                        "\n" +
                        "\uD83D\uDD0C6️⃣ Термоядерная электростанция\n" +
                        "Количество: "+ levelCountElectric( 6, userId) +"\n" +
                        "Добыто: "+ productCountElectric(6, userId) +" \uD83D\uDD0B киловатт энергии\n" +
                        "\n" +
                        "\uD83D\uDCE6 Киловатт энергии в аккумуляторах на складе: "+user.getElectricProducted()+"\n" +
                        "  \n" +
                        "Вы отдаете 30% всей добываемой Вами киловатт энергии акционеру.");

        messageBuilder
                .row()
                .button("\uD83D\uDD0C Купить электростанции", "/buy_electric")
                .row()
                .button("\uD83D\uDCE6 Отправить ресурсы на склад", "/send_toBoxElectric");


        return messageBuilder.build();
    }


    public int levelCount(int level, int id){
        List<OilPump> oilP = oilPumpService.findByLevel(level, id);
        return oilP.size();
    }

    public int levelCountElectric(int level, int id){
        List<Powerhouse> oilP = powerhouseService.findByLevel(level, id);
        return oilP.size();
    }

    public int productCountElectric(int level, int id){
        List<Powerhouse> oilP = powerhouseService.findByLevel(level, id);
        return oilP.stream().mapToInt(Powerhouse::getProducted).sum();
    }


    public int productCount(int level, int id){
        List<OilPump> oilP = oilPumpService.findByLevel(level, id);
        return oilP.stream().mapToInt(OilPump::getProducted).sum();
    }

    public List<SendMessage> buyOilPump(Update update){
        int userId = update.getCallbackQuery().getFrom().getId();
        List<SendMessage> messages = new ArrayList<>();
        String[] emojies = new String[]{"1️⃣", "2️⃣", "3️⃣", "4️⃣","5️⃣","6️⃣"};
        OilPump oilPump;

        for (int i = 0; i < 6; i++) {
            oilPump = oilPumpService.findById(i+1);
            MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));
            messageBuilder
                    .line()
                    .line("⛽️"+ emojies[i] +" " + oilPump.getName() + "\n" +
                            "  \n" +
                            "Добывает "+ oilPump.getProduction() +" \uD83D\uDEE2 баррелей нефти в час\n" +
                            "Цена: " + oilPump.getPrice() + " \uD83C\uDF11 OilCoin")
                    .row()
                    .button("\uD83D\uDED2 Купить", "/buy_oilPump" + i)
                    .row()
                    .button("\uD83D\uDECD Купить оптом", "/buy_oilPumpOptom" + i);
            messages.add(i, messageBuilder.build());
        }
        return messages;
    }

    public List<SendMessage> buyElectric(Update update){
        int userId = update.getCallbackQuery().getFrom().getId();
        List<SendMessage> messages = new ArrayList<>();
        String[] emojies = new String[]{"1️⃣", "2️⃣", "3️⃣", "4️⃣","5️⃣","6️⃣"};
        List<Powerhouse> powerhouses = powerhouseService.findAll();
        Powerhouse powerhouse;

        for (int i = 0; i < 6; i++) {
            powerhouse = powerhouses.get(i);
            MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));
            messageBuilder
                    .line()
                    .line("⛽️"+ emojies[i] +" "+ powerhouse.getName() +"\n" +
                            "  \n" +
                            "Добывает "+ powerhouse.getProduction() +" \uD83D\uDD0B киловатт в час\n" +
                            "Цена: " + powerhouse.getPrice() + " \uD83C\uDF15 ECoin")
                    .row()
                    .button("\uD83D\uDED2 Купить", "/buy_electric" + i)
                    .row()
                    .button("\uD83D\uDECD Купить оптом", "/buy_electricOptom" + i);
            messages.add(i, messageBuilder.build());
        }
        return messages;
    }

    public SendMessage buyOilPumpImpl(Update update, int level){
        int userId = update.getCallbackQuery().getFrom().getId();

        OilPump oilPump = new OilPump();
        OilPump oilPumpImpl = oilPumpService.findById(level);

        oilPump.setName(oilPumpImpl.getName());
        oilPump.setPrice(oilPumpImpl.getPrice());
        oilPump.setProduction(oilPumpImpl.getProduction());
        oilPump.setUserId(userId);
        oilPump.setLevel(level);

        oilPumpService.save(oilPump);

        User user = userService.getOrCreate(userId);
        user.setOilCoin(user.getOilCoin() - oilPump.getPrice());
        userService.update(user);

        user.setOilProductTime(user.getOilProductTime() + oilPump.getProduction());
        userService.update(user);

        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));
        messageBuilder
                .line()
                .line("Вы купили " + oilPump.getName());

        return messageBuilder.build();
    }

    public SendMessage buyElectricImpl(Update update, int level){
        int userId = update.getCallbackQuery().getFrom().getId();

        Powerhouse powerhouse = new Powerhouse();
        Powerhouse electricImpl = powerhouseService.findById(level);

        powerhouse.setName(electricImpl.getName());
        powerhouse.setPrice(electricImpl.getPrice());
        powerhouse.setProduction(electricImpl.getProduction());
        powerhouse.setUserId(userId);
        powerhouse.setLevel(level);

        powerhouseService.save(powerhouse);

        User user = userService.getOrCreate(userId);
        user.setECoin(user.getECoin() - powerhouse.getPrice());
        userService.update(user);

        user.setElectricProductTime(user.getElectricProductTime() + powerhouse.getProduction());
        userService.update(user);

        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));
        messageBuilder
                .line()
                .line("Вы купили " + powerhouse.getName());

        return messageBuilder.build();
    }

    public SendMessage buyElectricOptom(Update update, int level){
        int userId = update.getCallbackQuery().getFrom().getId();

        User user = userService.getOrCreate(userId);
        user.setPositions("buyElectricOptom" + level);
        userService.update(user);

        Powerhouse powerhouse = powerhouseService.findById(level);
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));
        messageBuilder
                .line()
                .line("Вы хотите купить " + powerhouse.getName() + "\n" +
                        "Введите количество");
        return messageBuilder.build();
    }

    public SendMessage buyOilPumpOptom(Update update, int level){
        int userId = update.getCallbackQuery().getFrom().getId();

        User user = userService.getOrCreate(userId);
        user.setPositions("buyOilPumpOptom" + level);
        userService.update(user);

        OilPump oilPump = oilPumpService.findById(level);
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));
        messageBuilder
                .line()
                .line("Вы хотите купить " + oilPump.getName() + "\n" +
                        "Введите количество");
        return messageBuilder.build();
    }

    public SendMessage buyElectricOptomImpl(Update update, int level, int quantity){
        Powerhouse electricImpl = powerhouseService.findById(level);
        int price = electricImpl.getPrice() * quantity;
        int userId = update.getMessage().getFrom().getId();
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));

        User user = userService.getOrCreate(userId);

        if (user.getECoin() < price){
            return messageBuilder
                    .line()
                    .line("Недостаточно \uD83C\uDF15ECoin для оплаты\n" +
                            "Требуется " + price + "\uD83C\uDF15 \n" +
                            "У вас " + user.getECoin())
                    .build();
        }

        for (int i = 0; i < quantity; i++) {
            Powerhouse powerhouse = new Powerhouse();
            powerhouse.setName(electricImpl.getName());
            powerhouse.setPrice(electricImpl.getPrice());
            powerhouse.setProduction(electricImpl.getProduction());
            powerhouse.setUserId(userId);
            powerhouse.setLevel(level);
            powerhouseService.save(powerhouse);
            user.setElectricProductTime(user.getElectricProductTime() + powerhouse.getProduction());
        }

        user.setPositions("back");
        userService.update(user);
        userService.update(user);

        return messageBuilder
                .line()
                .line("Вы купили " + electricImpl.getName() +"\n" +
                        "в количестве " + quantity)
                .build();
    }

    public SendMessage buyOilPumpOptomImpl(Update update, int level, int quantity){
        OilPump oilPumpImpl = oilPumpService.findById(level);
        int price = oilPumpImpl.getPrice() * quantity;
        int userId = update.getMessage().getFrom().getId();
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));

        User user = userService.getOrCreate(userId);

        if (user.getOilCoin() < price){
            return messageBuilder
                    .line()
                    .line("Недостаточно \uD83C\uDF11OilCoin для оплаты\n" +
                            "Требуется " + price + "\uD83C\uDF15 \n" +
                            "У вас " + user.getECoin())
                    .build();
        }

        for (int i = 0; i < quantity; i++) {
            OilPump oilPump = new OilPump();
            oilPump.setName(oilPumpImpl.getName());
            oilPump.setPrice(oilPumpImpl.getPrice());
            oilPump.setProduction(oilPumpImpl.getProduction());
            oilPump.setUserId(userId);
            oilPump.setLevel(level);
            oilPumpService.save(oilPump);
            user.setOilProductTime(user.getOilProductTime() + oilPump.getProduction());
        }

        user.setPositions("back");
        userService.update(user);
        userService.update(user);

        return messageBuilder
                .line()
                .line("Вы купили " + oilPumpImpl.getName() +"\n" +
                        "в количестве " + quantity)
                .build();
    }

    public SendMessage stat(Update update){
        int userId = update.getMessage().getFrom().getId();
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));
        User user = userService.get(userId).get();

        int sumProductionOil = user.getOilProductTime();
        int sumProductionPower = user.getElectricProductTime();

        int sumProductedOil = user.getOilProducted();
        int sumProductedPower = user.getElectricProducted();

        messageBuilder
                .line()
                .line("\uD83D\uDCCA Статистика Вашей компании\n" +
                        "  \n" +
                        "\uD83D\uDCDD Название Вашей компании:\n" +
                        " '" + user.getName() + "'\n" +
                        "\n" +
                        "\uD83D\uDCC5 Компания зарегистрирована:\n" +
                        " " + user.getRegDate() + "\n" +
                        "\n" +
                        "На данный момент Ваши установки добывают:\n" +
                        sumProductionOil +" \uD83D\uDEE2 баррелей в час\n" +
                        sumProductionPower + " \uD83D\uDD0B киловатт энергии в час\n" +
                        " \n" +
                        "\uD83D\uDEE2 На складе баррелей нефти:\n" +
                        " "+ sumProductedOil +"\n" +
                        "\n" +
                        "\uD83D\uDD0B На складе киловатт энергии:\n" +
                        " "+ sumProductedPower +"\n" +
                        "\n" +
                        "\uD83D\uDC64 Компаний, зарегистрировавшихся по Вашей реферальной ссылке:\n" +
                        " "+ userService.countReferals(userId) +"\n" +
                        "\n" +
                        "✅ Выполнено заданий:\n" +
                        " "+ user.getTaskCompleted() +"\n" +
                        "\n" +
                        "\uD83D\uDCD1 Доля Вашей компании, принадлежащая Вам:\n" +
                        " 70%");
        return messageBuilder.build();
    }

    public List<SendMessage> referals(Update update){
        int userId = update.getMessage().getFrom().getId();
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));
        User user = userService.get(userId).get();
        List<SendMessage> messages = new ArrayList<>();

        messageBuilder
                .line()
                .line("\uD83D\uDC65 Реферальная ссылка\n" +
                        "  \n" +
                        "За каждую регистрацию по Вашей реферальной ссылке, Вы будете получать 40 \uD83C\uDF11 OilCoin и 20 \uD83C\uDF15 ECoin. ( ❗️Реферал должен дать название своей компании❗️)\n" +
                        "Также, каждый, кто регистрируется по Вашей реферальной ссылке автоматически передает Вам 30% своих \uD83D\uDCC9 акций.\n" +
                        "\n" +
                        "Вы привели: "+ userService.countReferals(userId) +" реферала(ов).\n" +
                        "\n" +
                        "Ссылка:\n");
        messages.add(0, messageBuilder.build());

        SendMessage sendMessage = new SendMessage();
                 sendMessage.setChatId(String.valueOf(userId));
                 sendMessage.setText(user.getReferencesUrl());
                 sendMessage.disableWebPagePreview();

        messages.add(1, sendMessage);

        return messages;
    }

    public SendMessage oilToBox(Update update){
        int userId = update.getCallbackQuery().getFrom().getId();
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));
        User user = userService.get(userId).get();
        int oil = 0;

        List<OilPump> oilPumps = oilPumpService.findByUserId(userId);

        if (oilPumps == null || oilPumps.isEmpty()){
            return messageBuilder
                    .line("У вас нет нефтяных установок")
                    .build();
        }

        for (OilPump oilPump : oilPumps){
            oil = oil + oilPump.getProducted();
            oilPump.setProducted(0);
            oilPumpService.save(oilPump);
        }

        if (oil == 0){
            return messageBuilder
                    .line("У вас нет нефти, которую можно было бы собрать")
                    .build();
        }else {
            user.setOilProducted(user.getOilProducted() + oil);
            userService.update(user);
        }
        return messageBuilder
                .line("Собрано нефти: " + oil)
                .build();
    }

    public SendMessage electricToBox(Update update){
        int userId = update.getCallbackQuery().getFrom().getId();
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));
        User user = userService.get(userId).get();
        int electric = 0;

        List<Powerhouse> powerhouses = powerhouseService.findByUserId(userId);

        if (powerhouses == null || powerhouses.isEmpty()){
            return messageBuilder
                    .line("У вас нет электростанций")
                    .build();
        }

        for (Powerhouse powerhouse : powerhouses){
            electric = electric + powerhouse.getProducted();
            powerhouse.setProducted(0);
            powerhouseService.save(powerhouse);
        }

        if (electric == 0){
            return messageBuilder
                    .line("У вас нет энергии, которую можно было бы собрать")
                    .build();
        }else {
            user.setElectricProducted(user.getElectricProducted() + electric);
            userService.update(user);
        }
        return messageBuilder
                .line("Собрано энергии: " + electric)
                .build();
    }

    public SendMessage task(Update update){
        int userId = update.getMessage().getFrom().getId();
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));
        User user = userService.getOrCreate(userId);

        if (user.getTaskCompleted() == 0) {
            messageBuilder.line("\uD83D\uDCDC Задания\n" +
                    "\n" +
                    "Похоже, это Ваше первое задание! Что же, приступим. Первым делом, Вам необходимо придумать название для своей компании. Для этого Вам нужно пройти в раздел \uD83D\uDCA1 Дополнительно. Уверен, Вы без проблем найдете, как и где изменить название! \uD83D\uDE03\n" +
                    "\n" +
                    "\uD83D\uDCD2 *Задание: Придумать название для компании\n" +
                    "\uD83E\uDD47 Награда: 100 \uD83C\uDF11 OilCoin*")
                    .row()
                    .button("\uD83D\uDCCE Проверить задание", "/taskOne");
            return messageBuilder.build();
        }
        else if (user.getTaskCompleted() == 1){
            return taskTwo(update);
        }
        else if (user.getTaskCompleted() == 2){
          return taskThree(update);
        }
        else if (user.getTaskCompleted() == 3){
            return taskFour(update);
        }
        else if (user.getTaskCompleted() == 4){
            return taskFive(update);
        }
        else return taskEnd(update);
    }

    public List<SendMessage> taskOne(Update update){
        int userId = update.getCallbackQuery().getFrom().getId();
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));
        User user = userService.getOrCreate(userId);
        List<SendMessage> messages = new ArrayList<>();

            if (user.getName().equalsIgnoreCase("Без названия")) {
                messages.add(messageBuilder.line("Вы не указали название боту").build());
                return messages;
            }

            messageBuilder.line("✅ Задание успешно выполнено!\n" +
                    "    \n" +
                    "\uD83E\uDD47 Получена награда:  100 \uD83C\uDF11 OilCoin");
            messages.add(messageBuilder.build());

            user.setOilCoin(user.getOilCoin() + 100);
            user.setTaskCompleted(1);
            userService.update(user);

            messages.add(taskTwo(update));

        return messages;
    }

    public SendMessage taskTwo(Update update){
        int userId = 0;
        if (update.hasCallbackQuery()) {
            userId = update.getCallbackQuery().getFrom().getId();
        }
        else userId = update.getMessage().getFrom().getId();

        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));
        messageBuilder.line("\uD83D\uDCDC Задания\n" +
                "\n" +
                "Я смотрю, Вы отлично справились со своим первым заданием. В качестве награды Вы получили 100 \uD83C\uDF11 OilCoin! \n" +
                "   \n" +
                "Давайте купим на них ⛽️1️⃣ Деревянный ручной насос. Для этого перейдите в раздел \uD83C\uDFED Моя компания, выберите ⛽️ Нефтяные насосы и, в появившемся сообщении, нажмите на кнопку ⛽️ Купить насосы. В выпавшем списке найдите деревянный насос и нажмите купить. После покупки возвращайтесь сюда за наградой. \uD83D\uDE09\n" +
                "\n" +
                "\uD83D\uDCD2 Задание: Купить ⛽️1️⃣ Деревянный ручной насос\n" +
                "\uD83E\uDD47 Награда: 25 \uD83C\uDF15 ECoin")
                .row()
                .button("\uD83D\uDCCE Проверить задание", "/taskTwo");
        return messageBuilder.build();
    }

    public List<SendMessage> taskTwoImpl(Update update){
        int userId = update.getCallbackQuery().getFrom().getId();
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));
        User user = userService.getOrCreate(userId);
        List<SendMessage> messages = new ArrayList<>();

        List<OilPump> oilPumps = oilPumpService.findByLevel(1, userId);
        if (oilPumps.isEmpty()){
            messages.add(messageBuilder.line("Вы не купили Деревянный ручной насос!").build());
            return messages;
        }

        messageBuilder.line("✅ Задание успешно выполнено!\n" +
                "    \n" +
                "\uD83E\uDD47 Получена награда:  25 \uD83C\uDF15 ECoin");

        user.setECoin(user.getECoin() + 25);
        user.setTaskCompleted(2);
        userService.update(user);
        messages.add(messageBuilder.build());

        messages.add(taskThree(update));

        return messages;
    }

    public SendMessage taskThree(Update update){
        int userId = 0;
        if (update.hasCallbackQuery()) {
            userId = update.getCallbackQuery().getFrom().getId();
        }
        else userId = update.getMessage().getFrom().getId();
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));
        messageBuilder.line("\uD83D\uDCDC Задания\n" +
                "\n" +
                "Вот и Ваша первая фабрика! На данный момент, Вам принадлежит лишь 70% вашей компании. Остальные 30% принадлежат либо \uD83C\uDFEB Бирже (которая, в свою очередь, выставила Ваши \uD83D\uDCC9 акции (долю Вашей компании) на продажу), либо другому игроку, который пригласил Вас в эту игру или же выкупил Ваши \uD83D\uDCC9 акции на \uD83C\uDFEB Бирже.\n" +
                "Сейчас Вы будете получать не 16 \uD83D\uDEE2 баррелей нефти в час, а только 11. В то время как оставшиеся 5 \uD83D\uDEE2 баррелей нефти будут автоматически отправляться на счёт игрока, которому Ваши \uD83D\uDCC9 акции принадлежат.\n" +
                "\n" +
                "Но вернёмся к делу. Сейчас Ваша задача – получить и собрать свою первую \uD83D\uDEE2 нефть. Нефть и другие ресурсы добываются раз в час. Собрать её Вы сможете, нажав на кнопку \uD83D\uDCE6 Отправить ресурсы на склад в разделе ⛽️ Нефтяные насосы (\uD83C\uDFED Моя компания). Как только Вы соберёте нефть, возвращайтесь сюда за наградой. \uD83D\uDE09\n" +
                "\n" +
                "\uD83D\uDCD2 Задание: Собрать нефть на склад\n" +
                "\uD83E\uDD47 Награда: 25 \uD83C\uDF15 ECoin")
                .row()
                .button("\uD83D\uDCCE Проверить задание", "/taskThree");
        return messageBuilder.build();
    }


    public List<SendMessage> taskThreeImpl(Update update){
        int userId = update.getCallbackQuery().getFrom().getId();
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));
        User user = userService.getOrCreate(userId);
        List<SendMessage> messages = new ArrayList<>();

        if (user.getOilProducted() == 0){
            messages.add(messageBuilder.line("Вы не собрали ресурсы на склад!").build());
            return messages;
        }

        messageBuilder.line("✅ Задание успешно выполнено!\n" +
                "    \n" +
                "\uD83E\uDD47 Получена награда:  25 \uD83C\uDF15 ECoin");

        user.setECoin(user.getECoin() + 25);
        user.setTaskCompleted(3);
        userService.update(user);
        messages.add(messageBuilder.build());

        messages.add(taskFour(update));

        return messages;
    }


    public SendMessage taskFour(Update update){
        int userId = 0;
        if (update.hasCallbackQuery()) {
            userId = update.getCallbackQuery().getFrom().getId();
        }
        else userId = update.getMessage().getFrom().getId();
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));
        messageBuilder.line("\uD83D\uDCDC Задания\n" +
                "\n" +
                "Вы неплохо справляетесь с заданиями! Как Вы уже знаете, нефть и другие ресурсы можно продавать. Для этого в игре есть \uD83D\uDED2 Рынок. Минимальная партия для продажи – 500 \uD83D\uDEE2 баррелей нефти. Соберите в сумме как минимум 500 баррелей нефти и продайте ее на рынке.\n" +
                "   \n" +
                "Предупреждаю: на сбор 500 баррелей нефти Вы потратите значительную часть времени. Чтобы выполнить это задание быстрее, рекомендую купить \uD83C\uDF11 OilCoin в \uD83C\uDFE6 Банке и купить на них больше нефтяных насосов. Помните, все вложенные средства обязательно окупятся. \n" +
                "\n" +
                "\uD83D\uDCD2 Задание: Собрать и продать 500 баррелей нефти\n" +
                "\uD83E\uDD47 Награда: 50 \uD83C\uDF15 ECoin")
                .row()
                .button("\uD83D\uDCCE Проверить задание", "/taskFour");
        return messageBuilder.build();
    }

    public List<SendMessage> taskFourImpl(Update update){
        int userId = update.getCallbackQuery().getFrom().getId();
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));
        User user = userService.getOrCreate(userId);
        List<SendMessage> messages = new ArrayList<>();

        if (user.getGold() == 0){
            messages.add(messageBuilder.line("Вы не продали ресурсы на рынке!").build());
            return messages;
        }

        messageBuilder.line("✅ Задание успешно выполнено!\n" +
                "    \n" +
                "\uD83E\uDD47 Получена награда:  50 \uD83C\uDF15 ECoin");

        user.setECoin(user.getECoin() + 50);
        user.setTaskCompleted(4);
        userService.update(user);
        messages.add(messageBuilder.build());

        messages.add(taskFive(update));

        return messages;
    }

    public SendMessage taskFive(Update update){
        int userId = 0;
        if (update.hasCallbackQuery()) {
            userId = update.getCallbackQuery().getFrom().getId();
        }
        else userId = update.getMessage().getFrom().getId();
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));
        messageBuilder.line("\uD83D\uDCDC Задания\n" +
                "\n" +
                "Вы неплохо справляетесь с заданиями! Заключительное задание связано с \uD83D\uDCC9 акциями! Мы уверены, что вы также легко справитесь и с этим заданием\uD83D\uDCC9 " +
                "\n\n\uD83D\uDCD2 Задание: купить акции любой компании\n" +
                "\uD83E\uDD47 Награда: 25 \uD83C\uDF15 ECoin и 25 \uD83C\uDF11 OilCoin")
                .row()
                .button("\uD83D\uDCCE Проверить задание", "/taskFive");
        return messageBuilder.build();
    }


    public List<SendMessage> taskFiveImpl(Update update){
        int userId = update.getCallbackQuery().getFrom().getId();
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));
        User user = userService.getOrCreate(userId);
        List<SendMessage> messages = new ArrayList<>();
        List<Action> action = actionsService.findByUserId(userId);

        if (action.isEmpty()){
            messages.add(messageBuilder.line("Вы не купили акции!").build());
            return messages;
        }

        messageBuilder.line("✅ Задание успешно выполнено!\n" +
                "    \n" +
                "\uD83E\uDD47 Получена награда:  25 \uD83C\uDF15 ECoin и 25 \uD83C\uDF11 OilCoin");

        user.setECoin(user.getECoin() + 25);
        user.setOilCoin(user.getOilCoin() + 25);
        user.setTaskCompleted(5);
        userService.update(user);
        messages.add(messageBuilder.build());

        messages.add(taskEnd(update));

        return messages;
    }

    public SendMessage taskEnd(Update update){
        int userId = 0;
        if (update.hasCallbackQuery()) {
            userId = update.getCallbackQuery().getFrom().getId();
        }
        else userId = update.getMessage().getFrom().getId();
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));
        return messageBuilder.line("Заданий больше нет :(").build();
    }
}
