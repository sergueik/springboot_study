package example.gameEntities;

import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.telegram.telegrambots.meta.api.methods.send.SendMessage;
import org.telegram.telegrambots.meta.api.objects.Update;
import org.telegram.telegrambots.meta.api.objects.replykeyboard.ReplyKeyboardMarkup;
import org.telegram.telegrambots.meta.api.objects.replykeyboard.buttons.KeyboardRow;

import example.bot.builder.MessageBuilder;
import example.model.Action;
import example.model.User;
import example.service.ActionsService;
import example.service.UserService;

import java.util.ArrayList;
import java.util.List;

@Component
@RequiredArgsConstructor
public class Actions {

    private final ActionsService actionsService;
    private final UserService userService;
    private final ReplyKeyboardMarkup keyboard = new ReplyKeyboardMarkup();


    public SendMessage action(Update update){
        int userId = update.getMessage().getFrom().getId();
        MessageBuilder builder = MessageBuilder.create(String.valueOf(userId));
        createMenu();

        builder
                .line("\uD83C\uDFEB Биржа\n" +
                        "\n" +
                        "Добро пожаловать на биржу. Здесь Вы можете купить и продать акции PRO своей и чужой компаний.");
        SendMessage sendMessage = builder.build();
        sendMessage.setReplyMarkup(keyboard);
        return sendMessage;
    }

    public SendMessage actions(Update update){
        int userId = update.getMessage().getFrom().getId();
        MessageBuilder builder = MessageBuilder.create(String.valueOf(userId));
        return builder
                .line("\uD83D\uDCC9 Акции\n" +
                        "  \n" +
                        "Здесь Вы можете купить \uD83D\uDCC9 акции компаний-новичков, чьим акционером является \uD83C\uDFEB Биржа. . Покупка происходит случайным образом.\n" +
                        "\n" +
                        "Акции - это своего рода реферальная система. Главная особенность акций заключается в том, что Вам будет поступать 30% от всех ресурсов, собираемых компанией, акции которой Вы приобрели. Также Вам будут поступать бонусы в виде % от их пополнения.\n" +
                        "Например, если компания, акциями которой Вы владеете, пополняет свой баланс на 1000 \uD83C\uDF11 OilCoin, Вам также будет начислено 30% от этой суммы (т.е. 300 OilCoin). Реферальная система 3-х уровневая, то есть Вам также будут начисляться % от пополнения рефералов Ваших рефералов.\n" +
                        "\n" +
                        "Список всех валют, % от которых будет поступать Вам при пополнении баланса Вашими рефералами и их рефералами:\n" +
                        " \uD83C\uDF11 OilCoin\n" +
                        " \uD83C\uDF15 ECoin\n" +
                        " \uD83D\uDD37 Баллы\n" +
                        " \uD83D\uDD36 Баллы\n" +
                        "Ресурсы, которые поступают к Вам на склад, когда их собирает Ваш реферал:\n" +
                        " \uD83D\uDEE2 Баррели нефти\n" +
                        " \uD83D\uDD0B Киловатты энергии\n" +
                        " \n" +
                        "Особенность обычных акций в том, что компания, акциями которой Вы владеете, может выкупить их в любой момент за 15 \uD83D\uDD37 баллов. Данная сумма будет зачислена на Ваш баланс сразу же после их выкупа.")
                .row()
                .button("➕ Купить за 0,75\uD83D\uDD37", "/buy_action")
                .build();
    }

    public SendMessage buyAction(Update update){
        int userId = update.getCallbackQuery().getFrom().getId();
        double price = 0.75;
        example.model.Action actions = new example.model.Action();
        User user = userService.getOrCreate(userId);
        MessageBuilder builder = MessageBuilder.create(String.valueOf(userId));

        List<User> users = userService.findByUsername();

        if (users.isEmpty()){
            return builder
                    .line("К сожалению, еще нет компаний, указавших название")
                    .build();
        }

        int random = (int)(Math.random() * users.size());
        User user1 = users.get(random);

        Action act = actionsService.findByUserIdAndName(user1.getUserId(), user1.getName());

        user.setBallsTwo(user.getBallsTwo() - price);

        if (act != null){
            act.setQuantity(act.getQuantity()+1);
            actionsService.update(act);
        } else {
            actions.setUserId(userId);
            actions.setNameCompany(user.getName());
            actions.setQuantity(1);

            actionsService.update(actions);
            user.setBallsTwo(user.getBallsTwo() - price);
        }
        userService.update(user);
        return builder
                .line("Вы купили акцию компании " + user.getName())
                .build();
    }

    public SendMessage myActions(Update update){
        int userId = update.getMessage().getFrom().getId();
        MessageBuilder builder = MessageBuilder.create(String.valueOf(userId));
        List<Action> actions = actionsService.findByUserId(userId);

        builder
                .line("\uD83D\uDCD1 Мои акции\n");
        for (example.model.Action action : actions){
            builder.line("Название: " + action.getNameCompany() + " Кол-во: " + action.getQuantity());
        }
        return builder.build();
    }

    public SendMessage findCompany(Update update) {
        int userId = update.getMessage().getFrom().getId();
        MessageBuilder builder = MessageBuilder.create(String.valueOf(userId));
        return builder
                .line("\uD83D\uDD0E Поиск компаний\n" +
                        "  \n" +
                        "Здесь Вы можете найти и посмотреть информацию о других компаниях.")
                .row()
                .button("\uD83D\uDD0E Поиск", "/find_company")
                .build();
    }

    public SendMessage findCompanyImpl(Update update) {
        int userId = update.getCallbackQuery().getFrom().getId();
        MessageBuilder builder = MessageBuilder.create(String.valueOf(userId));

        User user = userService.getOrCreate(userId);
        user.setPositions("find_company");
        userService.update(user);

        return builder
                .line("\uD83D\uDD0E Поиск компаний\n" +
                        "  \n" +
                        "Введите название компании для поиска")
                .build();
    }

    public SendMessage findCompanyCheck(Update update){
        int userId = update.getMessage().getFrom().getId();
        String name = update.getMessage().getText();
        MessageBuilder builder = MessageBuilder.create(String.valueOf(userId));

        User usr = userService.getOrCreate(userId);
        usr.setPositions("back");
        userService.update(usr);

        List<User> companies = userService.findByUsername();
        for (User user : companies){
            if (user.getName().equalsIgnoreCase(name)){
                return builder
                        .line("Информация о компании\n")
                        .line("Название: " + user.getName())
                        .line()
                        .line("Нефти на складе: " + user.getOilProducted())
                        .line()
                        .line("Энергии на складе: "+ user.getElectricProducted())
                        .build();
            }
        }


        return builder
                .line("Такой компании не существует")
                .row()
                .button("Отмена", "/cancel")
                .build();
    }

    public SendMessage myActioner(Update update){
        int userId = update.getMessage().getFrom().getId();
        MessageBuilder builder = MessageBuilder.create(String.valueOf(userId));

        return builder
                .line("\uD83D\uDCBC Мой акционер\n" +
                        "\n" +
                        "Вашим акционером является:\n" +
                        "\uD83C\uDFEB 'Биржа'\n" +
                        "Ей принадлежит 30% Ваших акций.\n" +
                        "Тип выпущенных акций: \n" +
                        "\uD83D\uDCC9 Обычные.\n" +
                        "(Вы можете выкупить свои акции в любой момент)\n" +
                        "\n" +
                        "Биржа выставила Ваши акции на продажу и они могут быть приобретены кем-либо в любой момент.")
                .row()
                .button("Выкупить свои акции - 15 \uD83D\uDD37 ", "/buy_myActions")
                .build();
    }

    public SendMessage myActionerImpl(Update update){
        int userId = update.getCallbackQuery().getFrom().getId();
        MessageBuilder builder = MessageBuilder.create(String.valueOf(userId));
        User user = userService.getOrCreate(userId);
        double price = 15;

        if (user.getBallsTwo() < price){
            return builder
                    .line("\uD83C\uDFEB Биржа\n" +
                            "     \n" +
                            "Недостаточно средств!\n" +
                            "Баланс: " + user.getBallsTwo() + ", а необходимо 15 \uD83D\uDD37 балл")
                    .build();
        }
        Action actions = new Action();

        user.setBallsTwo(user.getBallsTwo() - price);

        Action act = actionsService.findByUserIdAndName(userId, user.getName());

        if (act != null){
            act.setQuantity(act.getQuantity()+1);
            actionsService.update(act);
        } else {
            actions.setUserId(userId);
            actions.setNameCompany(user.getName());
            actions.setQuantity(1);

            actionsService.update(actions);
            user.setBallsTwo(user.getBallsTwo() - price);
        }

        userService.update(user);
       return builder
               .line("Вы выкупили акцию своей компании")
               .build();
    }

    public void createMenu(){
        List<KeyboardRow> rowList = new ArrayList<>();

        KeyboardRow keyboardRow = new KeyboardRow();
        keyboardRow.add("\uD83D\uDCC9 Акции");

        KeyboardRow keyboardRow1 = new KeyboardRow();
        keyboardRow1.add("\uD83D\uDCD1 Мои акции");
        keyboardRow1.add("\uD83D\uDCBC Мой акционер");

        KeyboardRow keyboardRow2 = new KeyboardRow();
        keyboardRow2.add("\uD83D\uDD0E Поиск компаний");
        keyboardRow2.add("⬅️ Назад");

        rowList.add(keyboardRow);
        rowList.add(keyboardRow1);
        rowList.add(keyboardRow2);
        keyboard.setResizeKeyboard(true);
        keyboard.setKeyboard(rowList);
    }
}
