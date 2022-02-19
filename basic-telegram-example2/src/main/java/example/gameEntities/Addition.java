package example.gameEntities;

import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.telegram.telegrambots.meta.api.methods.send.SendMessage;
import org.telegram.telegrambots.meta.api.objects.Update;
import org.telegram.telegrambots.meta.api.objects.replykeyboard.ReplyKeyboardMarkup;
import org.telegram.telegrambots.meta.api.objects.replykeyboard.buttons.KeyboardRow;

import example.bot.builder.MessageBuilder;
import example.model.User;
import example.service.UserService;

import java.text.SimpleDateFormat;
import java.time.DayOfWeek;
import java.time.Duration;
import java.time.LocalDate;
import java.time.temporal.TemporalAdjusters;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

@Component
@RequiredArgsConstructor
public class Addition {

    private final UserService userService;
    private final ReplyKeyboardMarkup keyboard = new ReplyKeyboardMarkup();

    @Value("${bot.admin}")
    private String botAdmin;

    @Value("${bot.chat}")
    private String chat;

    public SendMessage add(Update update){
        int userId = update.getMessage().getFrom().getId();
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));

        createMenu();
        messageBuilder
                .line("\uD83D\uDCA1 Дополнительно");

        SendMessage sendMessage = messageBuilder.build();
        sendMessage.setReplyMarkup(keyboard);

        return sendMessage;
    }

    public void createMenu(){
        List<KeyboardRow> rowList = new ArrayList<>();

        KeyboardRow keyboardRow = new KeyboardRow();
        keyboardRow.add("\uD83D\uDC65 ТОП по рефералам");
        keyboardRow.add("\uD83D\uDCDD Название");

        KeyboardRow keyboardRow1 = new KeyboardRow();
        keyboardRow1.add("❓ Помощь");
        keyboardRow1.add("\uD83D\uDCAC Сообщество");

        KeyboardRow keyboardRow2 = new KeyboardRow();
        keyboardRow2.add("⬅️ Назад");

        rowList.add(keyboardRow);
        rowList.add(keyboardRow1);
        rowList.add(keyboardRow2);
        keyboard.setResizeKeyboard(true);
        keyboard.setKeyboard(rowList);
    }

    public void createCancelMenu(){
        List<KeyboardRow> rowList = new ArrayList<>();

        KeyboardRow keyboardRow2 = new KeyboardRow();
        keyboardRow2.add("⬅️ Назад");

        rowList.add(keyboardRow2);
        keyboard.setResizeKeyboard(true);
        keyboard.setKeyboard(rowList);
    }

    public SendMessage addName(Update update){
        int userId = update.getMessage().getFrom().getId();
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));

        return messageBuilder
                .line("\uD83D\uDCDD Название компании\n" +
                        "   \n" +
                        "Ваша компания пока никак не назывется. Вы можете придумать для нее абсолютно любое название состоящее из латинских букв и цифр.\n" +
                        "Название для компании - это его лицо. По нему Вас смогут находить другие компании и именно оно в первую очередь будет отображаться при взаимоотношениях с другими компаниями.\n" +
                        "\n" +
                        "✅ Полный список разрешенных символов:\n" +
                        "Латинский алфавит:\n" +
                        "A-Z, a-z\n" +
                        "Цифры:\n" +
                        "0-9\n" +
                        "Длина названия компании должна быть больше 4-х символов и меньше 20-ти.\n" +
                        "\n" +
                        "❗️ Запрещено в названии располагать рекламу и использовать нецезурные слова!")
                .row()
                .button("\uD83D\uDCDD Изменить название", "/change_companyName")
                .build();
    }

    public SendMessage changeName(Update update){
        int userId = update.getCallbackQuery().getFrom().getId();
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));
        User user = userService.getOrCreate(userId);

        createCancelMenu();

        user.setPositions("change_name");
        userService.update(user);
        SendMessage sendMessage = messageBuilder
                .line("\uD83D\uDCDD Изменить название\n" +
                        "  \n" +
                        "Введите название для Вашей компании.")
                .build();

        sendMessage.setReplyMarkup(keyboard);
        return sendMessage;
    }

    public SendMessage changeNameImpl(Update update){
        int userId = update.getMessage().getFrom().getId();
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));
        String name = update.getMessage().getText();

        User user = userService.getOrCreate(userId);

        if ("back".equalsIgnoreCase(user.getPositions())){
            return new SendMessage();
        }

        if (checkName(name)){
            return messageBuilder
                    .line("Введено неправильно имя!")
                    .build();
        }

        user.setPositions("back");
        userService.update(user);
        user.setName(name);
        userService.update(user);
        SendMessage sendMessage = messageBuilder
                .line("Теперь имя вашей компании: " + user.getName())
                .build();
        sendMessage.setReplyMarkup(keyboard);
        return sendMessage;
    }

    public SendMessage help(Update update){
        int userId = update.getMessage().getFrom().getId();
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));

        return messageBuilder
                .line("❓ Помощь\n" +
                        "  \n" +
                        "Здесь Вы найдете подсказки по игре.")
                .row()
                .button("✉️ Обратная связь", "/report")
                .row()
                .button("\uD83E\uDDD1\u200D\uD83C\uDF93 Пройти обучение заново", "/re_train")
                .row()
                .button("❔ FAQ по игре ❔", "/faq")
                .build();
    }

    public SendMessage report(Update update){
        int userId = update.getCallbackQuery().getFrom().getId();
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));
        User user = userService.getOrCreate(userId);

        messageBuilder
                .line("✉️Обратная связь\n" +
                        "  \n" +
                        "Если у Вас есть какие-то вопросы, идеи или пожелания, напишите нам. Мы обязательно ответим.");

        user.setPositions("report");
        userService.update(user);

        return messageBuilder
                .build();
    }

    public SendMessage answer(Update update){
        String userId = update.getCallbackQuery().getData().split("_")[1];
        MessageBuilder messageBuilder = MessageBuilder.create(botAdmin)
                .line("Введите сообщение: ");
        User user = userService.getOrCreate(Integer.parseInt(botAdmin));
        user.setPositions("answer_" + userId);
        userService.update(user);

        return messageBuilder
                .build();
    }

    public List<SendMessage> answerImpl(Update update){
        User admin = userService.getOrCreate(Integer.parseInt(botAdmin));
        String userId = admin.getPositions().split("_")[1];
        String text = update.getMessage().getText();
        List<SendMessage> messages = new ArrayList<>();

        MessageBuilder messageBuilder = MessageBuilder.create(userId);
        messageBuilder
                .line("Сообщение от админа: \n")
                .line(text);
        messages.add(messageBuilder.build());

        messageBuilder = MessageBuilder.create(botAdmin);
        messageBuilder
                .line("Сообщение отправлено!");
        messages.add(messageBuilder.build());

        admin.setPositions("back");
        userService.update(admin);

        return messages;
    }

    public SendMessage reportImpl(Update update){
        int userId = update.getMessage().getFrom().getId();
        String text = update.getMessage().getText();
        String date = new SimpleDateFormat("dd.MM.yyyy").format(new Date());
        String time = new SimpleDateFormat("H:m").format(new Date());

        User user = userService.getOrCreate(userId);
        user.setPositions("back");
        userService.update(user);

        MessageBuilder messageBuilder = MessageBuilder.create(botAdmin);
        return messageBuilder
                .line("Отправлено от: " + userId + "\n")
                .line("Дата: " + date + " | Время: " + time + "\n")
                .line("Сообщение: \n")
                .line(text)
                .row()
                .button("Ответить", "/answer_" + userId)
                .build();
    }
    // ответ юзер после отправки репорта
    public SendMessage reportImplToUser(Update update){
        int userId = update.getMessage().getFrom().getId();
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));
        return messageBuilder
                .line("Сообщение отправлено")
                .build();
    }

    public boolean checkName(String name){
        Pattern p = Pattern.compile("[^A-Za-z0-9 ]", Pattern.CASE_INSENSITIVE);
        Matcher m = p.matcher(name);
        return m.find();
    }

    public SendMessage faq(Update update){
        int userId = update.getCallbackQuery().getFrom().getId();
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));
        User user = userService.getOrCreate(userId);

        messageBuilder
                .line("❔ Обучающий мануал по игре ❔\n" +
                        "  \n" +
                        "Здесь Вы найдете ответы на самые часто задаваемые вопросы. Просто выберите одну из интересующих Вас тем.")
                .row()
                .button("Баллы \uD83D\uDD36\uD83D\uDD37", "/faq_balls")
                .row()
                .button("Вывод средств", "/faq_with")
                .row()
                .button("Акции \uD83D\uDCC9\uD83D\uDCC8", "/faq_actions")
                .row()
                .button("\uD83D\uDC65 Рефералы", "/faq_refs");

        return messageBuilder
                .build();
    }

    public SendMessage faqBalls(Update update){
        int userId = update.getCallbackQuery().getFrom().getId();
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));
        return messageBuilder
                .line("Баллы \uD83D\uDD36\uD83D\uDD37\n" +
                        "\n" +
                        "Баллы в игре нужны для покупки акций в игре. Вы можете получить их одним из следующих способов:\n" +
                        "1. Пополняя баланс в игре (за каждые 200 купленных коинов Вы получите 1 балл).\n" +
                        "2. За счет пополнения баланса рефералами.\n" +
                        "3. За счет выкупа у Вас \uD83D\uDCC9 Акций Вашими рефералами (15 \uD83D\uDD37 баллов).\n" +
                        "4. За счет продажи своих и чужих \uD83D\uDCC8 Акций PRO.\n" +
                        "5. Выиграть в Беспроигрышной лотерее (\uD83C\uDFB2 Игры > \uD83D\uDCB0 Джекпот).\n" +
                        "6. Занять призовое место в \uD83D\uDC65ТОПЕ по рефералам. (\uD83D\uDCA1Дополнительно > \uD83D\uDC65ТОП по рефералам).")
                .build();
    }

    public SendMessage faqWith(Update update){
        int userId = update.getCallbackQuery().getFrom().getId();
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));
        return messageBuilder
                .line("Вывод средств\n" +
                        "\n" +
                        "Минимальный вывод средств из игры:\n" +
                        " 1000 \uD83D\uDCB0 Gold, что равно 600 руб.\n" +
                        " 1000 ⚡️ ECrypt, что равно 0.002 BTC\n" +
                        "\n" +
                        "Вы можете вывести баланс на:\n" +
                        " Qiwi\n" +
                        " Яндекс.Деньги\n" +
                        " Карты Visa и Mastercard (Россия и Украина)")
                .build();
    }

    public SendMessage faqActions(Update update){
        int userId = update.getCallbackQuery().getFrom().getId();
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));
        return messageBuilder
                .line("Акции \uD83D\uDCC9\uD83D\uDCC8 \n" +
                        "  \n" +
                        "Существует 2 вида акций:\n" +
                        "1. Обычные акции \uD83D\uDCC9\n" +
                        "Вы можете купить акции компаний-новичков и получать 30% от всех собираемых ресурсов компании, а также % от пополнения внутриигрового счета. Их можно приобрести в разделе «\uD83C\uDFEB Биржа», далее «\uD83D\uDCC9 Акции».  Особенность обычных акций в том, что компания, акциями которой Вы владеете, может выкупить их в любой момент за 15 баллов\uD83D\uDD37. Данная сумма будет зачислена на Ваш баланс. \n" +
                        "2. PRO акции \uD83D\uDCC8 \n" +
                        "Вы можете купить акции компаний-PRO игроков и получать указанный владельцем компании процент от всех собираемых ресурсов, а также % от пополнения счета в виде баллов. PRO акции можно приобрести в разделе «\uD83C\uDFEBБиржа»," +
                        " далее «\uD83D\uDCC9Акции PRO». Для того, чтобы стать PRO-акционером, Вам необходимо выкупить свои акции в разделе «\uD83C\uDFEBБиржа», далее «\uD83D\uDCBCМой акционер».  После того, как Вы стали PRO акционером, у Вас появится возможность выставить от 7% до 49% своих акций PRO на биржу. Для этого нужно пройти в раздел «\uD83C\uDFEBБиржа», далее «\uD83D\uDCBCМой акционер».")
                .build();
    }

    public SendMessage faqRefs(Update update){
        int userId = update.getCallbackQuery().getFrom().getId();
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));
        return messageBuilder
                .line("Рефералы \uD83D\uDC65 \n" +
                        "  \n" +
                        "Рефералы – это компании людей, чьими акциями Вы владеете на 30%. То есть, Вы получаете долю в процентном соотношении с деятельности вашего реферала (сбор ресурсов, покупка баллов). Реферальная система состоит из 3 уровней. Вам также будут зачисляться % " +
                        "от пополнения счета рефералов Ваших рефералов (30% -> 10% -> 3%). Для того, чтобы привлечь людей в вашу реферальную систему, Вам необходимо разослать индивидуальную реферальную ссылку друзьям и знакомым. Её можно получить в разделе «\uD83C\uDFED Моя компания», далее «\uD83D\uDC65 Рефералы».")
                .build();
    }

    public SendMessage community(Update update) {
        int userId = update.getMessage().getFrom().getId();
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));
        SendMessage sendMessage =  messageBuilder
                .line("\uD83D\uDCAC Сообщество\n" +
                        "  \n" +
                        "Здесь Вы можете найти официальные чаты, посвященные проекту.\n" +
                        " \uD83C\uDDF7\uD83C\uDDFA<a href=\"" + chat + "\">Подписаться</a>\uD83C\uDDF7\uD83C\uDDFA\n" +
                        " \n" +
                        "Если Вы ранее не вступали в чат, Вы получите награду.")
                .build();
        sendMessage.enableHtml(true);
        sendMessage.disableWebPagePreview();
        return sendMessage;
    }

    public SendMessage topRefs(Update update){
        int userId = update.getMessage().getFrom().getId();
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));
        User user = userService.getOrCreate(userId);

        LocalDate currentDate = LocalDate.now();
        LocalDate nextMonday = currentDate.with(TemporalAdjusters.next(DayOfWeek.MONDAY));

        Duration between = Duration.between(currentDate.atStartOfDay(), nextMonday.atStartOfDay());

        List<User> usrs = userService.findUsersWithRefer();
        List<User> users = new ArrayList<>();
        for (User usr : usrs){
            users.add(userService.getOrCreate(usr.getReferId()));
        }
        users.sort(Comparator.comparing(User::getCountReferals));

        String[] arr = new String[]{"1️⃣", "2️⃣", "3️⃣", "4️⃣","5️⃣","6️⃣", "7️⃣","8️⃣","9️⃣","\uD83D\uDD1F"};

        messageBuilder
                .line("\uD83D\uDC65 ТОП по рефералам\n" +
                        "\n" +
                        " Перед Вами ТОП игроков по приглашению рефералов. \n" +
                        " Вы пригласили: "+ user.getCountReferals() +" чел.\n" +
                        " \n" +
                        "\uD83D\uDD25ТОП\uD83D\uDD1F участников:")
                .line();

        for (int i = 0; i < 10;i++){
            if (users.size()<i+1){
                return messageBuilder
                        .build();
            }
            messageBuilder
                   .line(arr[i] + user.getName() + " - пригласил " + users.get(i).getCountReferals() + " чел.");
        }
        return messageBuilder
                .build();
    }
}
