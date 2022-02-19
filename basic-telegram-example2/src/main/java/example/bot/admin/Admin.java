package example.bot.admin;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.telegram.telegrambots.meta.api.methods.send.SendMessage;
import org.telegram.telegrambots.meta.api.objects.Update;
import org.telegram.telegrambots.meta.api.objects.replykeyboard.ReplyKeyboardMarkup;
import org.telegram.telegrambots.meta.api.objects.replykeyboard.buttons.KeyboardRow;

import example.bot.builder.MessageBuilder;
import example.model.Channel;
import example.model.Payment;
import example.model.User;
import example.service.ChannelService;
import example.service.PaymentService;
import example.service.UserService;

import java.util.ArrayList;
import java.util.List;

@Component
@Slf4j
@RequiredArgsConstructor
public class Admin {

    private final UserService userService;
    private final ReplyKeyboardMarkup keyboard = new ReplyKeyboardMarkup();
    private final PaymentService paymentService;
    private final ChannelService channelService;

    public SendMessage admin(Update update){
        int userId = update.getMessage().getFrom().getId();
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));
        messageBuilder
                .line("Вы успешно авторизовались");
        createAdminMenu();

        SendMessage sendMessage = messageBuilder.build();
        sendMessage.setReplyMarkup(keyboard);

        return sendMessage;
    }

    public SendMessage checkPayments(Update update){
        int userId = update.getMessage().getFrom().getId();
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));
        List<Payment> payments = paymentService.findAllPaymentsNotSuccess();
        messageBuilder
                .line("Список всех запросов на вывод:");
        if (payments == null ||payments.isEmpty()){
           return messageBuilder
                    .line("Запросов не найдено").build();
        }
        for (Payment p : payments){
            String success = p.getSuccess() ? "Да" : "Нет";
            messageBuilder
                    .line("\n\nНомер транзакции: " + p.getId() + " | userId: " + p.getUserId() + " | Сумма: "
                            + p.getSum() + " | Время запроса: " + p.getTime() + " " + p.getPTime() + "| Выполнено: " + success);
        }
        return messageBuilder.build();
    }

    public SendMessage createPayment(Update update){
        int userId = update.getMessage().getFrom().getId();
        User user = userService.getOrCreate(userId);
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));
        messageBuilder
                .line("Введите номер транзакции")
                .row()
                .button("Отмена", "/cancelAdmin");

        user.setPositions("create_payment");
        userService.update(user);

        return messageBuilder.build();
    }

    public SendMessage createPaymentImpl(Update update){
        int userId = update.getMessage().getFrom().getId();
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));
        Payment p = null;
        try {
            p = paymentService.findById(Integer.parseInt(update.getMessage().getText()));
        }catch (Exception e){
            return messageBuilder.line("Вы ввели не число").build();
        }
        User user = userService.getOrCreate(p.getUserId());
        user.setPositions("back");
        userService.update(user);

        messageBuilder
                .line("Номер транзакции: " + p.getId() + " | userId: " + p.getUserId() + " | Сумма: "
                        + p.getSum() + " рублей | Время запроса: " + p.getTime() + " | Номер: " + user.getQiwi())
                .row()
                .button("Оплачено", "/success_" + p.getId())
                .row()
                .button("Отменено", "/notSuc_" + p.getId());
        return messageBuilder.build();
    }

    public List<SendMessage> success(Update update){
        int userId = update.getCallbackQuery().getFrom().getId();
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));
        int transId = Integer.parseInt(update.getCallbackQuery().getData().split("_")[1]);
        Payment p = paymentService.findById(transId);
        List<SendMessage> messages = new ArrayList<>();
        int paymenterId = p.getUserId();

        p.setSuccess(true);
        paymentService.update(p);
        messages.add(messageBuilder.line("Оплата выполнена").build());
        messageBuilder = MessageBuilder.create(String.valueOf(paymenterId));
        messages.add(messageBuilder.line("Вам выплачено " + p.getSum()).build());

        return messages;
    }

    public List<SendMessage> notSuccess(Update update){
        int userId = update.getCallbackQuery().getFrom().getId();
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));
        int transId = Integer.parseInt(update.getCallbackQuery().getData().split("_")[1]);
        Payment p = paymentService.findById(transId);
        List<SendMessage> messages = new ArrayList<>();
        int paymenterId = p.getUserId();

        p.setSuccess(true);
        paymentService.update(p);
        messages.add(messageBuilder.line("Оплата отменена").build());
        messageBuilder = MessageBuilder.create(String.valueOf(paymenterId));
        messages.add(messageBuilder.line("Ваша выплата отменена! обратитесь к " +
                "админу через обратную связь, указав номер транзакции\n" +
                "Номер транзакции: " + p.getId()).build());

        return messages;
    }

    public SendMessage messageToAll(Update update){
        int userId = update.getMessage().getFrom().getId();
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));
        User user = userService.getOrCreate(userId);
        user.setPositions("messageToAll");
        userService.update(user);
        return messageBuilder
                .line("Введите сообщение для всех")
                .row()
                .button("Отмена", "/cancelAdmin")
                .build();
    }

    public List<SendMessage> messageToAllImpl(Update update){
        int userId = update.getMessage().getFrom().getId();
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));
        String text = update.getMessage().getText();

        User user = userService.getOrCreate(userId);
        List<SendMessage> messages = new ArrayList<>();
        List<User> users = userService.findAll();

        user.setPositions("back");
        userService.update(user);

        messages.add(messageBuilder.line("Сообщения отправлены!").build());
        for (User u : users) {
            MessageBuilder messageBuilder1 = MessageBuilder.create(String.valueOf(u.getUserId()));
            messageBuilder1
                    .line("Сообщение от админа:\n")
                    .line(text);
            messages.add(messageBuilder1.build());
        }
        return messages;
    }

    public SendMessage addChannel(Update update){
        int userId = update.getMessage().getFrom().getId();
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));

        User user = userService.getOrCreate(userId);

        user.setPositions("addChannel");
        userService.update(user);

        return messageBuilder
                .line("Введите данные: @id url")
                .row()
                .button("Отмена", "/cancelAdmin")
                .build();
    }

    public SendMessage deleteChannel(Update update){
        int userId = update.getMessage().getFrom().getId();
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));

        User user = userService.getOrCreate(userId);

        user.setPositions("deleteChannel");
        userService.update(user);

        return messageBuilder
                .line("Введите данные: @id")
                .row()
                .button("Отмена", "/cancelAdmin")
                .build();
    }

    public SendMessage addChannelImpl(Update update){
        int userId = update.getMessage().getFrom().getId();
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));
        String[] data = update.getMessage().getText().split(" ");

        if (data.length != 2){
            return messageBuilder.line("Неверные данные").build();
        }

        Channel channel = new Channel();
        channel.setId(data[0]);
        channel.setUrl(data[1]);
        channelService.update(channel);

        User user = userService.getOrCreate(userId);
        user.setPositions("back");
        userService.update(user);
        log.info("Канал добавлен {}" + channel.getId());

        return messageBuilder.line("Канал добавлен").build();
    }

    public SendMessage deleteChannelImpl(Update update){
        int userId = update.getMessage().getFrom().getId();
        MessageBuilder messageBuilder = MessageBuilder.create(String.valueOf(userId));
        String channelId = update.getMessage().getText();

        channelService.delete(channelId);

        User user = userService.getOrCreate(userId);
        user.setPositions("back");
        userService.update(user);
        log.info("Канал удален {}" + channelId);

        return messageBuilder.line("Канал удален").build();
    }

    public void createAdminMenu(){
        List<KeyboardRow> rowList = new ArrayList<>();
        KeyboardRow keyboardRow = new KeyboardRow();
        keyboardRow.add("Получить список юзеров ожидающих оплату");
        keyboardRow.add("Выполнить оплату пользователю");

        KeyboardRow keyboardRow2 = new KeyboardRow();
        keyboardRow2.add("Удалить канал");
        keyboardRow2.add("Добавить канал");

        KeyboardRow keyboardRow1 = new KeyboardRow();
        keyboardRow1.add("Сообщение всем");
        keyboardRow1.add("⬅️ Назад");

        rowList.add(keyboardRow);
        rowList.add(keyboardRow2);
        rowList.add(keyboardRow1);
        keyboard.setResizeKeyboard(true);
        keyboard.setKeyboard(rowList);
    }
}
