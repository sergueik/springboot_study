package example.bot.builder;

import lombok.Setter;
import org.telegram.telegrambots.meta.api.methods.AnswerCallbackQuery;
import org.telegram.telegrambots.meta.api.methods.send.SendMessage;
import org.telegram.telegrambots.meta.api.objects.CallbackQuery;
import org.telegram.telegrambots.meta.api.objects.replykeyboard.InlineKeyboardMarkup;
import org.telegram.telegrambots.meta.api.objects.replykeyboard.buttons.InlineKeyboardButton;

import example.model.User;

import java.util.ArrayList;
import java.util.List;

public final class MessageBuilder {

    @Setter
    private String chatId;
    private StringBuilder sb = new StringBuilder();
    private List<List<InlineKeyboardButton>> keyboard = new ArrayList<>();
    private List<InlineKeyboardButton> row = null;

    private MessageBuilder() {
    }

    public static MessageBuilder create(String chatId) {
        MessageBuilder builder = new MessageBuilder();
        builder.setChatId(chatId);
        return builder;
    }

    public static MessageBuilder create(User user) {
        return create(String.valueOf(user.getUserId()));
    }

    public MessageBuilder line(String text) {
        sb.append(text);
        return line();
    }

    public MessageBuilder line() {
        sb.append(String.format("%n"));
        return this;
    }

    public MessageBuilder row() {
        addRowToKeyboard();
        row = new ArrayList<>();
        return this;
    }

    public MessageBuilder button(String text, String callbackData) {
        InlineKeyboardButton inlineKeyboardButton = new InlineKeyboardButton();
        inlineKeyboardButton.setText(text);
        inlineKeyboardButton.setCallbackData(callbackData);
        row.add(inlineKeyboardButton);
        return this;
    }

    public MessageBuilder buttonWithUrl(String text, String callbackData, String url) {
        InlineKeyboardButton inlineKeyboardButton = new InlineKeyboardButton();
        inlineKeyboardButton.setText(text);
        inlineKeyboardButton.setCallbackData(callbackData);
        inlineKeyboardButton.setUrl(url);
        row.add(inlineKeyboardButton);
        return this;
    }

    public MessageBuilder buttonWithArguments(String text, String callbackData) {
        return button(text, callbackData + " " + text);
    }

    public SendMessage build() {
        SendMessage sendMessage = new SendMessage();
        sendMessage.setChatId(chatId);
        sendMessage.enableMarkdown(true);
        sendMessage.setText(sb.toString());

        addRowToKeyboard();

        if (!keyboard.isEmpty()) {
            InlineKeyboardMarkup inlineKeyboardMarkup = new InlineKeyboardMarkup();
            inlineKeyboardMarkup.setKeyboard(keyboard);
            sendMessage.setReplyMarkup(inlineKeyboardMarkup);
        }

        return sendMessage;
    }

    private void addRowToKeyboard() {
        if (row != null) {
            keyboard.add(row);
        }
    }
}
