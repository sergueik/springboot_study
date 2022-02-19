package bot;

import model.User;

public class BotContext {
	private final ChatBot bot;
	private final User user;
	private final String input;

	public static BotContext of(ChatBot bot, User user, String text) {
		return new BotContext(bot, user, text);
	}

	public ChatBot getBot() {
		return bot;
	}

	public User getUser() {
		return user;
	}

	public String getInput() {
		return input;
	}

	public BotContext(ChatBot bot, User user, String input) {
		this.bot = bot;
		this.user = user;
		this.input = input;
	}
}
