package bot;

import org.telegram.telegrambots.meta.api.methods.send.SendMessage;
import org.telegram.telegrambots.meta.exceptions.TelegramApiException;

public enum BotState {

	Start {
		@Override
		public void enter(BotContext context) {
			sendMessage(context, "Привет :)");
		}

		@Override
		public BotState nextState() {
			return EnterPhone;
		}
	},

	EnterPhone {
		@Override
		public void enter(BotContext context) {
			sendMessage(context, "Введите, пожалуйста, свой телефон");
		}

		@Override
		public void handleInput(BotContext context) {
			context.getUser().setPhone(context.getInput());
		}

		@Override
		public BotState nextState() {
			return EnterEmail;
		}

	},
	EnterEmail {
		private BotState next;

		@Override
		public void enter(BotContext context) {
			sendMessage(context, "Введите, пожалуйста, свой норм");
		}

		@Override
		public void handleInput(BotContext context) {
			String email = context.getInput();

			if (Utils.isValidEmailAddress(email)) {
				context.getUser().setEmail(context.getInput());
				next = Approved;
			} else {
				sendMessage(context, "Некорректная почта! Введите заново");
				next = EnterEmail;
			}
		}

		@Override
		public BotState nextState() {
			return next;
		}

	},
	Approved(false) {
		public void enter(BotContext context) {
			sendMessage(context, "Спасибо за предоставленные данные!");
		}

		public BotState nextState() {
			return Start;
		}
	};

	private static BotState[] states;
	private final boolean inputNeeded;

	BotState() {
		this.inputNeeded = true;
	}

	BotState(boolean inputNeeded) {
		this.inputNeeded = inputNeeded;
	}

	public static BotState getInitialState() {
		return byId(0);
	}

	public static BotState byId(int id) {
		if (states == null) {
			states = BotState.values();
		}
		return states[id];
	}

	protected void sendMessage(BotContext context, String text) {
		SendMessage sendMessage = new SendMessage()
				.setChatId(context.getUser().getChatId()).setText(text);

		try {
			context.getBot().execute(sendMessage);
		} catch (TelegramApiException e) {
			e.printStackTrace();
		}
	}

	public boolean isInputNeeded() {
		return inputNeeded;
	}

	public void handleInput(BotContext context) {

	}

	public abstract void enter(BotContext context);

	public abstract BotState nextState();
}
