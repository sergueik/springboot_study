package example.bot;

import com.codepoetics.protonpack.collectors.CompletableFutures;
import com.microsoft.bot.builder.ActivityHandler;
import com.microsoft.bot.builder.MessageFactory;
import com.microsoft.bot.builder.TurnContext;
import com.microsoft.bot.schema.ChannelAccount;
import org.apache.commons.lang3.StringUtils;

import java.util.List;
import java.util.concurrent.CompletableFuture;

public class ActivityHandlerExample extends ActivityHandler {

	@Override
	protected CompletableFuture<Void> onMessageActivity(TurnContext turnContext) {

		return turnContext
				.sendActivity(
						MessageFactory.text("Echo: " + turnContext.getActivity().getText()))
				.thenApply(sendResult -> null);
	}

	@Override
	protected CompletableFuture<Void> onMembersAdded(
			List<ChannelAccount> membersAdded, TurnContext turnContext) {
		return membersAdded.stream()
				.filter(member -> !StringUtils.equals(member.getId(),
						turnContext.getActivity().getRecipient().getId()))
				.map(channel -> turnContext
						.sendActivity(MessageFactory.text("Hello and welcome!")))
				.collect(CompletableFutures.toFutureList())
				.thenApply(resourceResponses -> null);
	}
}
