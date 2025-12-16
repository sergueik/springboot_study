package serilogj.serilogj.core.enrichers;

import serilogj.serilogj.context.LogContext;
import serilogj.serilogj.core.ILogEventEnricher;
import serilogj.serilogj.core.ILogEventPropertyFactory;
import serilogj.serilogj.events.LogEvent;

public class LogContextEnricher implements ILogEventEnricher {
	@Override
	public void enrich(LogEvent logEvent, ILogEventPropertyFactory propertyFactory) {
		for(ILogEventEnricher enricher : LogContext.getEnrichers()) {
			enricher.enrich(logEvent, propertyFactory);
		}
	}
}
