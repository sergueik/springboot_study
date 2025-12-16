package serilogj.serilogj.parameters;

import java.util.ArrayList;

import serilogj.serilogj.events.LogEventProperty;
import serilogj.serilogj.events.MessageTemplate;

public class MessageTemplateProcessorResult {
	public MessageTemplate template;
	public ArrayList<LogEventProperty> properties;
}
