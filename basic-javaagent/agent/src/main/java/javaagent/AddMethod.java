package javaagent;

import net.bytebuddy.ByteBuddy;
import java.lang.ClassNotFoundException;
import java.lang.NoSuchMethodException;
import net.bytebuddy.agent.builder.AgentBuilder;
import net.bytebuddy.asm.Advice;
import net.bytebuddy.matcher.ElementMatchers;

import java.lang.instrument.Instrumentation;

import net.bytebuddy.asm.Advice;
import net.bytebuddy.agent.builder.AgentBuilder;
import net.bytebuddy.description.modifier.Visibility;
import net.bytebuddy.implementation.MethodCall;
import net.bytebuddy.implementation.MethodDelegation;
import net.bytebuddy.implementation.SuperMethodCall;
import net.bytebuddy.matcher.ElementMatchers;

public class AddMethod {

	private final static String traceid = "new_value";

	// static method to override some other method in the subject class
	public static String myGetTraceID() throws Exception {

		System.err.println("This is new value: " + traceid);
		return traceid;

	}
}
