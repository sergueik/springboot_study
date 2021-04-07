package javaagent;

import net.bytebuddy.agent.builder.AgentBuilder;
import net.bytebuddy.asm.Advice;
import net.bytebuddy.matcher.ElementMatchers;

import java.lang.instrument.Instrumentation;

import net.bytebuddy.asm.Advice;

 
public class Agent {

	public static void premain(String arguments, Instrumentation instrumentation) {

		System.err.println("Launched agent for time computation");

		new AgentBuilder.Default().with(new AgentBuilder.InitializationStrategy.SelfInjection.Eager())
				.type((ElementMatchers.any())).transform((builder, typeDescription, classLoader, module) -> builder
						.method(ElementMatchers.any()).intercept(Advice.to(TimerAdvice.class)))
				.installOn(instrumentation);

	}
	// class for measuring execution time of all running methods
	public static class TimerAdvice {
		/**
		 * From this enter method we start the timer and pass that value to exit method
		 * and the we getting the time speed for each method
		 */
		private final static String methodName = "processRequest";
		private final static String methodMatcher = ".*" + "( |\\.)" + methodName + "\\(" + ".*";

		@Advice.OnMethodEnter
		static long enter(@Advice.Origin String method) throws Exception {

			if (method.matches(methodMatcher)) {
				// System.err.println("enter processing method: " + method);
				long start = System.currentTimeMillis();
				return start;
			} else {
				// System.err.println("enter ignoring method: " + method);
				return 0;
			}
		}

		@Advice.OnMethodExit
		static void exit(@Advice.Origin String method, @Advice.Enter long start) throws Exception {
			if (method.matches(methodMatcher)) {
				String marker = (method.matches(methodMatcher)) ? "X" : " ";
				long end = System.currentTimeMillis();
				System.err.println(method + " " + marker + " took " + (end - start) + " milliseconds ");
			}
		}
	}

}
