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

public class Agent {

	private static final String traceid = System.getProperty("traceid");

	public static void premain(String arguments,
			Instrumentation instrumentation) {

		// profile subject class
		System.err.println("Launched agent for time computation");

		new AgentBuilder.Default()
				.with(new AgentBuilder.InitializationStrategy.SelfInjection.Eager())
				.type((ElementMatchers.any()))
				.transform((builder, typeDescription, classLoader, module) -> builder
						.method(ElementMatchers.any())
						.intercept(Advice.to(TimerAdvice.class)))
				.installOn(instrumentation);

		// redefine selected subject class method
		System.out.println("Lanched agent for method redefininion");

		// This will work but the usage of ElementMatchers.nameContains(...) in the
		// invoke looks a bit weird
		new AgentBuilder.Default()

				.with(new AgentBuilder.InitializationStrategy.SelfInjection.Eager())
				.type((ElementMatchers.any()))
				.transform((builder, typeDescription, classLoader, module) -> {

					return builder
							.defineMethod("myGetName", String.class, Visibility.PUBLIC)
							.intercept(MethodDelegation.to(Agent.AddMethodStatic.class))
							.method(ElementMatchers.nameContains("getName"))
							.intercept(SuperMethodCall.INSTANCE.andThen(MethodCall
									.invoke(ElementMatchers.nameContains("myGetName"))));
				}).installOn(instrumentation);

		// redefine selected subject class method
		System.out.println("Lanched agent for method redefininion");
		// this will also work and has a cleaner signature
		// NOTE: the AddMethod cannot be static inner of Agent
		// Note the code compiles just fine either way, just does not fire
		new AgentBuilder.Default()

				.with(new AgentBuilder.InitializationStrategy.SelfInjection.Eager())
				.type((ElementMatchers.any()))
				.transform((builder, typeDescription, classLoader, module) -> {

					try {

						return builder
								.defineMethod("myGetTraceID", String.class, Visibility.PUBLIC)
								.intercept(MethodDelegation.to(AddMethod.class))
								.method(ElementMatchers.nameContains("getTraceID"))
								.intercept(SuperMethodCall.INSTANCE.andThen(
										MethodCall.invoke(Class.forName("javaagent.AddMethod")
												.getMethod("myGetTraceID"))));
					} catch (ClassNotFoundException | NoSuchMethodException e) {
						System.err
								.println("Exception in dynamic method call: " + e.toString());
						return null;
					}
				}).installOn(instrumentation);

	}

	// class for measuring execution time of all running methods
	public static class TimerAdvice {
		private final static String methodName = "processRequest";
		private final static String methodMatcher = ".*" + "( |\\.)" + methodName
				+ "\\(" + ".*";

		@Advice.OnMethodEnter
		// on entering the subject method store the current time
		static long enter(@Advice.Origin String method) throws Exception {

			if (method.matches(methodMatcher)) {
				long start = System.currentTimeMillis();
				return start;
			} else {
				return 0;
			}
		}

		@Advice.OnMethodExit
		// on exiting the subject method compute the method call execution duration
		static void exit(@Advice.Origin String method, @Advice.Enter long start)
				throws Exception {
			if (method.matches(methodMatcher)) {
				String marker = (method.matches(methodMatcher)) ? "X" : " ";
				long end = System.currentTimeMillis();
				System.err.println(method + " " + marker + " took " + (end - start)
						+ " milliseconds ");
			}
		}
	}

	// has to be placed in separate file to be discoverable
	public static class AddMethodStatic {

		private final static String name = "new_name";

		// static method to override some other method in the subject class
		public static String myGetName() throws Exception {

			System.err.println("This is new name: " + name);
			return name;

		}

	}

}
