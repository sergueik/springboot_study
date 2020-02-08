package javaagent;

import net.bytebuddy.asm.Advice;

public class TimerAdvice {
	/**
	 * From this enter method we start the timer and pass that value to exit method
	 * and the we getting the time speed for each method
	 */
	private final static String methodName = "processRequest";
	private final static String methodMatcher = ".*" + "( |\\.)" + methodName + "\\(" + ".*";

	@Advice.OnMethodEnter
	static long enter(@Advice.Origin String method) throws Exception {

		if (method.matches(methodMatcher)) {
			long start = System.currentTimeMillis();
			return start;
		} else
			return 0;
	}

	@Advice.OnMethodExit
	static void exit(@Advice.Origin String method, @Advice.Enter long start) throws Exception {

		if (method.matches(methodMatcher)) {
			String marker = (method.matches(methodMatcher)) ? "X" : " ";
			long end = System.currentTimeMillis();
			System.out.println(method + " " + marker + " took " + (end - start) + " milliseconds ");
		}
	}

}
