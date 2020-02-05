package javaagent;

import net.bytebuddy.asm.Advice;

public class AllMethod {

	@Advice.OnMethodExit
	static void getAllMethods(@Advice.Origin String method) throws Exception {

		System.out.println(method);
	}

}