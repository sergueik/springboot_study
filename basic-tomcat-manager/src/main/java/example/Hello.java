package example;

// Import required java libraries
import java.io.*;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

// https://docs.oracle.com/javaee/7/api/javax/servlet/http/HttpServlet.html
public class Hello extends HttpServlet {

	private String message;

	public void init() throws ServletException {
		message = "message";
	}

	// inherited from class javax.servlet.GenericServlet
	// see also:
	// https://www.cs.tut.fi/lintula/manual/java/tutorial/servlets/lifecycle/service-threads.html
	public void destroy() {
		System.err.println("Destroyed ");
	}

	public void doGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {

		response.setContentType("text/html");

		PrintWriter out = response.getWriter();
		out.println("<h1>" + message + "</h1>");
	}
}

