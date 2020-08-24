<%@ page language="java" %>
<%@ page import="java.io.File" %>
<%@ page import="java.util.*" %>
<%@ page import="java.util.Iterator" %>
<%@ page import="java.util.Map" %>
<%@ page import="java.util.Set" %>
<%@ page import="java.net.InetAddress" %>
<%@ page import="java.net.UnknownHostException" %>
<html><body><pre><%!

public String getString(String name, Object value) {
  StringBuffer sb = new StringBuffer();
  sb.append("Name  : '").append(name).append("'");
  if (value != null) {
    sb.append("\nClass : ").append(value.getClass().getName());
    sb.append("\nString: '").append(String.valueOf(value)).append("'");

  } else {
    sb.append(" (null)");
  }
  return sb.append("\n\n").toString();
}
%><%
// print Server 
try {
  String hostname = InetAddress.getLocalHost().getHostName();
  out.println("Server:" + hostname );
} catch (UnknownHostException e) { 
}
// print request URL
out.println("Request URL: " + request.getRequestURL());
// print specific environment value
Map<String, String> map = System.getenv();
String key = "APP_SERVER";
out.println(key + " = " + System.getenv(key));

/*
  StringBuffer sb = new StringBuffer();

  sb.append("Request Parameters\n******************\n");
  for (java.util.Enumeration<java.lang.String> names = request.getParameterNames(); names.hasMoreElements(); ) {
    String name = names.nextElement();
    String value = request.getParameter(name);
    sb.append(getString(name, value));
  }

  sb.append("\n\nRequest Headers\n***************\n");
  for (java.util.Enumeration<java.lang.String> names = request.getHeaderNames(); names.hasMoreElements(); ) {
    String name = names.nextElement();
    String value = request.getHeader(name);
    sb.append(getString(name, value));
  }

  out.println(sb.toString());
*/  
%></pre></body></html>
