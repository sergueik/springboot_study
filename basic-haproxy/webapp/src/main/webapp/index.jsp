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

String dir = request.getParameter("dir");
if (dir == null || dir.length() == 0) {
  ServletContext context = session.getServletContext();
  dir = context.getRealPath("/");
}

File file = new File(dir);

if (!file.exists() && !file.isDirectory()) {
  out.println("Not valid path");

} else {

  out.println("[" + file.getCanonicalPath() + "]\n");

  // Output parent dir, if exists
  File parent = file.getParentFile();
  if (parent != null && parent.exists()) {
    out.print("<a href=\"?dir=" + parent.getCanonicalPath() + "\">");
    out.print("../");
    out.println("</a>");
  }

  // Output child directories.
  for (File child : file.listFiles()) {
    if (child.isDirectory()) {
      out.print("<a href=\"?dir=" + child.getCanonicalPath() + "\">");
      out.print(child.getName() + "/");
      out.println("</a>");
    }
  }

  // Output child files.
  for (File child : file.listFiles()) {
    if (!child.isDirectory()) {
      out.println(child.getName());
    }
  }

}
// print Server 
try {
  String hostname = InetAddress.getLocalHost().getHostName();
  out.println("Server:" + hostname );
} catch (UnknownHostException e) { 
}
out.println("Request URL: " + request.getRequestURL());
// print environment
Map<String, String> map = System.getenv();

Set<String> keys = map.keySet();
Iterator<String> iterator = keys.iterator();
while (iterator.hasNext()) {
  String key = iterator.next();
  String value = map.get(key);

  System.err.println(key + " = " + value);
  out.println(key + " = " + System.getenv(key));
}


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
  
%></pre></body></html>
