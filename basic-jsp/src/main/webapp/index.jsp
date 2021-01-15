<%@ page language="java" %>
<%@ page import="java.io.File" %>
<%@ page import="java.util.*" %>
<%@ page import="java.util.Iterator" %>
<%@ page import="java.util.Map" %>
<%@ page import="java.util.Set" %>
<%@ page import="java.net.InetAddress" %>
<%@ page import="java.net.UnknownHostException" %>
<%@ page import="java.io.IOException" %>
<%@ page import="java.util.Properties" %>
<%@ page import="java.io.InputStream" %>
<%@ page import="java.io.FileInputStream" %>
<%@ page import="java.io.FileNotFoundException" %>
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
// TODO: define on the page, pass through taglib
String key = "APP_SERVER";
out.println(key + " = " + System.getenv(key));

Properties properties = new Properties();
InputStream input  = null;
// TODO: define on the page, pass through taglib
String propertiesFile = "application.properties";
String propertiesPath = "/opt/tomcat/conf";
String propertyName = "application.setting";
try {
  // TODO: prepend server root
  // input = new FileInputStream(propertiesFile); 
  input = Thread.currentThread().getContextClassLoader().getResourceAsStream(propertiesFile);
  if (input == null ) { 
    out.println("Failed to get properties file resource as stream: " + propertiesFile );
  } else {
    properties.load(input);
    out.println(propertyName + " = " + properties.getProperty(propertyName));
  }
} catch (IOException e) {
  out.println("Exception: " + e.toString());
} catch (Exception e) {
out.println("Exception: " + e.toString());
} finally {
  if (input != null) {
    try {
      input.close();
    } catch (IOException e) {
      out.println("Exception: " + e.toString());
    }
  }
}

try {
  // TODO: prepend server root
  input = new FileInputStream(propertiesPath + "/" + propertiesFile); 
  if (input == null ) { 
    out.println("Failed to load properties from local file: " + propertiesPath + "/" + propertiesFile);
  } else {
    properties.load(input);
    out.println(propertyName + "(from file) = " + properties.getProperty(propertyName));
  }
} catch (IOException e) {
  out.println("Exception: " + e.toString());
} catch (Exception e) {
out.println("Exception: " + e.toString());
} finally {
  if (input != null) {
    try {
      input.close();
    } catch (IOException e) {
      out.println("Exception: " + e.toString());
    }
  }
}

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
