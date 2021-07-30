<%@ page language="java" %>
<%@ page import="java.io.File" %>
<%@ page import="java.util.*" %>
<%@ page import="java.util.Iterator" %>

<%@ page import="java.util.ArrayList" %>
<!-- The import java.util.Arrays cannot be resolved -->
<%@ page import="java.util.Map" %>
<%@ page import="java.util.Set" %>
<%@ page import="java.net.InetAddress" %>
<%@ page import="javax.servlet.ServletContext" %>
<%@ page import="java.net.UnknownHostException" %>
<%@ page import="java.io.IOException" %>
<%@ page import="java.io.*" %>
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
try {
  String hostname = InetAddress.getLocalHost().getHostName();
  out.println("Server:" + hostname );
} catch (UnknownHostException e) {
}
out.println("Request URL: " + request.getRequestURL());
Map<String, String> map = System.getenv();
// TODO: define some paramters through the page through taglib
// see also https://websphereportaltechies.wordpress.com/2013/11/26/reading-properties-file-in-jsp-using-jstl-taglib/
// List<String> keys = Arrays.asList("APP_SERVER", "CATALINA_HOME", "WINDIR", "CLASSPATH");

List<String> keys = new ArrayList<String>();
keys.add("APP_SERVER");
keys.add("CATALINA_HOME");
keys.add("WINDIR");
keys.add("CLASSPATH");
// NOTE: cannot use any Java 8 semantics e.g.

// foreach (key: keys) leads to
// Syntax error, insert &quot;;&quot; to complete Statement
// Lambda expressions cannot be on present on the jsp page even commented
// see also https://stackoverflow.com/questions/40035001/lambda-expressions-in-jsp-files-will-not-compile
// for web.xml setting
Iterator<String> keysIterator = keys.iterator();
String key = null;
out.println("Environment: ");
while (keysIterator.hasNext()) {
  key = keysIterator.next();
  out.println(key + " = " + System.getenv(key));
}
Properties properties = new Properties();
String propertiesFile = "application.properties";
String propertyName = "application.setting";
propertyName = "application.value";
InputStream input = null;
try {
  input = Thread.currentThread().getContextClassLoader().getResourceAsStream(propertiesFile);
  if (input == null ) {
    out.println("Failed to get properties file: " + propertiesFile );
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

String propertiesPath = getServletContext().getRealPath(propertiesFile);
try {
  input = new FileInputStream(propertiesPath);
  if (input == null ) {
    out.println("Failed to load properties file: " + propertiesPath );
  } else {
    properties.load(input);
    out.println("Property file \"" + propertiesPath  + "\" " + propertyName + " = " + properties.getProperty(propertyName));
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
