<!DOCTYPE html>
<%@ taglib prefix="sf" uri="http://www.springframework.org/tags/form" %>
<%@ taglib prefix="s" uri="http://www.springframework.org/tags" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="fmt" uri="http://java.sun.com/jsp/jstl/fmt" %>
<%@ page contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@ page import="example.model.Dummy" %>
<!-- NOTE: a typo in the package name leads to a vague runtime error: 
  An error occurred at line: ... in the generated java file: [...\hello_jsp.java]
  Only a type can be imported. example.mode.Dummy resolves to a package
  An error occurred at line: [...] in the jsp file [/WEB-INF/templates/hello.jsp]
  Dummy cannot be resolved
-->
<html lang="en">
<head>
  <meta charset="UTF-8">
  <title>Hello</title>
    <meta charset="UTF-8"/>
    <meta name="viewport" content="width=device-width, initial-scale=1" />
    <link href="https://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap.min.css" rel="stylesheet"/>
    <link href="https://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap-theme.min.css" rel="stylesheet"/>	
    <link href="https://maxcdn.bootstrapcdn.com/font-awesome/4.5.0/css/font-awesome.min.css" rel="stylesheet"/>	
    <link href="<s:url value="/resources/css/style.css"/>" rel="stylesheet"/>
</head>
<body>
  <div id="${id}">
    Hello ${name}
  </div>
  <div>
  <%
    String firstName = Dummy.getFirstName();
    String lastName = Dummy.getLastName();
    out.print("Name: " + firstName + "</br>");
    out.print("LastName: " + lastName);
  %>
  </div>
</body>
</html>