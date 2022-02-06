<%@ page language="java" contentType="text/html; charset=ISO-8859-1"
	pageEncoding="ISO-8859-1" isELIgnored="false"%>
<%@taglib uri="http://java.sun.com/jsp/jstl/core" prefix="c"%>
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
<style type="text/css">
a {
	text-align: center;
	font-size: 100px;
}

@import url('http://fonts.googleapis.com/css?family=Amarante');

html, body, div, span, applet, object, iframe, h1, h2, h3, h4, h5, h6, p,
	blockquote, pre, a, abbr, acronym, address, big, cite, code, del, dfn,
	em, img, ins, kbd, q, s, samp, small, strike, strong, sub, sup, tt, var,
	b, u, i, center, dl, dt, dd, ol, ul, li, fieldset, form, label, legend,
	table, caption, tbody, tfoot, thead, tr, th, td, article, aside, canvas,
	details, embed, figure, figcaption, footer, header, hgroup, menu, nav,
	output, ruby, section, summary, time, mark, audio, video {
	margin: 0;
	padding: 0;
	border: 0;
	font-size: 100%;
	font: inherit;
	vertical-align: baseline;
	outline: none;
	-webkit-font-smoothing: antialiased;
	-webkit-text-size-adjust: 100%;
	-ms-text-size-adjust: 100%;
	-webkit-box-sizing: border-box;
	-moz-box-sizing: border-box;
	box-sizing: border-box;
}

html {
	overflow-y: scroll;
}

body {
	background: #eee url('http://i.imgur.com/eeQeRmk.png');
	/* http://subtlepatterns.com/weave/ */
	font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
	font-size: 62.5%;
	line-height: 1;
	color: #585858;
	padding: 22px 10px;
	padding-bottom: 55px;
}

::selection {
	background: #5f74a0;
	color: #fff;
}

::-moz-selection {
	background: #5f74a0;
	color: #fff;
}

::-webkit-selection {
	background: #5f74a0;
	color: #fff;
}

br {
	display: block;
	line-height: 1.6em;
}

article, aside, details, figcaption, figure, footer, header, hgroup,
	menu, nav, section {
	display: block;
}

ol, ul {
	list-style: none;
}

input, textarea {
	-webkit-font-smoothing: antialiased;
	-webkit-text-size-adjust: 100%;
	-ms-text-size-adjust: 100%;
	-webkit-box-sizing: border-box;
	-moz-box-sizing: border-box;
	box-sizing: border-box;
	outline: none;
}

blockquote, q {
	quotes: none;
}

blockquote:before, blockquote:after, q:before, q:after {
	content: '';
	content: none;
}

strong, b {
	font-weight: bold;
}

table {
	border-collapse: collapse;
	border-spacing: 0;
}

img {
	border: 0;
	max-width: 100%;
}

h1 {
	font-family: 'Amarante', Tahoma, sans-serif;
	font-weight: bold;
	font-size: 3.6em;
	line-height: 1.7em;
	margin-bottom: 10px;
	text-align: center;
}

/** page structure **/
#wrapper {
	display: block;
	width: 850px;
	background: #fff;
	margin: 0 auto;
	padding: 10px 17px;
	-webkit-box-shadow: 2px 2px 3px -1px rgba(0, 0, 0, 0.35);
}

#keywords {
	margin: 0 auto;
	font-size: 1.2em;
	margin-bottom: 15px;
}

#keywords thead {
	cursor: pointer;
	background: lime;
}

#keywords thead tr th {
	font-weight: bold;
	padding: 12px 30px;
	padding-left: 42px;
}

#keywords thead tr th span {
	padding-right: 20px;
	background-repeat: no-repeat;
	background-position: 100% 100%;
}

#keywords thead tr th.headerSortUp, #keywords thead tr th.headerSortDown
	{
	background: #c9dff0;
}

#keywords thead tr th.headerSortUp span {
	background-image: url('http://i.imgur.com/SP99ZPJ.png');
}

#keywords thead tr th.headerSortDown span {
	background-image: url('http://i.imgur.com/RkA9MBo.png');
}

#keywords tbody tr {
	color: #555;
}

#keywords tbody tr td {
	text-align: center;
	padding: 15px 10px;
}

#keywords tbody tr td.lalign {
	text-align: left;
}
</style>
<meta http-equiv="Content-Type" content="text/html; charset=ISO-8859-1">
<title>Employee Home</title>
</head>
<body>
	<div
		style="text-align: center; font-size: 20px; color: blue; text-decoration: none;">
		<a href="${pageContext.request.contextPath}/getAllEmployees"> <img
			src="./images/fetch.png" alt="Fetch
			All Employees" width="250"
			height="100" border="0"></a>
	</div>
	<br>
	<div align="center">
		<form action="${pageContext.request.contextPath}/getEmployees">
			<input type="text" name="email" required=""
				placeholder="Enter Email-id"> <input type="submit"
				value="SEARCH">
		</form>
	</div>
	<br>
	<div id="wrapper">
		<h1>List of employees</h1>

		<table id="keywords" cellspacing="0" cellpadding="0">
			<thead>
				<tr>
					<th>EId</th>
					<th>First</th>
					<th>Middle</th>
					<th>Last</th>
					<th>Email</th>
				</tr>
			</thead>
			<tbody>
				<c:forEach items="${employees}" var="employees">
					<tr>
						<td class="lalign">${employees.employeeId}</td>
						<td>${employees.firstName}</td>
						<td>${employees.middleName}</td>
						<td>${employees.lastName}</td>
						<td>${employees.emailId}</td>
					</tr>
				</c:forEach>
			</tbody>
		</table>
	</div>
</body>
</html>