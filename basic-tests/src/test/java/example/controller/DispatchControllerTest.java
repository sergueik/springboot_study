package example.controller;
/**
 * Copyright 2023 Serguei Kouzmine
 */

import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.Collection;
import java.util.Locale;

import javax.servlet.ServletOutputStream;
import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletResponse;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.ApplicationContext;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import example.service.SimpleService;

@SpringBootTest
public class DispatchControllerTest {
	@Autowired
	ApplicationContext context;
	// @Autowired
	@InjectMocks
	// NOTE: incommenting leads to massive exceptio stack and VM crash
	DispatchController controller;

	@MockBean
	SimpleService service;

	private static String name = "name";
	final static String body = "Hello";
	// NOTE: have to match signature generic modifier
	// type mismatch: cannot convert from ResponseEntity<capture#1-of ?> to
	// ResponseEntity<Object>
	// private ResponseEntity<Object> responseEntity = null;
	private ResponseEntity<String> responseEntity = null;

	// https://docs.oracle.com/javaee/6/api/javax/servlet/http/HttpServletResponse.html
	// https://docs.oracle.com/javaee%2F6%2Fapi%2F%2F/javax/servlet/ServletResponse.html
	// see also:
	// https://stackoverflow.com/questions/51739333/how-to-create-httpservletresponse-for-unit-tests-in-spring
	@MockBean
	private HttpServletResponse response;

	// https://docs.oracle.com/javaee/7/api/javax/servlet/http/HttpServletresponseImpl.html
	private HttpServletResponseImpl responseImpl;
	// Provides a convenient implementation of the HttpServletResponse interface

	private String responseString;

	@BeforeEach
	public void setup() {

		when(service.hello()).thenReturn(body);
		when(service.hello(any(String.class))).thenReturn(body);
		Mockito.doThrow(new IllegalStateException("illegal state")).when(service)
				.hello("illegal state");

		Mockito.doThrow(new NullPointerException("null pointer")).when(service)
				.hello("null pointer");
		controller = context.getBean(DispatchController.class);
		responseImpl = new HttpServletResponseImpl();
	}

	@Test
	public void test1() {
		responseEntity = controller.callService(name);
		assertThat(responseEntity, notNullValue());
		assertThat(responseEntity.getStatusCode(), is(HttpStatus.OK));
		String respose = responseEntity.getBody();
		assertThat(respose, notNullValue());
		assertThat(respose.isEmpty(), is(false));

	}

	@Test
	public void test2() {
		responseEntity = controller.callService("illegal state");
		assertThat(responseEntity, notNullValue());
		assertThat(responseEntity.getBody(), nullValue());
		assertThat(responseEntity.getStatusCode(),
				is(HttpStatus.INTERNAL_SERVER_ERROR));

	}

	@Test
	public void test3() {
		responseEntity = controller.callService("null pointer");
		assertThat(responseEntity, notNullValue());
		assertThat(responseEntity.getBody(), nullValue());
		assertThat(responseEntity.getStatusCode(),
				is(HttpStatus.METHOD_NOT_ALLOWED));

	}

	@Test
	public void test4() {
		responseString = controller.callService(name, response);
		assertThat(responseString, notNullValue());
		assertThat(response, notNullValue());
		// see also: https://www.baeldung.com/mockito-verify
		verify(response).setStatus(HttpStatus.OK.value());
		assertThat(responseString.isEmpty(), is(false));

	}

	@Test
	public void test5() {
		responseString = controller.callService("illegal state", response);
		assertThat(responseString, nullValue());
		verify(response).setStatus(HttpStatus.INTERNAL_SERVER_ERROR.value());

	}

	@Test
	public void test6() {
		responseString = controller.callService("null pointer", response);
		assertThat(responseString, nullValue());
		verify(response).setStatus(HttpStatus.METHOD_NOT_ALLOWED.value());
	}

	@Test
	public void test7() {
		responseString = controller.callService(name, responseImpl);
		assertThat(responseString, notNullValue());
		assertThat(responseImpl.getStatus(), is(HttpStatus.OK.value()));
		assertThat(responseString.isEmpty(), is(false));

	}

	@Test
	public void test8() {
		responseString = controller.callService("illegal state", responseImpl);
		assertThat(responseString, nullValue());
		assertThat(responseImpl.getStatus(),
				is(HttpStatus.INTERNAL_SERVER_ERROR.value()));
	}

	@Test
	public void test9() {
		responseString = controller.callService("null pointer", responseImpl);
		assertThat(responseString, nullValue());
		assertThat(responseImpl.getStatus(),
				is(HttpStatus.METHOD_NOT_ALLOWED.value()));
	}

	// based on:
	// http://www.java2s.com/example/java-src/pkg/com/soho/framework/server/servlet/impl/httpservletresponseimpl-5dc51.html
	// see also:
	// https://alvinalexander.com/java/jwarehouse/eclipse/org.eclipse.equinox.http/src/org/eclipse/equinox/http/servlet/HttpServletResponseImpl.java.shtml
	public static class HttpServletResponseImpl implements HttpServletResponse {
		private int status;

		public HttpServletResponseImpl() {
		}

		@Override
		public void addCookie(Cookie cookie) {
		}

		@Override
		public void addDateHeader(String name, long date) {

		}

		@Override
		public void addHeader(String name, String value) {

		}

		@Override
		public void addIntHeader(String name, int value) {

		}

		@Override
		public boolean containsHeader(String name) {
			return false;
		}

		@Override
		public void sendError(int value) throws IOException {
		}

		@Override
		public void sendError(int value, String text) throws IOException {
		}

		@Override
		public void sendRedirect(String location) throws IOException {
		}

		@Override
		public void setDateHeader(String name, long date) {
		}

		@Override
		public void setHeader(String name, String value) {
		}

		@Override
		public void setIntHeader(String name, int value) {
		}

		@Override
		public ServletOutputStream getOutputStream() throws IOException {
			return null;
		}

		@Override
		public PrintWriter getWriter() throws IOException {
			return null;
		}

		@Override
		public void setStatus(int value) {
			this.status = value;
		}

		@Override
		public void setStatus(int value, String text) {
			this.status = value;
		}

		@Override
		public String getContentType() {
			return null;
		}

		@Override
		public void setContentType(String type) {
		}

		@Override
		public void setContentLength(int len) {
		}

		@Override
		public boolean isCommitted() {
			return true;
		}

		@Override
		public void reset() {
		}

		@Override
		public void resetBuffer() {
		}

		@Override
		public void flushBuffer() throws IOException {
		}

		@Override
		public int getBufferSize() {
			return 0;
		}

		@Override
		public void setBufferSize(int size) {
		}

		@Override
		public String encodeRedirectURL(String url) {
			return null;
		}

		@Override
		public String encodeRedirectUrl(String url) {
			return null;
		}

		@Override
		public String encodeURL(String url) {
			return null;
		}

		@Override
		public String encodeUrl(String url) {
			return null;
		}

		@Override
		public String getCharacterEncoding() {
			return null;
		}

		@Override
		public void setCharacterEncoding(String charset) {
		}

		@Override
		public Locale getLocale() {
			return null;
		}

		@Override
		public void setLocale(Locale loc) {
		}

		@Override
		public int getStatus() {
			return this.status;
		}

		@Override
		public String getHeader(String name) {
			return null;
		}

		@Override
		public Collection<String> getHeaders(String name) {
			return null;
		}

		@Override
		public Collection<String> getHeaderNames() {
			return null;
		}

		@Override
		public void setContentLengthLong(long arg0) {

		}

	}
}
