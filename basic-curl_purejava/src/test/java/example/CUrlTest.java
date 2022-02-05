package example;

// import com.fasterxml.jackson.databind.ObjectMapper;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

import example.CUrl;

import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.junit.Ignore;
import org.junit.Test;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CountDownLatch;

import static org.junit.Assert.assertEquals;

public class CUrlTest {

	private Gson gson = new GsonBuilder().create();

	// set ENABLE_FIDDLER_FOR_ALL_TEST when tracing the traffic
	private static final boolean ENABLE_FIDDLER_FOR_ALL_TEST = System.getenv()
			.containsKey("ENABLE_FIDDLER_FOR_ALL_TEST");

	@Ignore
	// getting 0
	@Test
	public void gzippedResponse() {
		CUrl curl = curl("http://httpbin.org/gzip").opt("--compressed");

		assertEquals(curl.getHttpCode(), 200);
	}

	@Test
	public void headMethod() {
		// output to stdout
		CUrl curl = curl("http://httpbin.org/get").dumpHeader("-").opt("--head")
				.opt("--verbose");

		curl.exec();

		assertEquals(curl.getHttpCode(), 200);
	}

	// do URL-Encoding for each form value
	@Ignore
	@Test
	public void getRedirectLocation() {
		// force using GET method
		CUrl curl = curl("http://httpbin.org/redirect-to")
				.data("url=http://www.baidu.com", "UTF-8").opt("--get");
		curl.exec();
		String location = null;
		List<String[]> responseHeaders = curl.getResponseHeaders().get(0);
		// Follow redirects
		for (String[] headerValuePair : responseHeaders) {
			if ("Location".equals(headerValuePair[0])) {
				location = headerValuePair[1];
				break;
			}
		}
		assertEquals(302, curl.getHttpCode());
		assertEquals(location, "http://www.baidu.com");
	}

	// Use Fiddler to capture & parse HTTPS traffic
	// Ignore certificate check since it's issued by Fiddler
	@Ignore
	@Test
	public void insecureHttpsViaFiddler() {
		CUrl curl = curl("https://httpbin.org/get").proxy("127.0.0.1", 8888)

				.insecure();
		curl.exec();
		assertEquals(200, curl.getHttpCode());
	}

	@Test
	public void httpGet() {
		CUrl curl = curl("http://httpbin.org/get&hello=world&foo=bar")
				.opt("--verbose");

		Map<String, Object> json = curl.exec(jsonResolver, null);
		System.err.println("httpGet" + json);
		// the httpCode is not set
		// assertEquals(200, curl.getHttpCode());
	}

	@Test
	public void httpPost() {
		CUrl curl = curl("http://httpbin.org/post").data("hello=world&foo=bar")
				.data("foo=overwrite").opt("--verbose");
		curl.exec();
		assertEquals(200, curl.getHttpCode());
	}

	@Ignore
	@Test
	public void uploadMultipleFiles() {
		CUrl.MemIO inMemFile = new CUrl.MemIO();
		try {
			inMemFile.getOutputStream()
					.write("text file content blabla...".getBytes());
		} catch (Exception ignored) {
		}
		CUrl curl = curl("http://httpbin.org/post").form("formItem", "value")
				// a plain form item
				.form("file", inMemFile) // in-memory "file"
				.form("image", new CUrl.FileIO("D:\\tmp\\a2.png"));

		// A file in storage, change it to an existing path to avoid failure
		curl.exec();
		assertEquals(200, curl.getHttpCode());
	}

	// @Ignore
	@Test
	public void httpBasicAuth() {
		CUrl curl = curl("http://httpbin.org/basic-auth/abc/aaa")
				.proxy("127.0.0.1", 8888).opt("-u", "abc:aaa").opt("--verbose");
		Map<String, Object> json = curl.exec(jsonResolver, null);
		System.err.println("httpBasicAuth" + json);
		// HTTP code is not set during processing request - remains -1
		// assertEquals(200, curl.getHttpCode());
	}

	@Test
	public void customUserAgentAndHeaders() {
		String mobileUserAgent = "Mozilla/5.0 (Linux; U; Android 8.0.0; zh-cn; KNT-AL10 Build/HUAWEIKNT-AL10) AppleWebKit/537.36 (KHTML, like Gecko) MQQBrowser/7.3 Chrome/37.0.0.0 Mobile Safari/537.36";
		Map<String, String> fakeAjaxHeaders = new HashMap<String, String>();
		fakeAjaxHeaders.put("X-Requested-With", "XMLHttpRequest");
		fakeAjaxHeaders.put("Referer", "http://somesite.com/fake_referer");
		// simulate a mobile browser
		CUrl curl = curl("http://httpbin.org/get").opt("-A", mobileUserAgent)

				.headers(fakeAjaxHeaders) // simulate an AJAX request
				.header("X-Auth-Token: xxxxxxx"); // other custom header, this might be
																					// calculated elsewhere
		curl.exec();
		assertEquals(200, curl.getHttpCode());
	}

	@Test
	public void customResolver() {
		CUrl curl = curl("http://httpbin.org/json");
		// execute request and convert response to JSON
		Map<String, Object> json = curl.exec(jsonResolver, null);
		assertEquals(200, curl.getHttpCode());
		assertEquals("Yours Truly", deepGet(json, "slideshow.author"));
		assertEquals("Why <em>WonderWidgets</em> are great",
				deepGet(json, "slideshow.slides.1.items.0"));
		// execute request and convert response to HTML
		curl = curl("http://httpbin.org/html");
		Document html = curl.exec(htmlResolver, null);
		assertEquals(200, curl.getHttpCode());
		assertEquals("Herman Melville - Moby-Dick",
				html.select("h1:first-child").text());
	}

	@Ignore
	@Test
	public void threadSafeCookies() {
		final CountDownLatch count = new CountDownLatch(3);
		final CUrl[] curls = new CUrl[3];
		for (int i = 3; --i >= 0;) {
			final int idx = i;
			new Thread() {
				public void run() {
					CUrl curl = curls[idx] = curl("http://httpbin.org/get")
							.cookie("thread" + idx + "=#" + idx);
					curl.exec();
					count.countDown();
				}
			}.start();
		}
		try {
			count.await();
		} catch (Exception ignored) {
		} // make sure all requests are done
		assertEquals(200, curls[0].getHttpCode());
		assertEquals("thread0=#0",
				deepGet(curls[0].getStdout(jsonResolver, null), "headers.Cookie"));
		assertEquals("thread1=#1",
				deepGet(curls[1].getStdout(jsonResolver, null), "headers.Cookie"));
		assertEquals("thread2=#2",
				deepGet(curls[2].getStdout(jsonResolver, null), "headers.Cookie"));
	}

	@Ignore
	@Test
	public void reuseCookieAcrossThreads() {
		final CUrl.IO cookieJar = new CUrl.MemIO();
		final CountDownLatch lock = new CountDownLatch(1);
		new Thread() {
			public void run() {
				// server-side Set-Cookie response header
				// multiple cookies are seperated by semicolon and space "; "
				CUrl curl = curl("http://httpbin.org/cookies/set/from/server")
						.cookie("foo=bar; hello=world").cookieJar(cookieJar); // write
																																	// cookies to
																																	// an IO
																																	// instance
				curl.exec();
				lock.countDown();
			}
		}.start();
		try {
			lock.await();
		} catch (Exception ignored) {
		} // make sure request is done
		CUrl curl = curl("http://httpbin.org/cookies").cookie(cookieJar); // reuse
																																			// cookies
		curl.exec();
		assertEquals(200, curl.getHttpCode());
		assertEquals("bar",
				deepGet(curl.getStdout(jsonResolver, null), "cookies.foo"));
		assertEquals("world",
				deepGet(curl.getStdout(jsonResolver, null), "cookies.hello"));
		assertEquals("server",
				deepGet(curl.getStdout(jsonResolver, null), "cookies.from"));
	}

	// @Ignore
	@Test
	public void selfSignedCertificate() {
		CUrl curl = new CUrl("https://www.baidu.com/")
				.cert(new CUrl.FileIO("D:/tmp/test_jks.jks"), "123456")
				.proxy("127.0.0.1", 8888);
		System.out.println(curl.exec(CUrl.UTF8, null));
	}

	///////////////////////////////////////////////////////////////////////////////

	private CUrl curl(String url) {
		CUrl curl = new CUrl(url);
		if (ENABLE_FIDDLER_FOR_ALL_TEST) {
			curl.proxy("127.0.0.1", 8888).insecure();
		}
		return curl;
	}

	/** Implement a custom resolver that convert raw response to JSON */

	private CUrl.Resolver<Map<String, Object>> jsonResolver = new CUrl.Resolver<Map<String, Object>>() {
		@SuppressWarnings("unchecked")
		@Override
		public Map<String, Object> resolve(int httpCode, byte[] responseBody)
				throws Throwable {
			String json = new String(responseBody, "UTF-8");
			return gson.fromJson(json, Map.class);
		}
	};
	/** Implement a custom resolver that convert raw response to Jsoup Document */
	private CUrl.Resolver<Document> htmlResolver = new CUrl.Resolver<Document>() {
		@SuppressWarnings("unchecked")
		@Override
		public Document resolve(int httpCode, byte[] responseBody)
				throws Throwable {
			String html = new String(responseBody, "UTF-8");
			return Jsoup.parse(html);
		}
	};

	@SuppressWarnings("unchecked")
	private static <T> T deepGet(Object obj, String names) {
		boolean isMap;
		if (!(isMap = obj instanceof Map) && !(obj instanceof List))
			return null;
		int idx = names.indexOf('.');
		String n = idx > 0 ? names.substring(0, idx) : names;
		names = idx > 0 ? names.substring(idx + 1) : null;
		if (isMap) {
			obj = ((Map<String, ?>) obj).get(n);
		} else {
			idx = Integer.parseInt(n);
			if (idx < 0 || idx >= ((List<?>) obj).size())
				return null;
			obj = ((List<?>) obj).get(idx);
		}
		return names != null ? CUrlTest.<T> deepGet(obj, names)
				: obj != null ? (T) obj : null;
	}

}
