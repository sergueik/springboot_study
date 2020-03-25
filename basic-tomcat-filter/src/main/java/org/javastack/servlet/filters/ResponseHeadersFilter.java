package org.javastack.servlet.filters;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Enumeration;
import java.util.List;

import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletResponse;

public final class ResponseHeadersFilter implements Filter {
	private List<Header> earlyHeaders = null;
	private List<Header> lateHeaders = null;

	@Override
	public void init(final FilterConfig filterConfig) throws ServletException {
		final List<Header> earlyHeaders = new ArrayList<Header>();
		final List<Header> lateHeaders = new ArrayList<Header>();
		final Enumeration<String> e = filterConfig.getInitParameterNames();
		while (e.hasMoreElements()) {
			final String name = e.nextElement();
			final String[] nt = name.split(":");
			final String value = filterConfig.getInitParameter(name);
			final TAG tag = ((nt.length < 2) ? TAG.SET : TAG.getTag(nt[1]));
			final TYPE type = ((nt.length < 3) ? TYPE.EARLY : TYPE.getType(nt[2]));
			if (type == TYPE.LATE) {
				lateHeaders.add(new Header(nt[0], tag, value));
			} else {
				earlyHeaders.add(new Header(nt[0], tag, value));
			}
		}
		this.earlyHeaders = Collections.unmodifiableList(earlyHeaders);
		this.lateHeaders = Collections.unmodifiableList(lateHeaders);
	}

	@Override
	public void doFilter(final ServletRequest request,
			final ServletResponse response, final FilterChain chain)
			throws IOException, ServletException {
		if (response instanceof HttpServletResponse) {
			final HttpServletResponse res = ((HttpServletResponse) response);
			for (final Header e : earlyHeaders) {
				processHeader(res, e);
			}
			chain.doFilter(request, response);
			for (final Header e : lateHeaders) {
				processHeader(res, e);
			}
		} else {
			chain.doFilter(request, response);
		}
	}

	private final void processHeader(final HttpServletResponse res,
			final Header e) {
		switch (e.tag) {
		case SET: {
			res.setHeader(e.name, e.value);
			break;
		}
		case SETIFEMPTY: {
			final String ov = res.getHeader(e.name);
			if ((ov == null) || ov.isEmpty()) {
				res.setHeader(e.name, e.value);
			}
			break;
		}
		case ADD: {
			res.addHeader(e.name, e.value);
			break;
		}
		case ADDIFEXIST: {
			final String ov = res.getHeader(e.name);
			if ((ov != null) && !ov.isEmpty()) {
				res.addHeader(e.name, e.value);
			}
			break;
		}
		}
	}

	@Override
	public void destroy() {
	}

	private static enum TAG {
		SET, // *default
		SETIFEMPTY, //
		ADD, //
		ADDIFEXIST; //

		static TAG getTag(final String name) {
			try {
				return TAG.valueOf(name.trim().toUpperCase());
			} catch (Exception e) {
			}
			return TAG.SET;
		}
	}

	private static enum TYPE {
		EARLY, // *default
		LATE; //

		static TYPE getType(final String name) {
			try {
				return TYPE.valueOf(name.trim().toUpperCase());
			} catch (Exception e) {
			}
			return TYPE.EARLY;
		}
	}

	private static class Header {
		final String name;
		final TAG tag;
		final String value;

		public Header(final String name, final TAG tag, final String value) {
			this.name = name;
			this.tag = tag;
			this.value = value;
		}
	}
}