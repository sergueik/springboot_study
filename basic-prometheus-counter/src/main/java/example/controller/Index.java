package example.controller;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.HttpHeaders;

import org.springframework.stereotype.Component;


// NOTE: old style annotations

@Component
@Path("/")
public class Index {

	@GET
	public String message(final @Context HttpHeaders httpHeaders,
			final @Context HttpServletRequest httpRequest) {
		return "<html>" + "<head>" + "</head>" + "<body>"
				+ "<a href=\"./hello-world\"\">Link to \"Hello world with counter for prometheus\" REST service</a><br/>"
				+ "<a href=\"./metrics\"\">Link to \"Metrics for prometheus\" REST service</a>"
				+ "</body>" + "</html>";
	}

}
