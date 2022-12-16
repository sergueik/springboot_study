package example;

import java.io.IOException;
import java.io.StringWriter;
import java.util.Arrays;
import java.util.Iterator;

import javax.xml.bind.JAXBElement;
import javax.xml.namespace.QName;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamResult;

import org.apache.http.client.methods.HttpPost;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.ws.WebServiceMessage;
import org.springframework.ws.client.core.WebServiceMessageCallback;
import org.springframework.ws.client.core.WebServiceTemplate;
import org.springframework.ws.soap.SoapHeaderElement;
import org.springframework.ws.soap.SoapMessage;
import org.springframework.ws.transport.context.TransportContext;
import org.springframework.ws.transport.context.TransportContextHolder;
import org.springframework.ws.transport.http.HttpComponentsConnection;
import org.springframework.xml.transform.StringSource;

import example.client.generated.ObjectFactory;
import example.client.generated.EmployeeByIdRequest;
import example.client.generated.EmployeeResponse;
import example.LogHelper;

@SpringBootApplication
@ComponentScan({ "example", "example.client.generated" })
public class Application implements CommandLineRunner {

	@Autowired
	LogHelper loghelper;

	// https://docs.spring.io/spring-ws/site/apidocs/org/springframework/ws/client/core/WebServiceTemplate.html
	@Autowired
//	@Qualifier("webServiceTemplate")
	private WebServiceTemplate webServiceTemplate;

	@Value("#{'${service.soap.action}'}")
	private String serviceSoapAction;

	@Value("#{'${service.user.id}'}")
	private String serviceUserId;

	@Value("#{'${service.user.password}'}")
	private String serviceUserPassword;

	public static void main(String[] args) {
		SpringApplication.run(Application.class, args);
		System.exit(0);
	}

	public void run(String... args) throws Exception {
		final EmployeeByIdRequest employeeByIdRequest = createEmployeeByIdRequest();
		@SuppressWarnings("unchecked")
		final JAXBElement<EmployeeResponse> jaxbElement = (JAXBElement<EmployeeResponse>) sendAndRecieve( employeeByIdRequest );
		final EmployeeResponse employeeResponse = jaxbElement.getValue();
		System.out.println(employeeResponse.getEmployee());
	}

	// see also:
	// https://www.tabnine.com/code/java/methods/org.springframework.ws.client.core.WebServiceTemplate/marshalSendAndReceive
	// https://docs.spring.io/spring-ws/site/apidocs/org/springframework/ws/client/core/WebServiceTemplate.html
	// https://docs.spring.io/spring-ws/site/apidocs/org/springframework/ws/client/core/WebServiceMessageCallback.html
	private Object sendAndRecieve(EmployeeByIdRequest seatMapRequestType) {
		return webServiceTemplate.marshalSendAndReceive(seatMapRequestType,
				new WebServiceMessageCallback() {
					@SuppressWarnings("deprecation")
					public void doWithMessage(WebServiceMessage message)
							throws IOException, TransformerException {

						SoapMessage soapMessage = (SoapMessage) message;

						soapMessage.setSoapAction(serviceSoapAction);
						org.springframework.ws.soap.SoapHeader soapheader = soapMessage
								.getSoapHeader();
						// those are empty
						Iterator<SoapHeaderElement> x = soapheader
								.examineAllHeaderElements();
						while (x.hasNext()) {
							loghelper.info("SOAP Header attribute: " + x.next().toString());
						}
						loghelper.info("SOAP Header: " + soapheader.toString());

						TransportContext context = TransportContextHolder
								.getTransportContext();
						HttpComponentsConnection connection = (HttpComponentsConnection) context
								.getConnection();
						HttpPost httpPost = connection.getHttpPost();
						String name = "Custom-Header";
						String value = "dummy";
						// httpPost.addHeader(name, value);
						loghelper.info("Added HTTP Header: "
								+ Arrays.asList(httpPost.getHeaders(name)));

						loghelper.info(
								"HTTP Headers: " + Arrays.asList(httpPost.getAllHeaders()));

						final StringWriter out = new StringWriter();
						webServiceTemplate.getMarshaller().marshal(
								getHeader(serviceUserId, serviceUserPassword),
								new StreamResult(out));
						Transformer transformer = TransformerFactory.newInstance()
								.newTransformer();
						transformer.transform(new StringSource(out.toString()),
								soapheader.getResult());
					}
				});
	}

	private EmployeeByIdRequest createEmployeeByIdRequest() {
		final ObjectFactory objectFactory = new ObjectFactory();
		final EmployeeByIdRequest obj = objectFactory.createEmployeeByIdRequest();
		
		obj.setId(1L);
		return obj;
	}

	private Object getHeader(final String userId, final String password) {
		return new Object();
	}

	public void setWebServiceTemplate(final WebServiceTemplate template) {
		this.webServiceTemplate = template;
	}

}

