package example.client.generated;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the example.client.generated package. 
 * <p>An ObjectFactory allows you to programatically 
 * construct new instances of the Java representation 
 * for XML content. The Java representation of XML 
 * content can consist of schema derived interfaces 
 * and classes representing the binding of schema 
 * type definitions, element declarations and model 
 * groups.  Factory methods for each of these are 
 * provided in this class.
 * 
 */
@XmlRegistry
public class ObjectFactory {

    private final static QName _Employee_QNAME = new QName("http://www.jpworks.com/employee", "employee");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: example.client.generated
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link EmployeesResponse }
     * 
     */
    public EmployeesResponse createEmployeesResponse() {
        return new EmployeesResponse();
    }

    /**
     * Create an instance of {@link EmployeeByIdRequest }
     * 
     */
    public EmployeeByIdRequest createEmployeeByIdRequest() {
        return new EmployeeByIdRequest();
    }

    /**
     * Create an instance of {@link EmployeeByNameRequest }
     * 
     */
    public EmployeeByNameRequest createEmployeeByNameRequest() {
        return new EmployeeByNameRequest();
    }

    /**
     * Create an instance of {@link EmployeeResponse }
     * 
     */
    public EmployeeResponse createEmployeeResponse() {
        return new EmployeeResponse();
    }

    /**
     * Create an instance of {@link EmployeesResponse.Employee }
     * 
     */
    public EmployeesResponse.Employee createEmployeesResponseEmployee() {
        return new EmployeesResponse.Employee();
    }

    /**
     * Create an instance of {@link Address }
     * 
     */
    public Address createAddress() {
        return new Address();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link Object }{@code >}
     * 
     * @param value
     *     Java instance representing xml element's value.
     * @return
     *     the new instance of {@link JAXBElement }{@code <}{@link Object }{@code >}
     */
    @XmlElementDecl(namespace = "http://www.jpworks.com/employee", name = "employee")
    public JAXBElement<Object> createEmployee(Object value) {
        return new JAXBElement<Object>(_Employee_QNAME, Object.class, null, value);
    }

}
