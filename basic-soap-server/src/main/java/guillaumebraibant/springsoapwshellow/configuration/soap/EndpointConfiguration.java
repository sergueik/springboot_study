package guillaumebraibant.springsoapwshellow.configuration.soap;

import javax.xml.ws.Endpoint;

import org.apache.cxf.Bus;
import org.apache.cxf.jaxws.EndpointImpl;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import guillaumebraibant.springsoapwshellow.soap.SayHelloInterfaceImpl;

/**
 * Classe de configuration qui expose le endpoint chargé de recevoir et de traiter les requêtes
 * au webservice.
 */
@Configuration
public class EndpointConfiguration {

    /**
     * Publie le endpoint chargé de recevoir et de traiter les requêtes
     * 
     * @param implementor Bean implémentant la logique pour recevoir et traiter une requête
     * @param bus Bus contenant l'ensemble des beans nécessaires pour faire fonctionner le framework
     * @return Endpoint initialisé
     */
    @Bean
    public Endpoint publishOrderTimeDockOrgService (final SayHelloInterfaceImpl implementor, final Bus bus)
    {
        final EndpointImpl endpoint = new EndpointImpl(bus, implementor);
        // Publishing URL will be : /context-servlet/services/sayhello
        // Example "http://138.68.89.11:8080/hellow/services/sayhello"
        endpoint.publish("/sayhello");
        return endpoint;
    }
}