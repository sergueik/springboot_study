package example.mapper;

import example.model.Person;
import example.rest.api.PersonResource;

import org.mapstruct.Mapper;

@Mapper(componentModel = "spring")
public interface PersonMapper {

    PersonResource toResource(Person person);

}
