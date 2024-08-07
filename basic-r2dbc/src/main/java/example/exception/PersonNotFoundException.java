package example.exception;


public class PersonNotFoundException extends NotFoundException {

    public PersonNotFoundException(Long id) {
        super(String.format("Person [%d] is not found", id));
    }

}
