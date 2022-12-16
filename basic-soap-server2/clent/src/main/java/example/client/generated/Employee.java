package example.client.generated;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.util.Date;

@AllArgsConstructor
@NoArgsConstructor
@Builder
@Data

public class Employee {
	long id;
	String firstname;
	String lastname;
	Date birthdate;
	String gender;
}


