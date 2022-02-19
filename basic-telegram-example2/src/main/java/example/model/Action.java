package example.model;

import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.Setter;

import javax.persistence.*;
import java.util.Objects;

@Entity
@Setter
@Getter
@Table(name = "actions")
public class Action {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private int actionsId;
    private String nameCompany;
    private int userId;
    private int quantity;
    private String type;

    public Action() {
        this.quantity = 0;
        this.userId = 0;
        this.nameCompany = "";
        this.type = "user";
    }
}
