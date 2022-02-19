package example.model;

import lombok.Getter;
import lombok.Setter;

import javax.persistence.*;
import java.util.Objects;

@Entity
@Table(name = "oil_pump")
@Getter
@Setter
public class OilPump {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private int pumpId;
    private String name;
    private Integer level;
    private int userId;
    private int producted;
    private int price;
    private int production;

    public OilPump(Integer level) {
        this.level = level;
        this.producted = 0;
    }

    public OilPump() {

    }
}
