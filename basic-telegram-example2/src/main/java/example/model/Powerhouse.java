package example.model;

import lombok.Getter;
import lombok.Setter;

import javax.persistence.*;

@Entity
@Getter
@Setter
@Table(name = "powerstation")
public class Powerhouse {

    @Id
    @Column(name = "power_id")
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private int powerhouseId;
    private String name;
    private Integer level;
    private int userId;
    private int producted;
    private int production;
    private int price;

}
