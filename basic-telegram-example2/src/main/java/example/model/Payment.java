package example.model;

import lombok.Getter;
import lombok.Setter;

import javax.persistence.*;
import java.util.Objects;

@Entity
@Setter
@Getter
public class Payment {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private int id;
    private Integer userId;
    private Integer sum;
    private Boolean success;
    private String time;
    @Column(name = "p_time")
    private String pTime;

}
