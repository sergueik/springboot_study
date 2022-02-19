package example.model;

import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.Setter;
import org.springframework.beans.factory.annotation.Value;

import javax.persistence.*;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.Objects;

@Entity
@Getter
@Setter
@RequiredArgsConstructor
@Table(name = "usr")
public class User {

    @Id
    private int userId;
    private Integer oilCoin;
    private Integer gold;
    private Integer eCoin;
    private Integer eCrypt;
    private Integer referId;
    private Integer countReferals;
    private String regDate;
    private Integer taskCompleted;
    private String qiwi;
    private Boolean dailyBonus;
    private String name;
    private int oilProductTime;
    private int electricProductTime;
    private int oilProducted;
    private int electricProducted;

    @Column(name = "references_url")
    private String referencesUrl;
    @Column(name = "user_role")
    private String role;
    private String positions;

    @Column(name = "balls_1")
    // желтые
    private double ballsOne;

    @Column(name = "balls_2")
    // синие
    private double ballsTwo;
    private boolean joined;

    public User(int userId) {
        this.userId = userId;
        this.oilCoin = 0;
        this.gold = 0;
        this.eCoin = 0;
        this.eCrypt = 0;
        this.referencesUrl = "";
        this.referId = 0;
        this.role = "user";
        this.positions = "back";
        this.ballsOne = 0.000;
        this.ballsTwo = 0.000;
        this.taskCompleted = 0;
        this.regDate = new SimpleDateFormat("dd.MM.yyyy").format(new Date());
        this.qiwi = "";
        this.dailyBonus = false;
        this.countReferals = 0;
        this.joined = false;
        this.name = "Без названия";
        this.electricProducted = 0;
        this.oilProducted = 0;
        this.oilProductTime = 0;
        this.electricProductTime = 0;
    }
}
