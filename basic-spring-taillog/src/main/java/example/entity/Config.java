package example.entity;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import com.fasterxml.jackson.annotation.JsonRootName;

import example.notification.WordNotification;
import lombok.Data;
import org.springframework.data.jpa.domain.AbstractPersistable;
import org.springframework.stereotype.Service;

import javax.persistence.*;
import java.util.Set;

@Data
@Entity
@Table(name = "TB_CONFIG")
@AttributeOverride(name = "id", column = @Column(name = "ID_CONFIG"))
public class Config extends AbstractPersistable<Integer> {

	@Column(name = "file")
	private String file;

	@ManyToOne(cascade = CascadeType.ALL)
	@JoinColumn(name = "ID_PALAVRA")
	private WordNotification word;

	@Column(name = "DELAY")
	private long delay;

	@Column(name = "URL_NOTIFICAR")
	private String url;

	@Override
	public void setId(final Integer id) {
		super.setId(id);
	}
}
