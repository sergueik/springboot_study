package example.beans;

//origin: https://github.com/xvitcoder/spring-mvc-angularjs

public class Train {
	private Long id;
	private String name;
	private Integer speed;
	private Boolean diesel;

	public Train() {
	}

	public Train(Long id, String name, Integer speed, Boolean diesel) {
		this.id = id;
		this.name = name;
		this.speed = speed;
		this.diesel = diesel;
	}

	public Long getId() {
		return id;
	}

	public void setId(Long data) {
		this.id = data;
	}

	public String getName() {
		return name;
	}

	public void setName(String data) {
		this.name = data;
	}

	public Integer getSpeed() {
		return speed;
	}

	public void setSpeed(Integer data) {
		this.speed = data;
	}

	public Boolean getDiesel() {
		return diesel;
	}

	public void setDiesel(Boolean data) {
		this.diesel = data;
	}
}
