package example.beans;

public class RailwayStation {

	private Long id;

	private String name;

	private Train train;

	public String getName() {
		return name;
	}

	public void setName(String data) {
		this.name = data;
	}

	public Train getTrain() {
		return train;
	}

	public void setTrain(Train data) {
		this.train = data;
	}

	public Long getId() {
		return id;
	}

	public void setId(Long data) {
		this.id = data;
	}

}
