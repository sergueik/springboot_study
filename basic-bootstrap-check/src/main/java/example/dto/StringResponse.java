package example.dto;

public class StringResponse {
	private String echo;

	public StringResponse(String s) {
		this.echo = s;
	}

	public String getEcho() {
		return echo;
	}

	public void setEcho(String echo) {
		this.echo = echo;
	}
}
