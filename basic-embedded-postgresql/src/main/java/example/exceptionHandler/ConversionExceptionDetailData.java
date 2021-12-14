package example.exceptionHandler;

public class ConversionExceptionDetailData {
	
	private int status;
	private String message;
	public ConversionExceptionDetailData(int status, String message) {
		super();
		this.status = status;
		this.message = message;
	}
	public ConversionExceptionDetailData() {
		super();
		// TODO Auto-generated constructor stub
	}
	public int getStatus() {
		return status;
	}
	public void setStatus(int status) {
		this.status = status;
	}
	public String getMessage() {
		return message;
	}
	public void setMessage(String message) {
		this.message = message;
	}
	
		
}
