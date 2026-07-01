package example.dto;

public class FinalizeRequest {

	private String uploadId;

	public FinalizeRequest() {
    }

	public String getUploadId() {
		return uploadId;
	}

	public void setUploadId(String value) {
		uploadId = value;
	}

	public FinalizeRequest(String uploadId) {
		this.uploadId = uploadId;
	}
}
