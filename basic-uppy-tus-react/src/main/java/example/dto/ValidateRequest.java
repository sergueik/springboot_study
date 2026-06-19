package example.dto;

public class ValidateRequest {
	private String uploadId;
	private String hash;

	public String getUploadId() {
		return uploadId;
	}

	public void setUploadId(String value) {
		uploadId = value;
	}

	public String getHash() {
		return hash;
	}

	public void setHash(String value) {
		hash = value;
	}
}
