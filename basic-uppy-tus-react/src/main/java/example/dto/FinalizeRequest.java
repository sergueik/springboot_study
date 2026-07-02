package example.dto;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

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

	@JsonCreator
	public FinalizeRequest(@JsonProperty("uploadId") String uploadId) {
		this.uploadId = uploadId;
	}
}
