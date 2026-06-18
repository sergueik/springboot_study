package example.dto;

public class FinalizeRequest {
	// NOTE: need to match the frontend
	// e.g. if JSON.stringify({uploadId}) is sent, but the FinalizeRequest has an id,
	// there error will be:
	// Required request body is missing: public org.springframework.http.ResponseEntity<?>
	// example.controller.FinalizeController.uploadJson(example.dto.FinalizeRequest)
	private String uploadId;

	public String getUploadId() {
		return uploadId;
	}

	public void setUploadId(String value) {
		uploadId = value;
	}
}
