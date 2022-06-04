package example.influx.model;

public class SuccessResponseData extends ResponseData {
	public SuccessResponseData() {
		super(true, DEFAULT_SUCCESS_CODE, "request succeeded", "request succeeded",
				(Object) null);
	}

	public SuccessResponseData(Object object) {
		super(true, DEFAULT_SUCCESS_CODE, "request succeeded", "request succeeded",
				object);
	}

	public SuccessResponseData(Integer code, String message, Object object) {
		super(true, code, message, message, object);
	}

	public SuccessResponseData(Integer code, String message, String localizedMsg,
			Object object) {
		super(true, code, message, localizedMsg, object);
	}

	public SuccessResponseData(Integer code, String message) {
		super(true, code, message);
	}
}
