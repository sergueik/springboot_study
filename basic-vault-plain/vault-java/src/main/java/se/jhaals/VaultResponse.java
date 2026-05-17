package se.jhaals;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.Map;

@JsonIgnoreProperties(ignoreUnknown = true)
public class VaultResponse {

	@JsonProperty("wrap_info")
	private String wrapInfo;
	@JsonProperty("request_id")
	private String requestId;
	@JsonProperty("warnings")
	private String warnings;

	public String getRequestId() {
		return requestId;
	}

	public String getWarnings() {
		return warnings;
	}

	public String getAuth() {
		return auth;
	}

	@JsonProperty("auth")
	private String auth;

	@JsonProperty("lease_id")
	private String leaseId;
	@JsonProperty
	private Boolean renewable;
	@JsonProperty("lease_duration")
	private String leaseDuration;
	@JsonProperty
	private Map<String, Object> data;

	public String getLeaseId() {
		return leaseId;
	}

	public Boolean getRenewable() {
		return renewable;
	}

	public String getLeaseDuration() {
		return leaseDuration;
	}

	public Map<String, Object> getData() {
		return data;
	}

	@Override
	public String toString() {
		return "VaultResponse{" + "leaseId='" + leaseId + '\'' + ", renewable=" + renewable + ", leaseDuration='"
				+ leaseDuration + '\'' + ", data=" + data + '}';
	}
}

/*
{
"request_id": "162d5ea8-eb01-d9eb-4a51-1195811d5b3f",
"lease_id": "",
"renewable": false,
"lease_duration": 0,
"data": {
 "data": {
   "public_key": "testuser",
   "secret_key": "test123"
 },
 "metadata": {
   "created_time": "2026-05-17T15:36:19.427219926Z",
   "custom_metadata": null,
   "deletion_time": "",
   "destroyed": false,
   "version": 1
 }
},
"wrap_info": null,
"warnings": null,
"auth": null
}

*/
