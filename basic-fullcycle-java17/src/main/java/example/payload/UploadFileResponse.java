package example.payload;

public class UploadFileResponse {
	private String fileName;
	private String fileDownloadUri;
	private String fileType;
	private long size;

	// TODO: add md5
	// see also:
	// https://github.com/wemakebug/FileUpload.Java/blob/master/src/main/java/com/zhangzhihao/FileUpload/Java/Utils/CreateMd5.java
	public UploadFileResponse(String fileName, String fileDownloadUri,
			String fileType, long size) {
		this.fileName = fileName;
		this.fileDownloadUri = fileDownloadUri;
		this.fileType = fileType;
		this.size = size;
	}

	public String getFileName() {
		return fileName;
	}

	public void setFileName(String value) {
		fileName = value;
	}

	public String getFileDownloadUri() {
		return fileDownloadUri;
	}

	public void setFileDownloadUri(String value) {
		fileDownloadUri = value;
	}

	public String getFileType() {
		return fileType;
	}

	public void setFileType(String value) {
		fileType = value;
	}

	public long getSize() {
		return size;
	}

	public void setSize(long value) {
		size = value;
	}
}