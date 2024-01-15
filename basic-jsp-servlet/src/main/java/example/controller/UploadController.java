package example.controller;

import static example.Constants.MAX_FILE_SIZE;
import static example.Constants.MAX_REQUEST_SIZE;
import static example.Constants.MEMORY_THRESHOLD;
import static example.Constants.UPLOAD_DIRECTORY;

import java.io.File;
import java.io.IOException;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.fileupload.FileItem;
import org.apache.commons.fileupload.disk.DiskFileItemFactory;
import org.apache.commons.fileupload.servlet.ServletFileUpload;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PostMapping;

@Controller
public class UploadController {
	private static final long serialVersionUID = 1L;

	// origin: https://www.baeldung.com/upload-file-servlet
	@PostMapping("/multiPartServlet")
	protected String doPost(HttpServletRequest request,
			HttpServletResponse response) throws ServletException, IOException {

		System.err.println("in upload");
		if (ServletFileUpload.isMultipartContent(request)) {
			System.err.println("in multipart");

			DiskFileItemFactory factory = new DiskFileItemFactory();
			factory.setSizeThreshold(MEMORY_THRESHOLD);
			factory.setRepository(new File(System.getProperty("java.io.tmpdir")));

			ServletFileUpload upload = new ServletFileUpload(factory);
			upload.setFileSizeMax(MAX_FILE_SIZE);
			upload.setSizeMax(MAX_REQUEST_SIZE);
			String uploadPath = System.getProperty("java.io.tmpdir") + File.separator
					+ UPLOAD_DIRECTORY;
			File uploadDir = new File(uploadPath);
			System.err.println("Creating dir: " + uploadPath);
			if (!uploadDir.exists()) {
				uploadDir.mkdir();
			}

			try {
				List<FileItem> formItems = upload.parseRequest(request);
				System.err.println("Processing items: " + formItems);
				if (formItems != null && formItems.size() > 0) {
					for (FileItem item : formItems) {
						if (!item.isFormField()) {
							String fileName = new File(item.getName()).getName();
							String filePath = uploadPath + File.separator + fileName;
							File storeFile = new File(filePath);

							System.err.println("Writing file: " + filePath);
							item.write(storeFile);
							System.err
									.println("File " + fileName + " has uploaded successfully!");
							request.setAttribute("message",
									"File " + fileName + " has uploaded successfully!");
						}
					}
				}
			} catch (Exception ex) {
				System.err.println("There was an error: " + ex.getMessage());
				request.setAttribute("message",
						"There was an error: " + ex.getMessage());
			}
		}
		return "/result.jsp";
	}
}
