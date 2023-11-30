package example.servlets;

import static example.Constants.MAX_FILE_SIZE;
import static example.Constants.MAX_REQUEST_SIZE;
import static example.Constants.MEMORY_THRESHOLD;
import static example.Constants.UPLOAD_DIRECTORY;

import java.io.File;
import java.io.IOException;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.fileupload.FileItem;
import org.apache.commons.fileupload.disk.DiskFileItemFactory;
import org.apache.commons.fileupload.servlet.ServletFileUpload;

public class SecondServlet extends HttpServlet {
	// @WebServlet(name = "UploadServlet", urlPatterns = { "/uploadFile" })
	// public class UploadServlet extends HttpServlet {
	private static final long serialVersionUID = 1L;

	protected void doPost(HttpServletRequest request,
			HttpServletResponse response) throws ServletException, IOException {

		if (ServletFileUpload.isMultipartContent(request)) {

			DiskFileItemFactory factory = new DiskFileItemFactory();
			factory.setSizeThreshold(MEMORY_THRESHOLD);
			factory.setRepository(new File(System.getProperty("java.io.tmpdir")));

			ServletFileUpload upload = new ServletFileUpload(factory);
			upload.setFileSizeMax(MAX_FILE_SIZE);
			upload.setSizeMax(MAX_REQUEST_SIZE);
			String uploadPath = getServletContext().getRealPath("") + File.separator
					+ UPLOAD_DIRECTORY;
			File uploadDir = new File(uploadPath);
			if (!uploadDir.exists()) {
				uploadDir.mkdir();
			}

			try {
				List<FileItem> formItems = upload.parseRequest(request);

				if (formItems != null && formItems.size() > 0) {
					for (FileItem item : formItems) {
						if (!item.isFormField()) {
							String fileName = new File(item.getName()).getName();
							String filePath = uploadPath + File.separator + fileName;
							File storeFile = new File(filePath);
							item.write(storeFile);
							request.setAttribute("message",
									"File " + fileName + " has uploaded successfully!");
						}
					}
				}
			} catch (Exception ex) {
				request.setAttribute("message",
						"There was an error: " + ex.getMessage());
			}
			getServletContext().getRequestDispatcher("/result.jsp").forward(request,
					response);
		}
	}

	/*
	public void doGet(HttpServletRequest httpServletRequest,
			HttpServletResponse httpServletResponse) {
		System.out.println("second servlet....");
	}
	*/
}
