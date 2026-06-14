package example.controller;

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;

import org.springframework.core.env.Environment;

import org.springframework.stereotype.Component;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import me.desair.tus.server.TusFileUploadService;
import me.desair.tus.server.exception.TusException;
import me.desair.tus.server.upload.UploadInfo;


import example.utils.TusStorageResolver;

@Controller
@RequestMapping(value = "/api/upload")

@CrossOrigin(origins = "*")
public class TusFileUploadController {

	private static final Logger logger = LoggerFactory.getLogger(TusFileUploadController.class);

	@Autowired
	private Environment env;

	@Autowired
	private TusFileUploadService tusFileUploadService;

	@Autowired
	private TusStorageResolver tusStorageResolver;

	@RequestMapping(value = { "", "/**" }, method = { RequestMethod.POST, RequestMethod.PATCH, RequestMethod.HEAD,
			RequestMethod.DELETE, RequestMethod.OPTIONS, RequestMethod.GET })
	public void processUpload(final HttpServletRequest servletRequest, final HttpServletResponse servletResponse)
			throws IOException, TusException {
		tusFileUploadService.process(servletRequest, servletResponse);

		UploadInfo info = tusFileUploadService.getUploadInfo(servletRequest.getRequestURI());

		if (info != null && !info.isUploadInProgress()) {
			logger.info("upload complete");
			logger.info("info: id: {} filename: {} local path: {}", info.getId(), info.getFileName(),
					tusStorageResolver.resolve(info));
		} else if (info != null)
			logger.info("upload in progress: {} / {} / {}", info.getOffset(), info.getLength(),
					(info.getOffset() - info.getLength()));
		else
			logger.info("upload not started");
		// access response header Location,Upload-Offset,Upload-length
		servletResponse.addHeader("Access-Control-Expose-Headers", "Location,Upload-Offset,Upload-Length");
	}
}
