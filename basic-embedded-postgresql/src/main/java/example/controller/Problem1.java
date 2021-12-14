package example.controller;

import org.json.JSONException;
import org.json.JSONObject;
import org.json.XML;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import example.exceptionHandler.ConversionException;

import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Paths;

import org.springframework.core.io.ClassPathResource;

/**
 * This is the class in which you need to write the code to develop a rest API
 * which will read the file myMS123466.xml from resource folder and convert its
 * xml content into json and finally the rest API will return that JSON content.
 */

@RestController
@RequestMapping("/api/v1")
public class Problem1 {

	/*
	@GetMapping("/ms")
	public String getJSONFromXML(  ) throws ConversionException {
		
		ClassPathResource res = new ClassPathResource("myMS123466.xml");
	
		File file;
		try {
			file = res.getFile();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			throw new ConversionException("File Not Found");
		}
		  
		BufferedReader br = null;
		try {
			br = new BufferedReader(new FileReader(file));
		} catch (FileNotFoundException e) {
			throw new ConversionException("Error Reading From File");
		}
		  
		String st;
		StringBuilder sb = new StringBuilder();
		try {
			while ((st = br.readLine()) != null) {
			    sb.append(st); 
			}
		} catch (IOException e) {
			throw new ConversionException("Error Reading From File");
		} 
		
		JSONObject xmlJSONObj;
		try {
			xmlJSONObj = XML.toJSONObject(sb.toString());
		} catch (JSONException e) {
			throw new ConversionException("Bad XML ");
		}finally {
			try {
				br.close();
			} catch (IOException e) {
				throw new ConversionException("Error Reading From File");
			}
		}
		return xmlJSONObj.toString();
		
	}
	*/

	@GetMapping(value = "/ms")
	public String convertXmlToJson() throws ConversionException {
		ClassPathResource xmlResource = new ClassPathResource("myMS123466.xml");

		String xmlString = null;
		try {
			xmlString = new String(
					Files.readAllBytes(Paths.get(xmlResource.getURL().toURI())));
		} catch (IOException | URISyntaxException e) {
			e.printStackTrace();
		}

		JSONObject xmlJSONObj = null;
		if (xmlString != null) {
			try {
				xmlJSONObj = XML.toJSONObject(xmlString);
			} catch (JSONException e) {
				throw new ConversionException("Format not supported ");
			}
		}
		return xmlJSONObj.toString();
	}
}
