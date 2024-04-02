package example.service.impl;

import java.io.*;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.*;
import javax.servlet.http.HttpServletResponse;

import org.apache.poi.hssf.usermodel.HSSFCell;
import org.apache.poi.hssf.usermodel.HSSFRow;
import org.apache.poi.hssf.usermodel.HSSFSheet;
import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.xssf.usermodel.XSSFCell;
import org.apache.poi.xssf.usermodel.XSSFRow;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.core.io.ClassPathResource;
import org.springframework.core.io.Resource;
import org.springframework.stereotype.Service;
import example.command.ExcelReplaceDataVO;
import example.service.ExcelService;
import example.util.*;

@Service
public class ExcelServiceImpl implements ExcelService {
	@Override
	public List<List<Object>> readExcel(File file) throws IOException {
		String fileName = file.getName();
		String extension = fileName.lastIndexOf(".") == -1 ? "" : fileName.substring(fileName.lastIndexOf(".") + 1);
		if ("xls".equals(extension)) {
			return readExcel2003(file);
		} else if ("xlsx".equals(extension)) {
			return readExcel2007(file);
		} else {
			throw new IOException("不支持的文件类型");
		}
	}

	@Override
	public List<List<Object>> readExcel(InputStream is, String suffix) throws IOException {
		if (Globals.SUFFIX_XLS.equals(suffix)) {
			return readExcel2003(is);
		} else if (Globals.SUFFIX_XLSX.equals(suffix)) {
			return readExcel2007(is);
		} else {
			throw new IOException("Unsupported file types");
		}
	}

	/**
	 * read office 2003 excel
	 */
	private List<List<Object>> readExcel2003(InputStream is) throws IOException {
		List<List<Object>> list = new LinkedList<List<Object>>();
		HSSFWorkbook hwb = new HSSFWorkbook(is);
		HSSFSheet sheet = hwb.getSheetAt(0);
		Object value = null;
		HSSFRow row = null;
		HSSFCell cell = null;
		int counter = 0;
		for (int i = sheet.getFirstRowNum(); counter < sheet.getPhysicalNumberOfRows(); i++) {
			row = sheet.getRow(i);
			if (row == null) {
				continue;
			} else {
				counter++;
			}
			List<Object> linked = new LinkedList<Object>();
			for (int j = row.getFirstCellNum(); j <= row.getLastCellNum(); j++) {
				cell = row.getCell(j);
				if (cell == null) {
					continue;
				}
				DecimalFormat df = new DecimalFormat("0");
				SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
				DecimalFormat nf = new DecimalFormat("0.00");
				switch (cell.getCellTypeEnum()) {
				case STRING:
					value = cell.getStringCellValue();
					break;
				case NUMERIC:
					if ("@".equals(cell.getCellStyle().getDataFormatString())) {
						value = df.format(cell.getNumericCellValue());
					} else if ("General".equals(cell.getCellStyle().getDataFormatString())) {
						value = nf.format(cell.getNumericCellValue());
					} else {
						value = sdf
								.format(org.apache.poi.ss.usermodel.DateUtil.getJavaDate(cell.getNumericCellValue()));
					}
					break;
				case BOOLEAN:
					value = cell.getBooleanCellValue();
					break;
				case BLANK:
					value = "";
					break;
				default:
					value = cell.toString();
				}
				if (value == null || "".equals(value)) {
					continue;
				}
				linked.add(value);
			}
			list.add(linked);
		}
		return list;
	}

	private List<List<Object>> readExcel2003(File file) throws IOException {
		return readExcel2003(new FileInputStream(file));
	}

	/**
	 * read Office 2007 excel
	 */
	private List<List<Object>> readExcel2007(InputStream is) throws IOException {
		List<List<Object>> list = new LinkedList<List<Object>>();
		XSSFWorkbook xwb = new XSSFWorkbook(is);
		XSSFSheet sheet = xwb.getSheetAt(0);
		Object value = null;
		XSSFRow row = null;
		XSSFCell cell = null;
		int counter = 0;
		for (int i = sheet.getFirstRowNum(); counter < sheet.getPhysicalNumberOfRows(); i++) {
			row = sheet.getRow(i);
			if (row == null) {
				continue;
			} else {
				counter++;
			}
			List<Object> linked = new LinkedList<Object>();
			for (int j = row.getFirstCellNum(); j <= row.getLastCellNum(); j++) {
				cell = row.getCell(j);
				if (cell == null) {
					continue;
				}
				DecimalFormat df = new DecimalFormat("0");
				SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
				DecimalFormat nf = new DecimalFormat("0.00");
				switch (cell.getCellTypeEnum()) {
				case STRING:
					value = cell.getStringCellValue();
					break;
				case NUMERIC:
					if ("@".equals(cell.getCellStyle().getDataFormatString())) {
						value = df.format(cell.getNumericCellValue());
					} else if ("General".equals(cell.getCellStyle().getDataFormatString())) {
						value = nf.format(cell.getNumericCellValue());
					} else {
						value = sdf
								.format(org.apache.poi.ss.usermodel.DateUtil.getJavaDate(cell.getNumericCellValue()));
					}
					break;
				case BOOLEAN:
					value = cell.getBooleanCellValue();
					break;
				case BLANK:
					value = "";
					break;
				default:
					value = cell.toString();
				}
				if (value == null || "".equals(value)) {
					continue;
				}
				linked.add(value);
			}
			list.add(linked);
		}
		return list;
	}

	private List<List<Object>> readExcel2007(File file) throws IOException {
		return readExcel2007(new FileInputStream(file));
	}

	@Override
	public void download(String filename, InputStream is, HttpServletResponse response) throws IOException {
		InputStream fis = new BufferedInputStream(is);
		byte[] buffer = new byte[fis.available()];
		fis.read(buffer);
		fis.close();
		response.reset();
		response.addHeader("Content-Disposition",
				"attachment;filename=" + new String(filename.getBytes(), "ISO-8859-1"));
		// response.addHeader("Content-Length", "" + file.length());
		OutputStream toClient = new BufferedOutputStream(response.getOutputStream());
		// NOTE: need to change locale
		response.setContentType("application/vnd.ms-excel;charset=gb2312");
		toClient.write(buffer);
		toClient.flush();
		toClient.close();
	}

	@Override
	public void download(String filename, String path, HttpServletResponse response) throws IOException {
		download(filename, new FileInputStream(path), response);
	}

	@Override
	public String export2003(String fileName, HttpServletResponse response) throws IOException {
		String docsPath;
		String filename = fileName + Globals.SUFFIX_XLS;
		Resource resource = new ClassPathResource(Globals.DOC + File.separator + fileName + Globals.SUFFIX_XLS);
		if (resource.exists()) {
			docsPath = resource.getFile().getPath();
		} else {
			String imagesPath = Globals.IMG + File.separator + "tomcat" + Globals.SUFFIX_PNG;
			ExcelExportUtil.export2003(imagesPath, Globals.DOC_PATH);
			docsPath = Globals.DOC_PATH + File.separator + Globals.EXPORT_BOOK;
			filename = Globals.EXPORT_BOOK;
		}
		download(filename, docsPath, response);
		return docsPath;
	}

	@Override
	public String export2007(String fileName, HttpServletResponse response) throws IOException {
		String filePath = Globals.DOC_PATH + File.separator + Globals.EXPORT_2007;
		ExcelExportUtil.export2007(filePath);
		download(Globals.EXPORT_2007, filePath, response);
		return filePath;
	}

	@Override
	public String template(String fileName, HttpServletResponse response) throws IOException {
		Resource resource = new ClassPathResource(Globals.DOC + File.separator + fileName + Globals.SUFFIX_XLS);
		InputStream is = resource.getInputStream();
		String exportFileName = fileName + System.currentTimeMillis() + Globals.SUFFIX_XLS;// 导出Excel文件名
		String exportFilePath = Globals.DOC_PATH + File.separator + exportFileName;
		ExcelTemplateUtil excel = ExcelTemplateUtil.getInstance().readTemplatePath(is);
		for (int i = 0; i < 5; i++) {
			excel.creatNewRow();
			excel.createNewCol("Col" + i);
			excel.createNewCol(i);
			excel.createNewCol(i);
		}
		Map<String, String> datas = new HashMap<String, String>();
		datas.put("title", "Apache POI");
		datas.put("content", "the Java API for Microsoft Documents");
		datas.put("date", DateUtil.format(new Date()));
		excel.replaceFind(datas);
		excel.insertSer();
		excel.writeToFile(exportFilePath);
		download(exportFileName, exportFilePath, response);
		return exportFilePath;
	}

	@Override
	public String replace(String fileName, HttpServletResponse response) throws IOException {
		Resource resource = new ClassPathResource(Globals.DOC + File.separator + fileName + Globals.SUFFIX_XLS);
		String exportFileName = fileName + System.currentTimeMillis() + Globals.SUFFIX_XLS;// 导出Excel文件名
		String exportFilePath = Globals.DOC_PATH + File.separator + exportFileName;
		List<ExcelReplaceDataVO> datas = new ArrayList<ExcelReplaceDataVO>();
		ExcelReplaceDataVO voCompany = new ExcelReplaceDataVO();
		voCompany.setRow(13);
		voCompany.setColumn(1);
		voCompany.setKey("company");
		voCompany.setValue("XXX有限公司");
		// example update
		ExcelReplaceDataVO voContent = new ExcelReplaceDataVO();
		voContent.setRow(4);
		voContent.setColumn(1);
		voContent.setKey("content");
		voContent.setValue("replacement content");
		datas.add(voCompany);
		datas.add(voContent);
		InputStream is = resource.getInputStream();
		ExcelReplaceUtil.replaceModel(datas, is, exportFilePath);
		download(exportFileName, exportFilePath, response);
		return exportFilePath;
	}
}
