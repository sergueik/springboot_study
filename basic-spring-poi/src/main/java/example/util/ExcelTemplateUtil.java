package example.util;

import java.io.*;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import org.apache.poi.openxml4j.exceptions.InvalidFormatException;
import org.apache.poi.ss.usermodel.*;
import org.apache.poi.ss.usermodel.CellType;

public class ExcelTemplateUtil {
	public final static String TEMPLATE_NAME = "datastart";
	public final static String STYLE = "style";
	public final static String DEFALULT_STYLE = "defaultStyle";
	public final static String SERNUMS = "sernums";
	private int initRowIndex;
	private int initColIndex;
	private int curRowIndex; // current row
	private int curColIndex; // current column
	private int lastRowInex; // last line
	private float defaultHeight; // default row height
	private int serColIndex; // serial number line
	private Workbook workbook = null;
	private Sheet sheet = null;
	private Row curRow = null; // current row
	private Map<Integer, CellStyle> styles = null;
	private CellStyle defaultStyle = null;
	private static ExcelTemplateUtil excel = new ExcelTemplateUtil();

	private ExcelTemplateUtil() {
	}

	public static ExcelTemplateUtil getInstance() {
		return excel;
	}

	public ExcelTemplateUtil readTemplateClassPath(String calsspath) {
		try {
			workbook = WorkbookFactory.create(ExcelTemplateUtil.class.getResourceAsStream(calsspath));
			initTemplate();
		} catch (IOException e) {
			e.printStackTrace();
			throw new RuntimeException("template file does not exist");
		}
		return this;
	}

	public ExcelTemplateUtil readTemplatePath(String path) {
		try {
			workbook = WorkbookFactory.create(new File(path));
			initTemplate();
		} catch (IOException e) {
			e.printStackTrace();
			throw new RuntimeException("template file does not exist");
		}
		return this;
	}

	public ExcelTemplateUtil readTemplatePath(InputStream is) {
		try {
			workbook = WorkbookFactory.create(is);
			initTemplate();
		} catch (IOException e) {
			e.printStackTrace();
			throw new RuntimeException("template file does not exist");
		}
		return this;
	}

	public void creatNewRow() {
		// curRowIndex != initRowIndxe :current row exists
		if (lastRowInex > curRowIndex && curRowIndex != initRowIndex) {
			sheet.shiftRows(curRowIndex, lastRowInex, 1, true, true);
			// some templates may end up with non-data such as dates or names,
			// and the rows need to move
			lastRowInex++;
		}
		curRow = sheet.createRow(curRowIndex);
		curRow.setHeightInPoints(defaultHeight);
		curRowIndex++;
		curColIndex = initColIndex;
	}

	public void createNewCol(String value) {
		Cell cell = curRow.createCell(curColIndex);
		setStyle(cell);
		cell.setCellValue(value);
		curColIndex++;
	}

	public void createNewCol(double value) {
		Cell cell = curRow.createCell(curColIndex);
		setStyle(cell);
		cell.setCellValue(value);
		curColIndex++;
	}

	public void createNewCol(boolean value) {
		Cell cell = curRow.createCell(curColIndex);
		setStyle(cell);
		cell.setCellValue(value);
		curColIndex++;
	}

	public void createNewCol(Date value) {
		Cell cell = curRow.createCell(curColIndex);
		setStyle(cell);
		cell.setCellValue(value);
		curColIndex++;
	}

	public void createNewCol(Calendar value) {
		Cell cell = curRow.createCell(curColIndex);
		setStyle(cell);
		cell.setCellValue(value);
		curColIndex++;
	}

	public void createNewCol(RichTextString value) {
		Cell cell = curRow.createCell(curColIndex);
		setStyle(cell);
		cell.setCellValue(value);
		curColIndex++;
	}

	public void replaceFind(Map<String, String> datas) {
		if (datas == null)
			return;
		for (Row row : sheet) {
			for (Cell cell : row) {
				if (cell.getCellTypeEnum() != CellType.STRING)
					continue;
				String value = cell.getStringCellValue().trim();
				if (value.startsWith("#")) {
					if (datas.containsKey(value.substring(1))) {
						cell.setCellValue(datas.get(value.substring(1)));
					}
				}
			}
		}
	}

	/**
	 * insert serial number
	 */
	public void insertSer() {
		int index = 1;
		Row row = null;
		Cell cell = null;
		for (int i = initRowIndex; i < curRowIndex; i++) {
			row = sheet.getRow(i);
			cell = row.createCell(serColIndex);
			setStyle(cell);
			cell.setCellValue(index++);
		}
	}

	public void writeToFile(String path) {
		FileOutputStream fos = null;
		try {
			fos = new FileOutputStream(path);
			workbook.write(fos);
		} catch (FileNotFoundException e) {
			e.printStackTrace();
			throw new RuntimeException("file not found");
		} catch (IOException e) {
			e.printStackTrace();
			throw new RuntimeException("file write exception");
		} finally {
			try {
				if (fos != null) {
					fos.close();
					fos = null;
				}
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
	}

	public void writeToStream(OutputStream stream) {
		try {
			workbook.write(stream);
		} catch (IOException e) {
			e.printStackTrace();
			throw new RuntimeException("bad output file");
		} finally {
			try {
				if (stream != null) {
					stream.close();
					stream = null;
				}
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
	}

	private void initTemplate() {
		sheet = workbook.getSheetAt(0);
		styles = new HashMap<Integer, CellStyle>();
		initConfigData();
		lastRowInex = sheet.getLastRowNum();
	}

	/**
	 * get the default style (if none, use the start style styles: custom styles
	 *
	 */
	private void initConfigData() {
		for (Row row : sheet) {
			for (Cell cell : row) {
				if (cell.getCellTypeEnum() != CellType.STRING)
					continue;
				String value = cell.getStringCellValue().trim();
				if (TEMPLATE_NAME.equals(value)) {
					initRowIndex = cell.getRowIndex();
					initColIndex = cell.getColumnIndex();
					curRowIndex = initRowIndex;
					curColIndex = initColIndex;
					defaultHeight = row.getHeightInPoints();
					if (defaultStyle == null)
						defaultStyle = cell.getCellStyle();
				}
				if (DEFALULT_STYLE.equals(value))
					defaultStyle = cell.getCellStyle();
				if (STYLE.equals(value)) {
					styles.put(cell.getColumnIndex(), cell.getCellStyle());
				}
				// Get the column where the style is located
				if (SERNUMS.equals(value))
					serColIndex = cell.getColumnIndex();
			}
		}
	}

	private void setStyle(Cell cell) {
		// use the custom style, if there is one otherwise use the default style
		if (styles.containsKey(curColIndex)) {
			cell.setCellStyle(styles.get(curColIndex));
		} else {
			cell.setCellStyle(defaultStyle);
		}
	}
}
