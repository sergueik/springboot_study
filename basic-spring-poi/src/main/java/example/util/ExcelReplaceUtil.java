package example.util;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.util.List;

import org.apache.poi.hssf.usermodel.HSSFCell;
import org.apache.poi.hssf.usermodel.HSSFRow;
import org.apache.poi.hssf.usermodel.HSSFSheet;
import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.poifs.filesystem.POIFSFileSystem;
import org.apache.poi.ss.usermodel.CellType;
import example.command.ExcelReplaceDataVO;

public class ExcelReplaceUtil {
	public static boolean replaceModel(List<ExcelReplaceDataVO> datas, InputStream is, String targetFilePath) {
		boolean bool = true;
		try {
			POIFSFileSystem fs = new POIFSFileSystem(is);
			HSSFWorkbook wb = new HSSFWorkbook(fs);
			HSSFSheet sheet = wb.getSheetAt(0);
			for (ExcelReplaceDataVO data : datas) {
				// 获取单元格内容
				HSSFRow row = sheet.getRow(data.getRow());
				HSSFCell cell = row.getCell((short) data.getColumn());
				String str = cell.getStringCellValue();
				str = str.replace(data.getKey(), data.getValue());
				cell.setCellType(CellType.STRING);
				// cell.setEncoding(HSSFCell.ENCODING_UTF_16);
				cell.setCellValue(str);
			}
			FileOutputStream fileOut = new FileOutputStream(targetFilePath);
			wb.write(fileOut);
			fileOut.close();
		} catch (Exception e) {
			bool = false;
			e.printStackTrace();
		}
		return bool;
	}

	public static boolean replaceModel(List<ExcelReplaceDataVO> datas, String sourceFilePath, String targetFilePath)
			throws FileNotFoundException {
		return replaceModel(datas, new FileInputStream(sourceFilePath), targetFilePath);
	}
}
