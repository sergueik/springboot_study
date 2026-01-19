package net.sf.cobolToJson.impl.updateSchema;

import java.io.IOException;

import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.IO.IReadLine;

/**
 * This class will supply one empty line for each record type
 * @author Bruce Martin
 *
 */
public class BlankLineReader implements IReadLine {

	private final BlankLine blankLine;
	private IIndexList indexList;
	
	/**
	 * This call will let you read one blank line for every record type 
	 * @param layout layout to use for the blank line
	 */
	public BlankLineReader(LayoutDetail layout) {
		super();
		this.blankLine = new BlankLine(layout);
		this.indexList = RecordIndexList.getIndexs(layout);
		
		blankLine.setStringValue("A");
		
		
	}

	@Override
	public AbstractLine read() throws IOException {

		if (indexList.isMoreIndexs()) {
			blankLine.setPreferredLayoutIdx(indexList.getNextIndex());
			return blankLine;
		}
		return null;
	}

}
