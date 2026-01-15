package net.sf.cobolToJson.impl.updateSchema;

import java.io.IOException;

import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.IO.IReadLine;

public class BlankLineReaderAllOptions implements IReadLine {

	private final ExitManager manager;
	private final BlankLine blankLine;
	//private final int maxIndex;
	//private int index = 0;
	private int lastUpdateCount = -1;
	private boolean end = false;
	private IIndexList indexList;

	
	public BlankLineReaderAllOptions(ExitManager manager, LayoutDetail layout) {
		super();
		this.manager = manager;
		this.blankLine = new BlankLine(layout);
		this.indexList = RecordIndexList.getIndexs(layout);
		blankLine.setStringValue("A");
		blankLine.setPreferredLayoutIdx(indexList.getNextIndex());
	}

	@Override
	public AbstractLine read() throws IOException {
		int updateCount = manager.getUpdateCount();
		if (updateCount == lastUpdateCount) {
			if (! indexList.isMoreIndexs()) {
				if (end) {
					return null;
				}
				indexList.resetIndex();;
				end = true;
				blankLine.setStringValue("");
				manager.setForceOk(true);
			}
			blankLine.setPreferredLayoutIdx(indexList.getNextIndex());
//			updateCount -= 1;
		} 
		lastUpdateCount = updateCount;
		return blankLine;
	}

}
