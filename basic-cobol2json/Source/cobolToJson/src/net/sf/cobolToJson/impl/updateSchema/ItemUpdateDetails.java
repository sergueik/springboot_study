package net.sf.cobolToJson.impl.updateSchema;

import net.sf.JRecord.schema.IArrayItemCheck;
import net.sf.JRecord.schema.IItemUpdateDetails;
import net.sf.JRecord.schema.jaxb.interfaces.IFormatField;
import net.sf.JRecord.schema.jaxb.interfaces.IRedefineSelection;
import net.sf.JRecord.schema.jaxb.interfaces.IWriteCheck;

public class ItemUpdateDetails implements IItemUpdateDetails {
	private final IFormatField formatField;
	private final IWriteCheck writeCheck;
	private final IRedefineSelection redefineSelection;
	
	ItemUpdateDetails(ExitManager exitManager, IItemUpdateDetails source) {
		formatField = source.getFormatField();
		if (exitManager == null) {
			writeCheck = null;
			redefineSelection = null;
		} else {
			writeCheck = source.getWriteCheck() == null ? null : exitManager.newWriteCheck();
			redefineSelection = source.getRedefineSelection() == null ? null : exitManager.newRedefinesSelection();
		}
		
	}
	
	@Override
	public IArrayItemCheck getArrayCheck() {
		return null;
	}

	@Override
	public IWriteCheck getWriteCheck() {
		return writeCheck;
	}

	@Override
	public IFormatField getFormatField() {
		return formatField;
	}

	@Override
	public IRedefineSelection getRedefineSelection() {
		return redefineSelection;
	}

}
