package net.sf.cobolToJson.impl.updateSchema;

import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.Line;
import net.sf.JRecord.Details.fieldValue.IFieldValue;

public class BlankLine extends Line {

	private int preferredLayoutIdx = 0;
	private String stringValue = "";
	
	public BlankLine(LayoutDetail schema) {
		super(schema);
	}

	@Override
	public int getPreferredLayoutIdx() {
		return preferredLayoutIdx;
	}

	public void setPreferredLayoutIdx(int preferredLayoutIdx) {
		this.preferredLayoutIdx = preferredLayoutIdx;
	}

	protected void setStringValue(String stringValue) {
		this.stringValue = stringValue;
	}

	@Override
	public IFieldValue getFieldValue(IFieldDetail field) {
		IFieldValue fieldValue = super.getFieldValue(field);
		if (fieldValue.isNumeric()) {
			fieldValue.set(0);
		} else {
			fieldValue.set(stringValue);
		}
		return fieldValue;
	}


}
