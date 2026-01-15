package net.sf.cobolToJson.impl.updateSchema;

import java.util.ArrayList;
import java.util.List;

import net.sf.JRecord.Details.LayoutDetail;

public class RecordIndexList {
	
	public static IIndexList getIndexs(LayoutDetail layout) {
		if (layout.hasTreeStructure()) {
			return new TreeIndexList(layout);
		}
		
		return new StandardIndexList(layout);
	}

	private static class StandardIndexList implements IIndexList {
		final int maxIndex;
		private int currentIndex = 0;
		
		private StandardIndexList(LayoutDetail layout) {
			maxIndex = layout.getRecordCount();
		}
		
		@Override
		public int getNextIndex() {
			return currentIndex < maxIndex ? currentIndex++ : -1;
		}
		
		@Override
		public void resetIndex() {
			currentIndex = 0;
		}
		
		@Override
		public boolean isMoreIndexs() {
			return currentIndex < maxIndex;
		}
	}
	
	private static class TreeIndexList implements IIndexList {
		private int idx = 0;
		private final List<Integer> indexs;
		
		TreeIndexList(LayoutDetail layout) {
			indexs = new ArrayList<>(layout.getRecordCount());
			for (int i = 0; i < layout.getRecordCount(); i++) {
				if (layout.getRecord(i).getParentRecordIndex() < 0) {
					indexs.add(i);
					addChildren(layout, i);
				}
			}
		}
		
		private void addChildren(LayoutDetail layout, int parentIndex) {
			for (int i = 0; i < layout.getRecordCount(); i++) {
				if (i != parentIndex && layout.getRecord(i).getParentRecordIndex() == parentIndex) {
					indexs.add(i);
					addChildren(layout, i);
				}
			}
		}

		@Override
		public int getNextIndex() {
			return idx < indexs.size() ? indexs.get(idx++) : -1;
		}

		@Override
		public void resetIndex() {
			idx = 0;
		}

		@Override
		public boolean isMoreIndexs() {
			return idx < indexs.size();
		}
		
	}
}
