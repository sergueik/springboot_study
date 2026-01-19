package net.sf.cobolToJson.impl.updateSchema;

import java.util.ArrayList;
import java.util.List;

import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.schema.jaxb.IItem;
import net.sf.JRecord.schema.jaxb.interfaces.IRedefineSelection;
import net.sf.JRecord.schema.jaxb.interfaces.IWriteCheck;

public class ExitManager {
	
	
	private List<IExit> exits = new ArrayList<>();
	private int updateCount = 0;
	private boolean forceOk = false;

	public IWriteCheck newWriteCheck() {
		return new WriteCheck(this);
	}
	
	public IRedefineSelection newRedefinesSelection() {
		return new RedefinesSelection(this);
	}
	
	
	
	public int getUpdateCount() {
		return updateCount;
	}



	public void setForceOk(boolean forceOk) {
		this.forceOk = forceOk;
	}



	static interface IExit {
		
	}
	
	static class UpdateRecorder {
		private ExitManager manager;
		private boolean doUpdate = true;

		public UpdateRecorder(ExitManager manager) {
			this.manager = manager;
		}
		
		void registerUpdate() {
			if (doUpdate) {
				manager.updateCount += 1;
			}
		}
		
		void remove(IExit exit) {
			if (doUpdate) {
				manager.exits.remove(exit);
				doUpdate = false;
			}
		}
		
		boolean forceOk() {
			return manager.forceOk;
		}

	}
	
	static class WriteCheck implements IWriteCheck, IExit {
		boolean next = true;
		private UpdateRecorder recordUpdates;
		
		public WriteCheck( ExitManager manager) {
			super();
			this.recordUpdates = new UpdateRecorder(manager);
			manager.exits.add(this);
		}

		@Override
		public boolean isOkToWrite(IItem item, AbstractLine line) {
			recordUpdates.registerUpdate();
		
			if (next || recordUpdates.forceOk()) {
				next = false;
				return true;
			}
			recordUpdates.remove(this);
			next = true;
			return false;
		}
	}
	
	static class RedefinesSelection implements IRedefineSelection, IExit {
		int index = -1;
		private UpdateRecorder recordUpdates;
		
		public RedefinesSelection(ExitManager manager) {
			this.recordUpdates = new UpdateRecorder(manager);
			manager.exits.add(this);
		}

		@Override
		public List<IItem> selectRedefinedItemToWrite(List<IItem> redefinedItemGroup, AbstractLine line) {
			recordUpdates.registerUpdate();
			int hold = index++;
			if (hold < 0 || recordUpdates.forceOk()) {
				return redefinedItemGroup;
			}
			ArrayList<IItem> items = new ArrayList<>();
			if (hold < redefinedItemGroup.size()) {
				items.add(redefinedItemGroup.get(hold));
			} else if (hold > redefinedItemGroup.size()) {
				index = 0;
				recordUpdates.remove(this);
			}
			return items;
		}
		
	}
}
