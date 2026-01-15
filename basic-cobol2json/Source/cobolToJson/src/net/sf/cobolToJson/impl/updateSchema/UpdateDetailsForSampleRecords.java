package net.sf.cobolToJson.impl.updateSchema;

import java.util.List;

import net.sf.JRecord.schema.GroupUpdateDetails;
import net.sf.JRecord.schema.IGroupUpdateDetails;
import net.sf.JRecord.schema.IItemUpdateDetails;

public class UpdateDetailsForSampleRecords implements IGroupUpdateDetails {

	private static final IItemUpdateDetails DEFAULT_UPDATE_DETAIL = GroupUpdateDetails.DEFAULT_UPDATE_DETAIL;
	private final IGroupUpdateDetails sourceUpdateDetails;
	private final ExitManager exitManager;
	
	
	public UpdateDetailsForSampleRecords(IGroupUpdateDetails sourceUpdateDetails, boolean simulateExits) {
		super();
		this.sourceUpdateDetails = sourceUpdateDetails;
		this.exitManager = simulateExits ? new ExitManager() : null;
	}


	@Override
	public IItemUpdateDetails getUpdateDetails(List<String> names) {
		IItemUpdateDetails updateDetails = sourceUpdateDetails.getUpdateDetails(names);
		return updateDetails == null || updateDetails == DEFAULT_UPDATE_DETAIL
				? DEFAULT_UPDATE_DETAIL 
				: new ItemUpdateDetails(exitManager, updateDetails);
	}

	public ExitManager getExitManager() {
		return exitManager;
	}	
}
