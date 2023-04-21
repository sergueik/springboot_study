package net.arunoday.metric.store.model;

import java.util.Date;

import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;

/**
 * Counter event entity.
 * 
 * @author Aparna Chaudhary
 */
@Document(collection = "counter_events")
public class CounterEventEntity {

	public static final String EVENT_TYPE_FIELD = "eventType";
	public static final String OCCURED_ON_FIELD = "occuredOn";
	public static final String TOTAL_COUNT_FIELD = "totalCount";

	@Id
	private String id;
	private Date occuredOn;
	/** Type of event. */
	private String eventType;
	private double totalCount;
	private ContextData contextData;

	/**
	 * Public Constructor
	 */
	public CounterEventEntity() {
		super();
	}

	public String getId() {
		return id;
	}

	public Date getOccuredOn() {
		return occuredOn;
	}

	public void setOccuredOn(Date occuredOn) {
		this.occuredOn = occuredOn;
	}

	public String getEventType() {
		return eventType;
	}

	public void setEventType(String eventType) {
		this.eventType = eventType;
	}

	public double getTotalCount() {
		return totalCount;
	}

	public void setTotalCount(double totalCount) {
		this.totalCount = totalCount;
	}

	public ContextData getContextData() {
		return contextData;
	}

	public void setContextData(ContextData contextData) {
		this.contextData = contextData;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (obj == null || !getClass().equals(obj.getClass())) {
			return false;
		}
		CounterEventEntity that = (CounterEventEntity) obj;
		return this.getId().equals(that.getId());
	}

	@Override
	public int hashCode() {
		return getId().hashCode();
	}

}
