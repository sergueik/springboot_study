package org.rrd4j.data;

/**
 * Abstract class to be used for custom datasources.
 *
 * <p>If you wish to use a custom datasource in a graph, you should create a class implementing this interface
 * that represents that datasource, and then pass this class on to the RrdGraphDef.</p>
 * @deprecated use implementations of {@link IPlottable} instead
 */
@Deprecated
public abstract class Plottable implements IPlottable {
    /**
     * Retrieves datapoint value based on a given timestamp.
     * Use this method if you only have one series of data in this class.
     *
     * @param timestamp Timestamp in seconds for the datapoint.
     * @return Double value of the datapoint.
     */
    public abstract double getValue(long timestamp);
}
