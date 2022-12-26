package example;

public interface SimplePullParser {

	int START_DOCUMENT = 0;
	int END_DOCUMENT = 1;
	int START_TAG = 2;
	int END_TAG = 3;
	int TEXT = 4;
	int CDSECT = 5;
	int ENTITY_REF = 6;
	int LEGACY = 999;

	int getType();
	String getText();
	String getName();
	int getAttributeCount();
	String getAttributeName(int index);
	String getAttributeValue(int index);
	String getAttributeValue(String name);
  int next();

}