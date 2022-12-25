package example;

import org.junit.Assert;
import org.junit.Test;

import example.exception.ParseException;

import java.util.ArrayList;
import java.util.List;


// original copyright: fastXml author(https://github.com/fastxml/fastxml)

public class FastXmlParserIntegrationTest {
	private static byte[] xml = ("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
			+ "<root xmlns:env=\"http://www.w3.org/2003/05/soap-envelope\">\n"
			+ "    <env:student env:id=\"1\" env:group=\"1\">\n"
			+ "        <name>张三</name>\n" + "        <sex>mail</sex>\n"
			+ "        <age>18</age>\n" + "        <email>zhangsan@163.com</email>\n"
			+ "        <address city='北京' state='45' zip='012345' country='345'>\n"
			+ "            <line1><![CDATA[L i n e 1 012345 \"你好世界!\"]]></line1>\n"
			+ "            <line2>L i n e 2 012345 012345</line2>\n"
			+ "        </address>\n" + "    </env:student>\n" + "    <env:student>\n"
			+ "        <name>李四</name>\n" + "        <sex>female</sex>\n"
			+ "        <age>18</age>\n" + "        <email>lisi@163.com</email>\n"
			+ "        <address city='杭州' state='' zip='' country='345'>\n"
			+ "            <line1><![CDATA[L i n e 1 012345 \"你好世界!\"]]></line1>\n"
			+ "            <line2>L i n e 2 012345 012345</line2>\n"
			+ "        </address>\n" + "    </env:student>\n" + "</root>").getBytes();

	// tagName and attribute name.
	// In this way, we has no need to decode tagName and attribute name
	private static byte[] root = "root".getBytes();
	private static byte[] env_student = "env:student".getBytes();
	private static byte[] env_id = "env:id".getBytes();
	private static byte[] env_group = "env:group".getBytes();
	private static byte[] name = "name".getBytes();
	private static byte[] sex = "sex".getBytes();
	private static byte[] age = "age".getBytes();
	private static byte[] email = "email".getBytes();
	private static byte[] address = "address".getBytes();
	private static byte[] city = "city".getBytes();

	@Test
	public void test() throws ParseException {
		FastXmlParser parser = FastXmlFactory.newInstance(xml);
		int event = parser.next(); // START_DOCUMENT
		List<Student> students = new ArrayList<Student>(2);
		int nextEvent;
		do {
			if (parser.next() == FastXmlParser.START_TAG && parser.isMatch(root)) { // root
																																							// start
				processStudentElements(parser, students);
				parser.next(); // root end
			}
			nextEvent = parser.getNextEvent();
		} while (nextEvent != FastXmlParser.END_DOCUMENT);

		for (Student s : students) {
			System.out.println(s);
		}

	}

	public static void processStudentElements(FastXmlParser parser,
			List<Student> students) throws ParseException {
		do {
			parser.next(); // START_TAG: "env:student"
			if (parser.isMatch(env_student)) { // tagName has namespace:
																					// "env:student", just consider it as
																					// a string
				Student studentObj = new Student();
				if (parser.getNextEvent() == FastXmlParser.ATTRIBUTE_NAME) {
					processStudentAttributes(parser, studentObj);
				}
				if (parser.getNextEvent() == FastXmlParser.START_TAG) {
					processStudentChildren(parser, studentObj);
				} else {
					throw ParseException.formatError("student info is empty", parser);
				}
				students.add(studentObj);
			}
			parser.next(); // END_TAG: tagName has namespace: "env:student", just
											// consider it as a string
		} while (parser.getNextEvent() == FastXmlParser.START_TAG);
	}

	public static void processStudentAttributes(FastXmlParser parser,
			Student studentObj) throws ParseException {
		do {
			parser.next(); // ATTIBUTE_NAME
			// attribute name has namespace, just consider it as a string: "env:id",
			// "env:group"
			if (parser.isMatch(env_id)
					&& parser.next() == FastXmlParser.ATTRIBUTE_VALUE) {
				studentObj.setId(parser.getLong());
			} else if (parser.isMatch(env_group)
					&& parser.next() == FastXmlParser.ATTRIBUTE_VALUE) {
				studentObj.setGroup(parser.getInt());
			} else {
				throw ParseException.formatError(
						"invalid attribute name: " + parser.getString(), parser);
			}
		} while (parser.getNextEvent() == FastXmlParser.ATTRIBUTE_NAME);
	}

	public static void processStudentChildren(FastXmlParser parser,
			Student studentObj) throws ParseException {
		do {
			parser.next(); // START_TAG
			if (parser.isMatch(name) && parser.getNextEvent() == FastXmlParser.TEXT) {
				parser.next(); // text
				studentObj.setName(parser.getStringWithDecoding()); // need decode
				parser.next(); // END_TAG
			} else if (parser.isMatch(sex)
					&& parser.getNextEvent() == FastXmlParser.TEXT) {
				parser.next(); // text
				studentObj.setSex(parser.getString());
				parser.next(); // END_TAG
			} else if (parser.isMatch(age)
					&& parser.getNextEvent() == FastXmlParser.TEXT) {
				parser.next(); // text
				studentObj.setAge(parser.getInt());
				parser.next(); // END_TAG
			} else if (parser.isMatch(email)
					&& parser.getNextEvent() == FastXmlParser.TEXT) {
				parser.next(); // text
				studentObj.setEmail(parser.getString());
				parser.next(); // END_TAG
			} else if (parser.isMatch(address)) {
				if (parser.getNextEvent() == FastXmlParser.ATTRIBUTE_NAME) {
					processAddressAttributes(parser, studentObj); // get city
				}
				parser.skipCurrentTag();// skip the whole address tag
			}
		} while (parser.getNextEvent() == FastXmlParser.START_TAG);
	}

	public static void processAddressAttributes(FastXmlParser parser,
			Student studentObj) throws ParseException {
		do {
			parser.next();
			if (parser.isMatch(city)
					&& parser.next() == FastXmlParser.ATTRIBUTE_VALUE) {
				studentObj.setCity(parser.getStringWithDecoding());
			} else {
				parser.next();// ATTIBUTE_VALUE
			}
		} while (parser.getNextEvent() == FastXmlParser.ATTRIBUTE_NAME);
	}

	public static class Student {
		private long id;
		private int group;
		private String name;
		private String sex;
		private int age;
		private String email;
		private String city;

		@Override
		public String toString() {
			return "Student{" + "id=" + id + ", group=" + group + ", name='" + name
					+ '\'' + ", sex='" + sex + '\'' + ", age=" + age + ", email='" + email
					+ '\'' + ", city='" + city + '\'' + '}';
		}

		public long getId() {
			return id;
		}

		public void setId(long id) {
			this.id = id;
		}

		public int getGroup() {
			return group;
		}

		public void setGroup(int group) {
			this.group = group;
		}

		public String getName() {
			return name;
		}

		public void setName(String name) {
			this.name = name;
		}

		public String getSex() {
			return sex;
		}

		public void setSex(String sex) {
			this.sex = sex;
		}

		public int getAge() {
			return age;
		}

		public void setAge(int age) {
			this.age = age;
		}

		public String getEmail() {
			return email;
		}

		public void setEmail(String email) {
			this.email = email;
		}

		public String getCity() {
			return city;
		}

		public void setCity(String city) {
			this.city = city;
		}
	}

}
