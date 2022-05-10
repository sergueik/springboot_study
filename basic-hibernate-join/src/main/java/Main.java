import java.util.Iterator;
import java.util.List;

import org.hibernate.Query;
import org.hibernate.Session;
import org.hibernate.SessionFactory;

import com.otp.hibernate.pojo.HibernateUtility;

public class Main {

	@SuppressWarnings({ "rawtypes", "unchecked" })
	public static void main(String[] args) {
		SessionFactory factory = HibernateUtility.getSessionFactory();
		Session session = factory.openSession();
		// HQL
		Query query = session.createQuery(
				"select c.customerName, c.customerCity, i.itemName,i.price from Customer c "
						+ "left join c.items i");
		List<Object[]> objectList = query.list();
		Iterator<Object[]> objectIterator = objectList.iterator();
		while (objectIterator.hasNext()) {
			Object rows[] = (Object[]) objectIterator.next();
			System.out.println(
					rows[0] + " -- " + rows[1] + "--" + rows[2] + "--" + rows[3]);
		}
		session.clear();
		System.err.println("cleared session");
		session.close();
		System.err.println("closed session");
		// NOTE: appears hanging after "closed session"
		System.exit(0);
	}

}
