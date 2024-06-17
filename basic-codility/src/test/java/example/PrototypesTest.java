package example;
/**
 * Copyright 2024 Serguei Kouzmine
 */

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.Set;
import java.util.HashSet;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;

public class PrototypesTest {

	public class Complex extends ComplexPrototype {

		private final String data;
		private final String text;
		private int position;

		Complex(String data, String text) {
			this.data = data;
			this.text = text;
		}

		public int getPosition() {
			return position;
		}

		public void setPosition(int value) {
			position = value;
		}

		public String getData() {
			return data;
		}

		public String getText() {
			return text;
		}

		@Override
		public Complex copy() {
			Complex complexClone = new Complex(this.getData(), this.getText());
			complexClone.setPosition(this.getPosition());
			return complexClone;
		}

	}

	public abstract class ComplexPrototype {

		public abstract Complex copy();
	}

	@Test
	public void givenAPlasticTreePrototypeWhenClonedThenCreateA_Clone() {
		// ...

		Complex p = new Complex("data", "text");
		final int position = 10;
		p.setPosition(position);
		Complex q = (Complex) p.copy();
		final int otherPosition = 20;
		q.setPosition(otherPosition);

		assertEquals(position, p.getPosition());
		assertEquals(otherPosition, q.getPosition());
	}
}
