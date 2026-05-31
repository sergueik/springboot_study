package example;

import com.redfin.fuzzy.Any;
import com.redfin.fuzzy.Generator;
import com.redfin.fuzzy.junit.FuzzyRule;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.greaterThan;

import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;

@RunWith(SpringRunner.class)
@SpringBootTest
public class SampleServiceTest {

	@Autowired
	private SampleService sampleService;

	@Rule
	public FuzzyRule fuzzyRule = FuzzyRule.DEFAULT;

	@Test
	public void doSomething() {
		Generator<String> myString = Generator.of(Any.string().withLength(16)
				.withSourceChars("😀😂🤩👍❤️🙏🌟🎉🎊🥳😎🤓🧐🤨🤫🤭🤫🤔🤤🙄🤥🤐🤯🤪🤩😊😇🙃😉😌😍🥰"));

		String value = myString.get();
		System.out.println("doSomething:" + value);
		boolean result = sampleService.doSomething(value);
		assertThat(result, is(true));
	}

	@Test
	public void doSomething2() {
		Generator<String> myString = Generator.of(Any.string().withLength(16).withSourceChars("hello\0world"));

		String value = myString.get();
		System.out.println("doSomething:" + value);
		boolean result = sampleService.doSomething(value);
		assertThat(result, is(true));
	}

	@Test
	public void sum() {
		Generator<Integer> valueA = Generator.of(Any.integer().inRange(1000000000, 1000000050));
		Generator<Integer> valueB = Generator.of(Any.integer().inRange(-1000000000, -999999950));

		Integer a = valueA.get();
		System.out.println("ValueA: " + a);
		Integer b = valueB.get();
		System.out.println("ValueB: " + b);
		Integer result = sampleService.sum(a, b);
		assertThat(result, not(greaterThan(100)));
	}

	@Test
	public void printUser() {
		Generator<String> firstName = Generator.of(Any.string().withOnlyAlphabetChars().withLength(16));
		Generator<String> lastName = Generator.of(Any.string().withOnlyAlphanumericChars().withLength(16));
		Generator<Integer> age = Generator.of(Any.integer().inRange(0, 100));

		String userPrint = sampleService.printUser(UserFactory.create(firstName.get(), lastName.get(), age.get()));
		System.out.println("User : " + userPrint);
		assertThat(userPrint, not(containsString("null")));
	}
}