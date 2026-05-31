### Info

replica of 
[fuzzytesting](https://github.com/PoisonedYouth/fuzzytesting)


Test implementation for imperative usage of fuzzy testing in maven java project
### Standalone

Without Spring the annotation is:
```java
import com.redfin.fuzzy.junit.Fuzzy;
import com.redfin.fuzzy.junit.Fuzz;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

@ExtendWith(Fuzzy.class)
public class MyFuzzyTest {

    @Test
    void testWithFuzzyStrings(@Fuzz String fuzzString) {
        // fuzzString is automatically generated
        Assertions.assertNotNull(fuzzString);
        // Your property/assertions here
    }
}
```
The `@Fuzz` annotation marks parameters to be fuzzed.

`@ExtendWith(Fuzzy.class)` hooks the fuzzing extension into JUnit.


```xml
<plugin>
  <groupId>com.code-intelligence</groupId>
  <artifactId>jazzer-maven-plugin</artifactId>
  <version>0.16.0</version>
  <executions>
    <execution>
      <goals>
        <goal>fuzz</goal>
      </goals>
    </execution>
  </executions>
</plugin>

```
```xml
<plugin>
  <groupId>edu.berkeley.cs.jqf</groupId>
  <artifactId>jqf-maven-plugin</artifactId>
  <version>1.7</version>
  <executions>
    <execution>
      <goals>
        <goal>fuzz</goal>
      </goals>
    </execution>
  </executions>
</plugin>

```
### See Also

	 * https://github.com/redfin/fuzzy#user-content-use-with-junit
     * [Jazzer — Java fuzzing with libFuzzer backend](https://github.com/CodeIntelligenceTesting/jazzer)
     * https://gitlab.com/gitlab-org/security-products/analyzers/fuzzers/javafuzz
     * [JQF (Java Quick Fuzz)](https://github.com/rohanpadhye/JQF)

     * [Fuzz testing is a software testing technique..](https://github.com/baileyfu/fuzzing4j)
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
