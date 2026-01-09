## Info


Why this happens

This is not about underscores anymore. It’s actually a Java regex parser limitation:

Java regex has a maximum compiled regex size, and extremely large or complex patterns can trigger misleading PatternSyntaxExceptions.

Named groups are especially sensitive — if the parser sees a long chain of groups, sometimes it reports the wrong exception at the point where the internal regex state machine fails.

Your patterns are growing incrementally (147 → 166 → 190+), so eventually the parser trips on the cumulative structure, even if every individual group is valid.

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
