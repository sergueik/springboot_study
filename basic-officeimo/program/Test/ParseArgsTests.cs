using System;
using System.Text;
using System.Linq;
using Utils;

using NUnit.Framework;
using Program;

namespace Test
{

	[TestFixture]
	public class ParseArgsTests
	{
		private StringBuilder verificationErrors = new StringBuilder();
		private ParseArgs parseArgs;
		private static readonly string url = "http://www.google.com?q=123";
		private static readonly int number = 42;
		private static readonly int num_tokens = 5;
		private static readonly string commandLine = String.Format("-url:{0} -number:{1} -debug -arg_with_space=\"a test\" -arg_with_repeated_spaces=\"another test  with    space\"", url, number);

		[SetUp]
		public void setUp()
		{
			verificationErrors.Clear();
			parseArgs = new ParseArgs(commandLine);
		}

		[TearDown]
		public void tearDown()
		{
			Assert.AreEqual("", verificationErrors.ToString());
		}

		[Test]
		public void test1()
		{
			String result = parseArgs.GetMacro("url");
			StringAssert.Contains(url, result);
			Assert.AreEqual(url, result);
		}

		[Test]
		public void test2()
		{
			var result = parseArgs.GetMacro("debug");
			Assert.IsNotNull(result);
			StringAssert.AreEqualIgnoringCase("TRUE", result);
			var debug = Boolean.Parse(parseArgs.GetMacro("debug"));
			Assert.AreEqual(true, debug);
		}

		[Test]
		public void test3()
		{
			var result = parseArgs.GetMacro("arg_with_space");
			Assert.IsNotNull(result);
			StringAssert.Contains(" ", result);
			Assert.AreEqual("a test", result);
		}

		[Test]
		public void test4()
		{
			var result = parseArgs.GetMacro("arg_with_repeated_spaces");
			Assert.IsNotNull(result);
			StringAssert.Contains("  ", result);
			Assert.AreEqual("another test  with    space", result);
		}

		[Test]
		public void test5()
		{
			var tokens = ParseArgs.splitTokens(commandLine);
			Assert.AreEqual(num_tokens, tokens.Length);

			Console.Error.WriteLine("Length: " + tokens.Length);
			StringAssert.Contains(" ", tokens[3]);
			StringAssert.Contains("  ", tokens[4]);
			StringAssert.IsMatch("[^ ]+", tokens[0]);
			for (var cnt = 0; cnt != tokens.Length; cnt++)
				Console.Error.WriteLine("token: " + tokens[cnt]);
		}

		[Test]
		public void test6()
		{
			parseArgs = new ParseArgs("-arg_repeated=x -arg_repeated=y -arg_repeated=z");
			var result = parseArgs.GetMacro("arg_repeated");
			Assert.IsNotNull(result);
			Assert.AreEqual("z", result);
			Console.Error.WriteLine("result: " + result);
		}

		[Test]
		public void test7() {
			parseArgs = new ParseArgs("-arg_multi=a,b,c");

			var result = parseArgs.GetMacro("arg_multi");
			Assert.IsNotNull(result);
			StringAssert.Contains(",", result);
			Console.Error.WriteLine("result: " + result);
		}
		
		[Test]
		public void test8() {
			parseArgs = new ParseArgs("-arg_enum=modules");
			var argument = parseArgs.GetMacro("arg_enum");
			Console.Error.WriteLine(String.Format("argument: \"{0}\"", argument));
			Assert.IsNotNull(argument);
			// Error CS0453: The type 'Program.ExtractionTarget?' must be a non-nullable value type in order to use it as parameter 'TEnum' in the generic type or method 'System.Enum.TryParse<TEnum>(string, bool, out TEnum)'
			// Nullable<Program.ExtractionTarget> result = null;
			// if (Enum.TryParse<Nullable<Program.ExtractionTarget>>(argument, true, out result)) {
			Program.ExtractionTarget result = Program.ExtractionTarget.References;
			if (Enum.TryParse<Program.ExtractionTarget>(argument, true, out result)) {
				Console.WriteLine(String.Format("Converted: {0}", result)); // Output: Active
			} else {
				Console.WriteLine(String.Format("Invalid argument string:\"{0}\"", argument));
			}
			Assert.AreEqual(Program.ExtractionTarget.Modules, result);
			
			// altrenative
			

			Assert.IsTrue(Enum.TryParse<Program.ExtractionTarget>(
				argument, true, out result)
			&& Enum.IsDefined(typeof(Program.ExtractionTarget), result));
		}
		
		[Test]
		public void test9() {
		}
		
		
	}
}
