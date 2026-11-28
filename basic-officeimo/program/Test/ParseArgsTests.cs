using System;
using System.Text;
using System.Linq;
using System.Collections.Generic;


using NUnit.Framework;
using Program;
using Utils;

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
		public void test7()
		{
			parseArgs = new ParseArgs("-arg_multi=a,b,c");

			var result = parseArgs.GetMacro("arg_multi");
			Assert.IsNotNull(result);
			StringAssert.Contains(",", result);
			Console.Error.WriteLine("result: " + result);
		}

		[Test]
		public void test8()
		{
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
			Assert.IsTrue(Enum.TryParse<Program.ExtractionTarget>(argument, true, out result) && Enum.IsDefined(typeof(Program.ExtractionTarget), result));
		}

		[Test]
		public void test9() {
			var data = new List<Program.ExtractionTarget>();
			var values = Enum.GetValues(typeof(Program.ExtractionTarget));
			int number = values.Length > 4 ? 4 : values.Length;

			while (number > data.Count) {
				var randomTarget = (Program.ExtractionTarget)values.GetValue(random.Next(values.Length));
				if (!data.Contains(randomTarget))
					data.Add(randomTarget);
			}
			// NOTE: `Append` creates a new
			data.Add(data[0]);
			var targets = Program.Program.ParseExtractionTargets(new ParseArgs(String.Format("-arg_enum={0}", String.Join(",", data.Select(element => element.ToString()).ToList().ToArray()))), "arg_enum");
			Assert.AreEqual(number, targets.Count());
			foreach (Program.ExtractionTarget target in data){	Assert.Contains(target, targets); }
		}

		[Test]
		// refactored test9
		public void test10() {
			var data = new List<Program.ExtractionTarget>();

			int number = Math.Min( 4, Enum.GetValues(typeof(Program.ExtractionTarget)).Length);

			while (data.Count < number) {
				Program.ExtractionTarget target = GetRandomEnum<Program.ExtractionTarget>();
				if (!data.Contains(target))
					data.Add(target);
			}

			// Deliberately introduce a duplicate.
			data.Add(data[0]);

			Program.ExtractionTarget[] targets = Program.Program.ParseExtractionTargets( new ParseArgs("-arg_enum=" + String.Join(",", data.Select(element => element.ToString()).ToArray())), "arg_enum");

			Assert.AreEqual(number, targets.Length);
		}

		// Reuse a single Random instance: creating a new Random in every iteration can produce repeated values
		private static readonly Random random = new Random();
		// NOTE: c# 5.0 Error CS0702: Constraint cannot be special class 'System.Enum'
		
		private static T GetRandomEnum<T>() where T : struct {
			System.Type typeOfT = typeof(T);

			if (!typeOfT.IsEnum)
				throw new ArgumentException("Generic T must be an enum");

			Array values = Enum.GetValues(typeOfT);

			int index = random.Next(values.Length);

			return (T)values.GetValue(index);
		}
	}
}
