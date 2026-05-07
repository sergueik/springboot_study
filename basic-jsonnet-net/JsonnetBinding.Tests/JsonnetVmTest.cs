using System;
using System.Collections.Generic;
using System.IO;
using NUnit.Framework;
using Utils;

namespace JsonnetBinding.Tests {
	[TestFixture]
	public class JsonnetVmTest {
		protected JsonnetVm Vm = new JsonnetVm();
	/*	
		[OneTimeSetUp]
		public void Setup() {
			try {
				Vm = new JsonnetVm();
			} catch (Exception ex) {
				Console.WriteLine(ex.ToString());
				throw;
			}	
		}

*/
		[Test]
		public void EvaluateFile()
		{
			var filename = Path.GetTempFileName();
			File.WriteAllText(filename, "{ x: 1 , y: self.x + 1 } { x: 10 }");
			var result = Vm.EvaluateFile(filename);

			Assert.AreEqual(@"{
   ""x"": 10,
   ""y"": 11
}
", result);
		}
        
		[Test]
		public void ErrorEvaluatingThrowsException()
		{
			var ex = Assert.Throws<JsonnetException>(() =>
                Vm.EvaluateSnippet("test.jsonnet", "{ x: 1 , y: self.x / 0 } { x: 10 }"));
            
			StringAssert.StartsWith(ex.Message, "RUNTIME ERROR: division by zero.");
		}

		[Test]
		public void MaxStack()
		{
			Vm.MaxStack = 2;
            
			var snippet = @"
{
    a: { x: 0 },
    b: self.a { x +: 1 },
    c: self.b { x +: 1 } ,
    d: self.c { x +: 1 } 
}";

			var ex = Assert.Throws<JsonnetException>(() => Vm.EvaluateSnippet("test.jsonnet", snippet));

			StringAssert.StartsWith(ex.Message, "RUNTIME ERROR: max stack frames exceeded.");
		}
        
		[Test]
		public void NativeCallbackUsingDelegate()
		{
			Vm.AddNativeCallback("concat", new Func<string, string, object>((a, b) => a + b));
			Vm.AddNativeCallback("return_types", new Func<object>(() =>
                new Dictionary<string, object> {
				{ "a", new object[] { 1, 2, 3, null, new object[] { } } },
				{ "b", 1.0 },
				{ "c", true },
				{ "d", null }, {
					"e", new Dictionary<string, object> {
						{ "x", 1 },
						{ "y", 2 },
						{ "z", new[] { "foo" } },
					}
				}, {
					"f", new
                        {
                            Foo = "bar"
                        }
				}
			}));

			var result = Vm.EvaluateSnippet("test.jsonnet", @"
std.assertEqual(({ x: 1, y: self.x } { x: 2 }).y, 2) &&
std.assertEqual(std.native('concat')('foo', 'bar'), 'foobar') &&
std.assertEqual(std.native('return_types')(), {a: [1, 2, 3, null, []], b: 1, c: true, d: null, e: {x: 1, y: 2, z: ['foo']}, f: { Foo: 'bar' }}) &&
true
");
            
			Assert.AreEqual("true\n", result);
		}

		[Test]
		public void NativeCallbackIsNull()
		{
			var ex = Assert.Throws<ArgumentNullException>(() => Vm.AddNativeCallback("test", null));
			Assert.AreEqual("d", ex.ParamName);
		}

		[Test]
		public void NativeCallbackTypeMismatch()
		{
			Vm.AddNativeCallback("test", new Func<int, string>(s => "aaa"));

			var ex = Assert.Throws<JsonnetException>(() =>
                Vm.EvaluateSnippet("test.jsonnet", "std.native('test')('a')"));
			StringAssert.StartsWith(ex.Message,
				"RUNTIME ERROR: Object of type 'System.String' cannot be converted to type 'System.Int32'.");
		}

		[Test]
		public void NativeCallbackExceptionThown()
		{
			Vm.AddNativeCallback("test", new Func<object, string>(s => {
				throw new Exception("Test error");
			}));

			var ex = Assert.Throws<JsonnetException>(() =>
                Vm.EvaluateSnippet("test.jsonner", "std.native('test')('a')"));
			StringAssert.StartsWith(ex.Message, "RUNTIME ERROR: Test error");
		}
        
		/// <summary>
		/// The import callback is invoked when jsonnet wants to load an external file.
		/// </summary>
		[Test]
		public void ImportCallback()
		{
			Vm.ImportCallback = (string dir, string rel, out string here) => {
				Assert.AreEqual("/some/path/", dir);
				Assert.AreEqual("bar.libsonnet", rel);
				here = "";
				return "42";
			};
            
			GC.Collect();

			var result = Vm.EvaluateSnippet("/some/path/test.jsonnet", "local bar = import 'bar.libsonnet';bar");
            
			Assert.AreEqual("42" + Environment.NewLine, result);
		}

		/// <summary>
		/// The import callback cannot be null.
		/// </summary>
		[Test]
		public void ImportCallbackIsNull()
		{
			Assert.Throws<ArgumentNullException>(() => {
				Vm.ImportCallback = null;
			});
		}

		/// <summary>
		/// The here output argument of the import callback is used in stack traces in the case where there is an error
		/// in the imported file.
		/// </summary>
		[Test]
		public void ImportCallbackReturnsHere()
		{
			Vm.ImportCallback = (string dir, string rel, out string here) => {
				here = "/a/b/bar.libsonnet";
				return "{,}";
			};

			GC.Collect();
			var ex = Assert.Throws<JsonnetException>(() =>
                Vm.EvaluateSnippet("test.jsonnet", "local foo = import 'foo.libsonnet';{'foo': foo}"));

			StringAssert.StartsWith(ex.Message, "STATIC ERROR: /a/b/bar.libsonnet:1:2");
		}
        
		[Test]
		public void ImportCallbackExceptionThrown()
		{
			Vm.ImportCallback = (string dir, string rel, out string here) => {
				throw new Exception("Test error");
			};
            
			GC.Collect();

			var ex = Assert.Throws<JsonnetException>(() =>
                Vm.EvaluateSnippet("test.jsonnet", "import 'test.libjsonnet'"));
			StringAssert.StartsWith(ex.Message,
				"RUNTIME ERROR: couldn't open import \"test.libjsonnet\": Test error");
		}

		[Test]
		public void StringOutput()
		{
			Vm.StringOutput = true;

			var result = Vm.EvaluateSnippet("test.jsonnet", "'123' + '4'");
			Assert.AreEqual("1234" + Environment.NewLine, result);
		}
	}
}