### Info

replica of [JsonnetBinding-Dotnet](https://github.com/JustinPealing/JsonnetBinding-Dotnet) the .NET Bindings for [Jsonnet](https://jsonnet.org/)
running on mono

### Usage
```sh
IMAGE=basic-jsonnet-net
docker pull ubuntu:22.04
docker build -f Dockerfile -t $IMAGE .
```
followed by
```sh
NAME=$IMAGE
docker run --name $NAME --rm $IMAGE
```

```text
NUnit Console Runner 3.12.0 (.NET 2.0)
Copyright (c) 2021 Charlie Poole, Rob Prouse
Thursday, 07 May 2026 13:40:22

labels=All is deprecated and will be removed in a future release. Please use labels=Before instead.

Runtime Environment
   OS Version: Linux 5.15.0.177 
   Runtime: .NET Framework CLR v4.0.30319.42000

Test Files
    /WorkSpace/JsonnetBinding.Tests/bin/Debug/Tests.dll

=> JsonnetBinding.Tests.JsonnetVmTest.ErrorEvaluatingThrowsException
=> JsonnetBinding.Tests.JsonnetVmTest.EvaluateFile
=> JsonnetBinding.Tests.JsonnetVmTest.ImportCallback
=> JsonnetBinding.Tests.JsonnetVmTest.ImportCallbackExceptionThrown
=> JsonnetBinding.Tests.JsonnetVmTest.ImportCallbackIsNull
=> JsonnetBinding.Tests.JsonnetVmTest.ImportCallbackReturnsHere
=> JsonnetBinding.Tests.JsonnetVmTest.MaxStack
=> JsonnetBinding.Tests.JsonnetVmTest.NativeCallbackExceptionThown
=> JsonnetBinding.Tests.JsonnetVmTest.NativeCallbackIsNull
=> JsonnetBinding.Tests.JsonnetVmTest.NativeCallbackTypeMismatch
=> JsonnetBinding.Tests.JsonnetVmTest.NativeCallbackUsingDelegate
=> JsonnetBinding.Tests.JsonnetVmTest.StringOutput

Errors, Failures and Warnings

1) Failed : JsonnetBinding.Tests.JsonnetVmTest.ErrorEvaluatingThrowsException
  Expected: String starting with "RUNTIME ERROR: division by zero.
	test.jsonnet:1:13-23	object <anonymous>
	During manifestation	
"
  But was:  "RUNTIME ERROR: division by zero."
at NUnit.Framework.StringAssert.StartsWith (System.String expected, System.String actual, System.String message, System.Object[] args) [0x00007] in <497e7c0354334ae6b2579b219192bcf0>:0
at NUnit.Framework.StringAssert.StartsWith (System.String expected, System.String actual) [0x00000] in <497e7c0354334ae6b2579b219192bcf0>:0
at JsonnetBinding.Tests.JsonnetVmTest.ErrorEvaluatingThrowsException () [0x00013] in /WorkSpace/JsonnetBinding.Tests/JsonnetVmTest.cs:43

2) Failed : JsonnetBinding.Tests.JsonnetVmTest.ImportCallbackExceptionThrown
  Expected: String starting with "RUNTIME ERROR: couldn't open import "test.libjsonnet": Test error
	test.jsonnet:1:1-25	
"
  But was:  "RUNTIME ERROR: couldn't open import "test.libjsonnet": Test error"
at NUnit.Framework.StringAssert.StartsWith (System.String expected, System.String actual, System.String message, System.Object[] args) [0x00007] in <497e7c0354334ae6b2579b219192bcf0>:0
at NUnit.Framework.StringAssert.StartsWith (System.String expected, System.String actual) [0x00000] in <497e7c0354334ae6b2579b219192bcf0>:0
at JsonnetBinding.Tests.JsonnetVmTest.ImportCallbackExceptionThrown () [0x00044] in /WorkSpace/JsonnetBinding.Tests/JsonnetVmTest.cs:188

3) Failed : JsonnetBinding.Tests.JsonnetVmTest.ImportCallbackReturnsHere
  Expected: String starting with "STATIC ERROR: /a/b/bar.libsonnet:1:2: unexpected: "," while parsing field definition
"
  But was:  "STATIC ERROR: /a/b/bar.libsonnet:1:2"
at NUnit.Framework.StringAssert.StartsWith (System.String expected, System.String actual, System.String message, System.Object[] args) [0x00007] in <497e7c0354334ae6b2579b219192bcf0>:0
at NUnit.Framework.StringAssert.StartsWith (System.String expected, System.String actual) [0x00000] in <497e7c0354334ae6b2579b219192bcf0>:0
at JsonnetBinding.Tests.JsonnetVmTest.ImportCallbackReturnsHere () [0x00044] in /WorkSpace/JsonnetBinding.Tests/JsonnetVmTest.cs:174

4) Failed : JsonnetBinding.Tests.JsonnetVmTest.MaxStack
  Expected: String starting with "RUNTIME ERROR: max stack frames exceeded.
	test.jsonnet:4:15-25	object <anonymous>
	test.jsonnet:5:15-25	object <anonymous>
	test.jsonnet:6:15-25	object <anonymous>
	test.jsonnet:6:8-25	object <anonymous>
	During manifestation	
"
  But was:  "RUNTIME ERROR: max stack frames exceeded."
at NUnit.Framework.StringAssert.StartsWith (System.String expected, System.String actual, System.String message, System.Object[] args) [0x00007] in <497e7c0354334ae6b2579b219192bcf0>:0
at NUnit.Framework.StringAssert.StartsWith (System.String expected, System.String actual) [0x00000] in <497e7c0354334ae6b2579b219192bcf0>:0
at JsonnetBinding.Tests.JsonnetVmTest.MaxStack () [0x00038] in /WorkSpace/JsonnetBinding.Tests/JsonnetVmTest.cs:61

5) Failed : JsonnetBinding.Tests.JsonnetVmTest.NativeCallbackExceptionThown
  Expected: String starting with "RUNTIME ERROR: Test error
	test.jsonner:1:1-24	
"
  But was:  "RUNTIME ERROR: Test error"
at NUnit.Framework.StringAssert.StartsWith (System.String expected, System.String actual, System.String message, System.Object[] args) [0x00007] in <497e7c0354334ae6b2579b219192bcf0>:0
at NUnit.Framework.StringAssert.StartsWith (System.String expected, System.String actual) [0x00000] in <497e7c0354334ae6b2579b219192bcf0>:0
at JsonnetBinding.Tests.JsonnetVmTest.NativeCallbackExceptionThown () [0x00043] in /WorkSpace/JsonnetBinding.Tests/JsonnetVmTest.cs:124

6) Failed : JsonnetBinding.Tests.JsonnetVmTest.NativeCallbackIsNull
  Expected string length 1 but was 23. Strings differ at index 1.
  Expected: "d"
  But was:  "delegate is not defined"
  ------------^
at JsonnetBinding.Tests.JsonnetVmTest.NativeCallbackIsNull () [0x00013] in /WorkSpace/JsonnetBinding.Tests/JsonnetVmTest.cs:101

7) Failed : JsonnetBinding.Tests.JsonnetVmTest.NativeCallbackTypeMismatch
  Expected: String starting with "RUNTIME ERROR: Object of type 'System.String' cannot be converted to type 'System.Int32'.
	test.jsonnet:1:1-24	
"
  But was:  "RUNTIME ERROR: Object of type 'System.String' cannot be converted to type 'System.Int32'."
at NUnit.Framework.StringAssert.StartsWith (System.String expected, System.String actual, System.String message, System.Object[] args) [0x00007] in <497e7c0354334ae6b2579b219192bcf0>:0
at NUnit.Framework.StringAssert.StartsWith (System.String expected, System.String actual) [0x00000] in <497e7c0354334ae6b2579b219192bcf0>:0
at JsonnetBinding.Tests.JsonnetVmTest.NativeCallbackTypeMismatch () [0x00043] in /WorkSpace/JsonnetBinding.Tests/JsonnetVmTest.cs:111

8) Error : JsonnetBinding.Tests.JsonnetVmTest.NativeCallbackUsingDelegate
Utils.JsonnetException : RUNTIME ERROR: max stack frames exceeded.
		function <anonymous>
	test.jsonnet:2:1-53	
  at Utils.JsonnetVm.EvaluateSnippet (System.String filename, System.String snippet) [0x0002a] in /WorkSpace/JsonnetBinding/JsonnetVm.cs:108 
  at JsonnetBinding.Tests.JsonnetVmTest.NativeCallbackUsingDelegate () [0x00061] in /WorkSpace/JsonnetBinding.Tests/JsonnetVmTest.cs:87 
  at (wrapper managed-to-native) System.Reflection.RuntimeMethodInfo.InternalInvoke(System.Reflection.RuntimeMethodInfo,object,object[],System.Exception&)
  at System.Reflection.RuntimeMethodInfo.Invoke (System.Object obj, System.Reflection.BindingFlags invokeAttr, System.Reflection.Binder binder, System.Object[] parameters, System.Globalization.CultureInfo culture) [0x0006a] in <d636f104d58046fd9b195699bcb1a744>:0 

Run Settings
    DisposeRunners: True
    InternalTraceLevel: Debug
    WorkDirectory: /tmp
    ImageRuntimeVersion: 4.0.30319
    ImageTargetFrameworkName: .NETFramework,Version=v4.5
    ImageRequiresX86: False
    ImageRequiresDefaultAppDomainAssemblyResolver: False
    TargetRuntimeFramework: mono-4.0
    NumberOfTestWorkers: 2

Test Run Summary
  Overall result: Failed
  Test Count: 12, Passed: 4, Failed: 8, Warnings: 0, Inconclusive: 0, Skipped: 0
    Failed Tests - Failures: 7, Errors: 1, Invalid: 0
  Start time: 2026-05-07 13:40:22Z
    End time: 2026-05-07 13:40:23Z
    Duration: 0.792 seconds
```

this indicates definite progress in running the application :

* `libjsonnet.so` is loading correctly
* P/Invoke signatures are mostly correct
* callbacks are executing
* strings are crossing native/managed boundary successfully
* memory ownership is probably mostly right


while observed errors are now higher-level semantic mismatches between JSONnet, Mono and .Net

* Different libjsonnet version
* Different VM settings
* Different API usage path
* Truncated string due to embedded NUL
* Wrong ownership/free timing
* UTF8/ANSI marshaling mismatch

### Cleanup

```sh
docker container prune -f
docker image prune -f 
docker image rm $IMAGE ubuntu:22.04
```

### Troubleshooting

```text
 /WorkSpace/JsonnetBinding.Tests/JsonnetBinding.Tests.csproj(89,3): error MSB4019: The imported project "/usr/lib/mono/msbuild/Current/bin/Microsoft.CSharp.Targets" was not found. Confirm that the expression in the Import declaration "/usr/lib/mono/msbuild/Current/bin/Microsoft.CSharp.Targets" is correct, and that the file exists on disk
```
this is side effect of using
```xml
  <Import Project="$(MSBuildBinPath)\Microsoft.CSharp.Targets" />
```
instead of canonical
```xml
  <Import Project="$(MSBuildToolsPath)\Microsoft.CSharp.targets" />
```

removal leads to
```text
  /WorkSpace/JsonnetBinding/JsonnetBinding.csproj : error MSB4057: The target "Build" does not exist in the project.
  /WorkSpace/JsonnetBinding.Tests/JsonnetBinding.Tests.csproj : error MSB4057: The target "Build" does not exist in the project.
```


```text
  /WorkSpace/JsonnetBinding/JsonnetVm.cs(184,18): error CS0117: 'Array' does not contain a definition for 'Empty' [/WorkSpace/JsonnetBinding/JsonnetBinding.csproj]
  /WorkSpace/JsonnetBinding/JsonnetVm.cs(217,47): error CS1061: 'byte[]' does not contain a definition for 'Append' and no accessible extension method 'Append' accepting a first argument of type 'byte[]' could be found
  (are you missing a using directive or an assembly reference?)
  [/WorkSpace/JsonnetBinding/JsonnetBinding.csproj]
```

The missing APIs are:

`Array.Empty<T>()` → introduced in newer framework profiles (__.NET__ __4.6__ / newer __Mono__ profiles)
`LINQ Append()` → introduced much later than __4.5__ (System.Linq extensions from newer __.NET Standard__ / __.NET Core__ era)

The project may still happily compile on __Windows__ targeting __4.5__, bacause one of these is happening:

* Visual Studio is using newer reference assemblies/compiler behavior
* NuGet package brings compatibility shims
* the project was never truly validated on a clean __.NET__ __4.5__ machine.

__Mono__ is being stricter and exposing the real API surface.


### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
