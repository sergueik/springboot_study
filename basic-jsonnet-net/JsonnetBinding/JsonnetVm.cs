using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Text;

namespace Utils
{
	public class JsonnetVm
	{
		private readonly JsonnetVmHandle _handle;
		private readonly IDictionary<string, NativeMethods.JsonnetNativeCallback> _nativeCallbacks =
			new Dictionary<string, NativeMethods.JsonnetNativeCallback>();
		private NativeMethods.JsonnetImportCallback _importCallback;
        
		public JsonnetVm()
		{
						try {

			_handle = NativeMethods.jsonnet_make();
					} catch (Exception ex) {
				Console.WriteLine(ex.ToString());
				throw;
			}	
		}

		/// <summary>
		/// Set the maximum stack depth.
		/// </summary>
		public uint MaxStack {
			set { NativeMethods.jsonnet_max_stack(_handle, value); }
		}

		/// <summary>
		/// Set the number of objects required before a garbage collection cycle is allowed.
		/// </summary>
		public uint GcMinObjects {
			set { NativeMethods.jsonnet_gc_min_objects(_handle, value); }
		}

		/// <summary>
		/// Run the garbage collector after this amount of growth in the number of objects.
		/// </summary>
		public double GcGrowthTrigger {
			set { NativeMethods.jsonnet_gc_growth_trigger(_handle, value); }
		}

		/// <summary>
		/// Set the number of lines of stack trace to display (0 for all of them).
		/// </summary>
		public uint MaxTrace {
			set { NativeMethods.jsonnet_max_trace(_handle, value); }
		}

		/// <summary>
		/// Expect a string as output and don't JSON encode it.
		/// </summary>
		public bool StringOutput {
			set { NativeMethods.jsonnet_string_output(_handle, value ? 1 : 0); }
		}
        
		public ImportCallback ImportCallback {
			set {
				if (value == null)
					throw new ArgumentNullException("the value is null");
                
				_importCallback = (IntPtr ctx, string dir, string rel, out IntPtr here, out bool success) => {
					try {
						string foundHere;
						var result = value(dir, rel, out foundHere);
						// TODO: Make sure that these strings are de-allocated in a failure
						here = AllocJsonnetString(_handle, foundHere);
						success = true;
						return AllocJsonnetString(_handle, result);
					} catch (Exception e) {
						success = false;
						here = IntPtr.Zero;
						return AllocJsonnetString(_handle, e.Message);
					}
				};
				NativeMethods.jsonnet_import_callback(_handle, _importCallback, IntPtr.Zero);
			}
		}

		public string EvaluateFile(string filename)
		{
			bool error;
			var result = NativeMethods.jsonnet_evaluate_file(_handle, filename, out error);
			var resultString = MarshalAndDeallocateString(result);

			if (error)
				throw new JsonnetException(resultString);

			return resultString;
		}
        
		public string EvaluateSnippet(string filename, string snippet)
		{
			bool error;
			var result = NativeMethods.jsonnet_evaluate_snippet(_handle, filename, snippet, out error);
			var resultString = MarshalAndDeallocateString(result);

			if (error)
				throw new JsonnetException(resultString);

			return resultString;
		}

		public JsonnetVm AddExtVar(string key, string value)
		{
			NativeMethods.jsonnet_ext_var(_handle, key, value);
			return this;
		}

		public JsonnetVm AddExtCode(string key, string value)
		{
			NativeMethods.jsonnet_ext_code(_handle, key, value);
			return this;
		}

		public JsonnetVm AddTlaVar(string key, string value)
		{
			NativeMethods.jsonnet_tla_var(_handle, key, value);
			return this;
		}

		public JsonnetVm AddTlaCode(string key, string value)
		{
			NativeMethods.jsonnet_tla_code(_handle, key, value);
			return this;
		}
       

		public JsonnetVm AddNativeCallback(string name, Delegate d)
		{
			if (d == null)
				throw new ArgumentNullException("delegate is not defined");

			var parameters = d.Method.GetParameters();

			NativeMethods.JsonnetNativeCallback NativeCallback = (IntPtr ctx, IntPtr argv, out bool success) => {
				try {
					var convertedArgs = MarshalNativeCallbackArgs(argv, parameters);
					var result = d.Method.Invoke(d.Target, convertedArgs);
					success = true;
					return JsonConvert.ConvertToNative(_handle, result);
				} catch (TargetInvocationException ex) {
					success = false;
					return JsonConvert.ConvertToNative(
						_handle,
						ex.InnerException != null ? ex.InnerException.Message : null
					);
				} catch (Exception ex) {
					success = false;
					return JsonConvert.ConvertToNative(_handle, ex.Message);
				}
			};

			var parameterNames = parameters
        .Select(p => p.Name)
        .Concat(new string[] { null })   // Append not available in C# 5
        .ToArray();

			NativeMethods.jsonnet_native_callback(
				_handle,
				name,
				NativeCallback,
				IntPtr.Zero,
				parameterNames);

			// Prevent GC
			_nativeCallbacks[name] = NativeCallback;

			return this;
		}

		private object[] MarshalNativeCallbackArgs(IntPtr argv, ParameterInfo[] parameters)
		{
			// argv is a pointer to a null-terminated array of arguments, however we know that the length of the array
			// will be equal to the number of parameters on the method as the jsonnet vm validates that the number of
			// arguments supplied matches the number of arguments on the native callback
			if (parameters.Length == 0)
				return Compat.EmptyArray<object>();
			// requires 4.6.2
				// return Array.Empty<object>();
			var args = new IntPtr[parameters.Length];
			Marshal.Copy(argv, args, 0, parameters.Length);
			return parameters
                .Select((t, i) => JsonConvert.ConvertNativeArgumentToManaged(_handle, new JsonnetJsonValue(args[i])))
                .ToArray();
		}

		/// <summary>
		/// Marshal the contents of a string returned from Jsonnet, then de-allocates it.
		/// </summary>
		private string MarshalAndDeallocateString(IntPtr result)
		{
			try {
            
				return Marshal.PtrToStringAnsi(result);
			} finally {
				NativeMethods.jsonnet_realloc(_handle, result, UIntPtr.Zero);    
			}
		}

		/// <summary>
		/// Allocates a Jsonnet string.
		/// </summary>
		/// <param name="vm"></param>
		/// <param name="value">Value to give the string.</param>
		/// <returns>IntPtr to the allocated Jsonnet string</returns>
		/// <remarks> 
		/// This method allocates a Jsonnet string (using jsonnet_realloc) of the correct length for the supplied
		/// string, and then copies the supplied value into the allocated string.
		/// </remarks>
		private static IntPtr AllocJsonnetString(JsonnetVmHandle vm, string value)
		{

    var bytes = Encoding.ASCII
        .GetBytes(value)
        .Concat(new byte[] { 0 })
        .ToArray();
 // need 4.6.2
			// var bytes = Encoding.ASCII.GetBytes(value).Append((byte)0).ToArray();

			var result = NativeMethods.jsonnet_realloc(vm, IntPtr.Zero, new UIntPtr((uint)bytes.Length + 1));
			Marshal.Copy(bytes, 0, result, bytes.Length);
			return result;
		}
	}
}
