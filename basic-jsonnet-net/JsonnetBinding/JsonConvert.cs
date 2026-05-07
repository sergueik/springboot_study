using System;
using System.Collections;
using System.Collections.Generic;
using System.Runtime.InteropServices;

namespace Utils
{
    /// <summary>
    /// Helper methods for converting JsonnetJsonValues to and from managed objects.  
    /// </summary>
    internal static class JsonConvert
    {
    	public static JsonnetJsonValue ConvertToNative(JsonnetVmHandle vm, object v)
{
    if (v == null)
    {
        return NativeMethods.jsonnet_json_make_null(vm);
    }

    var str = v as string;
    if (str != null)
    {
        return NativeMethods.jsonnet_json_make_string(vm, str);
    }

    if (v is bool)
    {
        return NativeMethods.jsonnet_json_make_bool(vm, (bool)v);
    }

    if (v is int)
    {
        return NativeMethods.jsonnet_json_make_number(vm, (int)v);
    }

    if (v is double)
    {
        return NativeMethods.jsonnet_json_make_number(vm, (double)v);
    }

    var dict = v as IDictionary<string, object>;
    if (dict != null)
    {
        return ConvertDictionaryToNative(vm, dict);
    }

    var enumerable = v as IEnumerable;
    if (enumerable != null)
    {
        return ConvertEnumerableToNative(vm, enumerable);
    }

    return ConvertObjectPropertiesToNative(vm, v);
}

        private static JsonnetJsonValue ConvertDictionaryToNative(JsonnetVmHandle vm, IDictionary<string, object> dictionary)
        {
            var obj = NativeMethods.jsonnet_json_make_object(vm);
            try
            {
                foreach (var val in dictionary)
                    NativeMethods.jsonnet_json_object_append(vm, obj, val.Key, ConvertToNative(vm, val.Value));
                return obj;
            }
            catch
            {
                NativeMethods.jsonnet_json_destroy(vm, obj);
                throw;
            }
        }

        private static JsonnetJsonValue ConvertEnumerableToNative(JsonnetVmHandle vm, IEnumerable enumerable)
        {
            var array = NativeMethods.jsonnet_json_make_array(vm);
            try
            {
                foreach (var val in enumerable)
                    NativeMethods.jsonnet_json_array_append(vm, array, ConvertToNative(vm, val));
                return array;
            }
            catch
            {
                NativeMethods.jsonnet_json_destroy(vm, array);
                throw;
            }
        }

        private static JsonnetJsonValue ConvertObjectPropertiesToNative(JsonnetVmHandle vm, object v)
        {
            var obj = NativeMethods.jsonnet_json_make_object(vm);
            try
            {
                foreach (var p in v.GetType().GetProperties())
                    NativeMethods.jsonnet_json_object_append(vm, obj, p.Name, ConvertToNative(vm, p.GetValue(v)));
                return obj;
            }
            catch
            {
                NativeMethods.jsonnet_json_destroy(vm, obj);
                throw;
            }
        }

        /// <summary>
        /// Converts a native jsonnet value supplied to a native method into its managed equivalent.
        /// </summary>
        public static object ConvertNativeArgumentToManaged(JsonnetVmHandle vm, JsonnetJsonValue v)
        {
            if (NativeMethods.jsonnet_json_extract_null(vm, v))
                return null;

            var str = NativeMethods.jsonnet_json_extract_string(vm, v);
            if (str != IntPtr.Zero)
            {
                // TODO: I Don't understand why this works, 
                // but putting the return type on the method as string does not
                return Marshal.PtrToStringAnsi(str);
            }
            double val = 0;
            if (NativeMethods.jsonnet_json_extract_number(vm, v, out val))
                return val;

            var b = NativeMethods.jsonnet_json_extract_bool(vm, v);
            if (b == 0) return false;
            if (b == 1) return true;
            
            throw new InvalidOperationException("Unknown JsonnetJsonValue");
        }
    }
}