using System;
using System.Runtime.InteropServices;

namespace Utils
{
    /// <summary>
    /// PInvoke calls for libjsonnet. See jsonnet.h for reference:
    /// https://github.com/google/jsonnet/blob/master/include/libjsonnet.h
    /// </summary>
    internal static class NativeMethods
    {
        public delegate IntPtr JsonnetImportCallback(
            IntPtr ctx, string baseDir, string rel, out IntPtr foundHere, out bool success);

        public delegate JsonnetJsonValue JsonnetNativeCallback(
            IntPtr ctx, IntPtr argv, out bool success);
        
        [DllImport("libjsonnet.so")]
        public static extern JsonnetVmHandle jsonnet_make();

        [DllImport("libjsonnet.so")]
        public static extern void jsonnet_max_stack(JsonnetVmHandle vm, uint v);
        
        [DllImport("libjsonnet.so")]
        public static extern void jsonnet_gc_min_objects(JsonnetVmHandle vm, uint v);
        
        [DllImport("libjsonnet.so")]
        public static extern void jsonnet_gc_growth_trigger(JsonnetVmHandle vm, double v);
        
        [DllImport("libjsonnet.so")]
        public static extern void jsonnet_string_output(JsonnetVmHandle vm, int v);

        [DllImport("libjsonnet.so")]
        public static extern IntPtr jsonnet_json_extract_string(JsonnetVmHandle vm, JsonnetJsonValue v);

        [DllImport("libjsonnet.so")]
        public static extern bool jsonnet_json_extract_number(JsonnetVmHandle vm, JsonnetJsonValue v, out double outVal);

        [DllImport("libjsonnet.so")]
        public static extern int jsonnet_json_extract_bool(JsonnetVmHandle vm, JsonnetJsonValue v);
        
        [DllImport("libjsonnet.so")]
        public static extern bool jsonnet_json_extract_null(JsonnetVmHandle vm, JsonnetJsonValue v);

        [DllImport("libjsonnet.so")]
        public static extern JsonnetJsonValue jsonnet_json_make_string(JsonnetVmHandle vm, string v);
        
        [DllImport("libjsonnet.so")]
        public static extern JsonnetJsonValue jsonnet_json_make_number(JsonnetVmHandle vm, double v);
        
        [DllImport("libjsonnet.so")]
        public static extern JsonnetJsonValue jsonnet_json_make_bool(JsonnetVmHandle vm, bool v);
        
        [DllImport("libjsonnet.so")]
        public static extern JsonnetJsonValue jsonnet_json_make_null(JsonnetVmHandle vm);
        
        [DllImport("libjsonnet.so")]
        public static extern JsonnetJsonValue jsonnet_json_make_array(JsonnetVmHandle vm);
        
        [DllImport("libjsonnet.so")]
        public static extern void jsonnet_json_array_append(JsonnetVmHandle vm, JsonnetJsonValue arr, JsonnetJsonValue v);
        
        [DllImport("libjsonnet.so")]
        public static extern JsonnetJsonValue jsonnet_json_make_object(JsonnetVmHandle vm);
        
        [DllImport("libjsonnet.so")]
        public static extern void jsonnet_json_object_append(JsonnetVmHandle vm, JsonnetJsonValue obj, string f, JsonnetJsonValue v);
        
        [DllImport("libjsonnet.so")]
        public static extern void jsonnet_json_destroy(JsonnetVmHandle vm, JsonnetJsonValue v);

        [DllImport("libjsonnet.so")]
        public static extern IntPtr jsonnet_realloc(JsonnetVmHandle vm, IntPtr buf, UIntPtr sz);

        [DllImport("libjsonnet.so")]
        public static extern void jsonnet_import_callback(JsonnetVmHandle vm, JsonnetImportCallback cb, IntPtr ctx);
        
        [DllImport("libjsonnet.so")]
        public static extern void jsonnet_native_callback(
            JsonnetVmHandle vm, string name, JsonnetNativeCallback cb, IntPtr ctx, string[] parms);

        [DllImport("libjsonnet.so")]
        public static extern void jsonnet_ext_var(JsonnetVmHandle vm, string key, string value);

        [DllImport("libjsonnet.so")]
        public static extern void jsonnet_ext_code(JsonnetVmHandle vm, string key, string value);

        [DllImport("libjsonnet.so")]
        public static extern void jsonnet_tla_var(JsonnetVmHandle vm, string key, string value);

        [DllImport("libjsonnet.so")]
        public static extern void jsonnet_tla_code(JsonnetVmHandle vm, string key, string value);
        
        [DllImport("libjsonnet.so")]
        public static extern void jsonnet_max_trace(JsonnetVmHandle vm, uint v);

        [DllImport("libjsonnet.so")]
        public static extern void jsonnet_jpath_add(JsonnetVmHandle vm, string v);
        
        [DllImport("libjsonnet.so")]
        public static extern IntPtr jsonnet_evaluate_file(JsonnetVmHandle vm, string filename, out bool error);
        
        [DllImport("libjsonnet.so")]
        public static extern IntPtr jsonnet_evaluate_snippet(JsonnetVmHandle vm, string filename, string snippet, out bool error);

        [DllImport("libjsonnet.so")]
        public static extern void jsonnet_destroy(IntPtr vm);
    }
}
