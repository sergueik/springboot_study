using Microsoft.Win32.SafeHandles;

namespace Utils
{
    /// <summary>
    /// Safe handle implementation for a pointer to the JsonnetVm struct.
    /// </summary>
    internal class JsonnetVmHandle : SafeHandleZeroOrMinusOneIsInvalid
    {
        private JsonnetVmHandle() : base(true) { }

        protected override bool ReleaseHandle()
        {
            NativeMethods.jsonnet_destroy(handle);
            return true;
        }
    }
}