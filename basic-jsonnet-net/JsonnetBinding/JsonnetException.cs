using System;

namespace Utils
{
    /// <summary>
    /// Exception thrown when an error is returned from a Jsonnet call.
    /// </summary>
    public class JsonnetException : Exception
    {
        /// <summary>
        /// Initializes a new instance of <see cref="JsonnetException"/>.
        /// </summary>
        /// <param name="message">Response from Jsonnet.</param>
        public JsonnetException(string message) : base(message) { } 
    }
}