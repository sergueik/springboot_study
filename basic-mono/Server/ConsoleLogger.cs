using System;
using System.Linq;
using Utils;

namespace Program {
    public class ConsoleLogger:ILogger
    {
        public void Log(object message)
        {
            Console.WriteLine(message);
        }
    }
}
