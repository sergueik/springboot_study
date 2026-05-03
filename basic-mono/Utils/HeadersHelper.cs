using System;
using System.ComponentModel;
using System.Linq;

namespace Utils {
    public static class HeadersHelper {
        public static string GetDescription(this Enum value) {
            var valueType = value.GetType();
            var memberName = Enum.GetName(valueType, value);
            if (memberName == null) return null;
            var fieldInfo = valueType.GetField(memberName);
            var attribute = Attribute.GetCustomAttribute(fieldInfo, typeof(DescriptionAttribute));
            return (attribute == null) ? null : (attribute as DescriptionAttribute).Description;
        }
    }
}