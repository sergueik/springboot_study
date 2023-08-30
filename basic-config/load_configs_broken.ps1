param(
  # currently unused, placeholder
  [String]$list_url = "/configs/{hostname}/list?newer={last_run_epoch}",
  # server /configs/*/list response mockup
  [string]$filelist_file = ([System.IO.Path]::Combine((resolve-path -path '.').path,'filelist.json')),
  # currently unused, placeholder
  [String]$url = "/load/{filename}?newer={last_run_epoch}",
  [String]$local_config_path = 'configurations',
  [string]$hostname = "${env:COMPUTERNAME}".ToLower(),
  [long]$interval = 10,
  [switch]$debug
)



# the script is using .net class to build Dictionary/List from JSON below.
# origin: https://4sysops.com/archives/convert-json-to-a-powershell-hash-table/
# see also: https://github.com/sergueik/powershell_ui_samples/blob/master/external/parse_json_hash.ps1

# NOTE: found not to work with a simple JSON provided below. 

<#
Found defect with coverting to hash such a minimal JSON:

{
  "status": "OK",
  "result": [
    "file1",
    "file2",
    "file4"
  ]
}

Response:

Name                           Value
----                           -----
result                         {System.Collections.Hashtable}
status                         OK

# TODO: investigate

#>



function ConvertTo-Hashtable {
    [CmdletBinding()]
    [OutputType('hashtable')]
    param (
        [Parameter(ValueFromPipeline)]
        $InputObject
    )

    process {
        if ($null -eq $InputObject) {
            return $null
        }

        if ($InputObject -is [System.Collections.IEnumerable] -and $InputObject -isnot [string]) {
            $collection = @(
               $InputObject | foreach-object { 
                    $object = $_ 
                    ConvertTo-Hashtable -InputObject $object
                }
            )
            Write-Output -NoEnumerate $collection
        } elseif ($InputObject -is [psobject]) { 
            # convertFrom-Json produces System.Management.Automation.PSCustomObject 
            # https://stackoverflow.com/questions/14012773/difference-between-psobject-hashtable-and-pscustomobject#:~:text=%5BPSCustomObject%5D%20is%20a%20type%20accelerator,called%20with%20no%20constructor%20parameters.
            # which properties enumeration is fastest through its PSObject
            $dictionary = @{}
            foreach ($property in $InputObject.PSObject.Properties) {
                $dictionary[$property.Name] = ConvertTo-Hashtable -InputObject $property.Value
            }
            $dictionary
        } else {
            # the object is likely a hash table
            $InputObject
        }
    }
}

$namespace = 'Utils'
$class = 'JSONParser'
if (-not ("${namespace}.${class}" -as [type])) { 

Add-Type -IgnoreWarnings @'
using System;
using System.Collections;
using System.Collections.Generic;
using System.Reflection;
using System.Runtime.Serialization;
using System.Text;
// origin: https://github.com/zanders3/json
namespace Utils {
    // Really simple JSON parser in ~300 lines
    // - Attempts to parse JSON files with minimal GC allocation
    // - Nice and simple "[1,2,3]".FromJson<List<int>>() API
    // - Classes and structs can be parsed too!
    //      class Foo { public int Value; }
    //      "{\"Value\":10}".FromJson<Foo>()
    // - Can parse JSON without type information into Dictionary<string,object> and List<object> e.g.
    //      "[1,2,3]".FromJson<object>().GetType() == typeof(List<object>)
    //      "{\"Value\":10}".FromJson<object>().GetType() == typeof(Dictionary<string,object>)
    // - No JIT Emit support to support AOT compilation on iOS
    // - Attempts are made to NOT throw an exception if the JSON is corrupted or invalid: returns null instead.
    // - Only public fields and property setters on classes/structs will be written to
    //
    // Limitations:
    // - No JIT Emit support to parse structures quickly
    // - Limited to parsing <2GB JSON files (due to int.MaxValue)
    // - Parsing of abstract classes or interfaces is NOT supported and will throw an exception.
    public static class JSONParser
    {
        [ThreadStatic] static Stack<List<string>> splitArrayPool;
        [ThreadStatic] static StringBuilder stringBuilder;
        [ThreadStatic] static Dictionary<Type, Dictionary<string, FieldInfo>> fieldInfoCache;
        [ThreadStatic] static Dictionary<Type, Dictionary<string, PropertyInfo>> propertyInfoCache;

		public static Object FromJson(string json){
			return json.FromJson<object>();
		}
		// extension method
        public static T FromJson<T>(this string json)
        {
            // Initialize, if needed, the ThreadStatic variables
            if (propertyInfoCache == null) propertyInfoCache = new Dictionary<Type, Dictionary<string, PropertyInfo>>();
            if (fieldInfoCache == null) fieldInfoCache = new Dictionary<Type, Dictionary<string, FieldInfo>>();
            if (stringBuilder == null) stringBuilder = new StringBuilder();
            if (splitArrayPool == null) splitArrayPool = new Stack<List<string>>();

            //Remove all whitespace not within strings to make parsing simpler
            stringBuilder.Length = 0;
            for (int i = 0; i < json.Length; i++)
            {
                char c = json[i];
                if (c == '"')
                {
                    i = AppendUntilStringEnd(true, i, json);
                    continue;
                }
                if (char.IsWhiteSpace(c))
                    continue;

                stringBuilder.Append(c);
            }

            //Parse the thing!
            return (T)ParseValue(typeof(T), stringBuilder.ToString());
        }

        static int AppendUntilStringEnd(bool appendEscapeCharacter, int startIdx, string json)
        {
            stringBuilder.Append(json[startIdx]);
            for (int i = startIdx + 1; i < json.Length; i++)
            {
                if (json[i] == '\\')
                {
                    if (appendEscapeCharacter)
                        stringBuilder.Append(json[i]);
                    stringBuilder.Append(json[i + 1]);
                    i++;//Skip next character as it is escaped
                }
                else if (json[i] == '"')
                {
                    stringBuilder.Append(json[i]);
                    return i;
                }
                else
                    stringBuilder.Append(json[i]);
            }
            return json.Length - 1;
        }

        //Splits { <value>:<value>, <value>:<value> } and [ <value>, <value> ] into a list of <value> strings
        static List<string> Split(string json)
        {
            List<string> splitArray = splitArrayPool.Count > 0 ? splitArrayPool.Pop() : new List<string>();
            splitArray.Clear();
            if (json.Length == 2)
                return splitArray;
            int parseDepth = 0;
            stringBuilder.Length = 0;
            for (int i = 1; i < json.Length - 1; i++)
            {
                switch (json[i])
                {
                    case '[':
                    case '{':
                        parseDepth++;
                        break;
                    case ']':
                    case '}':
                        parseDepth--;
                        break;
                    case '"':
                        i = AppendUntilStringEnd(true, i, json);
                        continue;
                    case ',':
                    case ':':
                        if (parseDepth == 0)
                        {
                            splitArray.Add(stringBuilder.ToString());
                            stringBuilder.Length = 0;
                            continue;
                        }
                        break;
                }

                stringBuilder.Append(json[i]);
            }

            splitArray.Add(stringBuilder.ToString());

            return splitArray;
        }

        internal static object ParseValue(Type type, string json)
        {
            if (type == typeof(string))
            {
                if (json.Length <= 2)
                    return string.Empty;
                StringBuilder parseStringBuilder = new StringBuilder(json.Length);
                for (int i = 1; i < json.Length - 1; ++i)
                {
                    if (json[i] == '\\' && i + 1 < json.Length - 1)
                    {
                        int j = "\"\\nrtbf/".IndexOf(json[i + 1]);
                        if (j >= 0)
                        {
                            parseStringBuilder.Append("\"\\\n\r\t\b\f/"[j]);
                            ++i;
                            continue;
                        }
                        if (json[i + 1] == 'u' && i + 5 < json.Length - 1)
                        {
                            UInt32 c = 0;
                            if (UInt32.TryParse(json.Substring(i + 2, 4), System.Globalization.NumberStyles.AllowHexSpecifier, null, out c))
                            {
                                parseStringBuilder.Append((char)c);
                                i += 5;
                                continue;
                            }
                        }
                    }
                    parseStringBuilder.Append(json[i]);
                }
                return parseStringBuilder.ToString();
            }
            if (type.IsPrimitive)
            {
                var result = Convert.ChangeType(json, type, System.Globalization.CultureInfo.InvariantCulture);
                return result;
            }
            if (type == typeof(decimal))
            {
                decimal result;
                decimal.TryParse(json, System.Globalization.NumberStyles.Float, System.Globalization.CultureInfo.InvariantCulture, out result);
                return result;
            }
            if (type == typeof(DateTime))
            {
                DateTime result;
                DateTime.TryParse(json.Replace("\"",""), System.Globalization.CultureInfo.InvariantCulture, System.Globalization.DateTimeStyles.None, out result);
                return result;
            }
            if (json == "null")
            {
                return null;
            }
            if (type.IsEnum)
            {
                if (json[0] == '"')
                    json = json.Substring(1, json.Length - 2);
                try
                {
                    return Enum.Parse(type, json, false);
                }
                catch
                {
                    return 0;
                }
            }
            if (type.IsArray)
            {
                Type arrayType = type.GetElementType();
                if (json[0] != '[' || json[json.Length - 1] != ']')
                    return null;

                List<string> elems = Split(json);
                Array newArray = Array.CreateInstance(arrayType, elems.Count);
                for (int i = 0; i < elems.Count; i++)
                    newArray.SetValue(ParseValue(arrayType, elems[i]), i);
                splitArrayPool.Push(elems);
                return newArray;
            }
            if (type.IsGenericType && type.GetGenericTypeDefinition() == typeof(List<>))
            {
                Type listType = type.GetGenericArguments()[0];
                if (json[0] != '[' || json[json.Length - 1] != ']')
                    return null;

                List<string> elems = Split(json);
                var list = (IList)type.GetConstructor(new Type[] { typeof(int) }).Invoke(new object[] { elems.Count });
                for (int i = 0; i < elems.Count; i++)
                    list.Add(ParseValue(listType, elems[i]));
                splitArrayPool.Push(elems);
                return list;
            }
            if (type.IsGenericType && type.GetGenericTypeDefinition() == typeof(Dictionary<,>))
            {
                Type keyType, valueType;
                {
                    Type[] args = type.GetGenericArguments();
                    keyType = args[0];
                    valueType = args[1];
                }

                //Refuse to parse dictionary keys that aren't of type string
                if (keyType != typeof(string))
                    return null;
                //Must be a valid dictionary element
                if (json[0] != '{' || json[json.Length - 1] != '}')
                    return null;
                //The list is split into key/value pairs only, this means the split must be divisible by 2 to be valid JSON
                List<string> elems = Split(json);
                if (elems.Count % 2 != 0)
                    return null;

                var dictionary = (IDictionary)type.GetConstructor(new Type[] { typeof(int) }).Invoke(new object[] { elems.Count / 2 });
                for (int i = 0; i < elems.Count; i += 2)
                {
                    if (elems[i].Length <= 2)
                        continue;
                    string keyValue = elems[i].Substring(1, elems[i].Length - 2);
                    object val = ParseValue(valueType, elems[i + 1]);
                    dictionary[keyValue] = val;
                }
                return dictionary;
            }
            if (type == typeof(object))
            {
                return ParseAnonymousValue(json);
            }
            if (json[0] == '{' && json[json.Length - 1] == '}')
            {
                return ParseObject(type, json);
            }

            return null;
        }

        static object ParseAnonymousValue(string json)
        {
            if (json.Length == 0)
                return null;
            if (json[0] == '{' && json[json.Length - 1] == '}')
            {
                List<string> elems = Split(json);
                if (elems.Count % 2 != 0)
                    return null;
                var dict = new Dictionary<string, object>(elems.Count / 2);
                for (int i = 0; i < elems.Count; i += 2)
                    dict[elems[i].Substring(1, elems[i].Length - 2)] = ParseAnonymousValue(elems[i + 1]);
                return dict;
            }
            if (json[0] == '[' && json[json.Length - 1] == ']')
            {
                List<string> items = Split(json);
                var finalList = new List<object>(items.Count);
                for (int i = 0; i < items.Count; i++)
                    finalList.Add(ParseAnonymousValue(items[i]));
                return finalList;
            }
            if (json[0] == '"' && json[json.Length - 1] == '"')
            {
                string str = json.Substring(1, json.Length - 2);
                return str.Replace("\\", string.Empty);
            }
            if (char.IsDigit(json[0]) || json[0] == '-')
            {
                if (json.Contains("."))
                {
                    double result;
                    double.TryParse(json, System.Globalization.NumberStyles.Float, System.Globalization.CultureInfo.InvariantCulture, out result);
                    return result;
                }
                else
                {
                    int result;
                    int.TryParse(json, out result);
                    return result;
                }
            }
            if (json == "true")
                return true;
            if (json == "false")
                return false;
            // handles json == "null" as well as invalid JSON
            return null;
        }

        static Dictionary<string, T> CreateMemberNameDictionary<T>(T[] members) where T : MemberInfo
        {
            Dictionary<string, T> nameToMember = new Dictionary<string, T>(StringComparer.OrdinalIgnoreCase);
            for (int i = 0; i < members.Length; i++)
            {
                T member = members[i];
                if (member.IsDefined(typeof(IgnoreDataMemberAttribute), true))
                    continue;

                string name = member.Name;
                if (member.IsDefined(typeof(DataMemberAttribute), true))
                {
                    DataMemberAttribute dataMemberAttribute = (DataMemberAttribute)Attribute.GetCustomAttribute(member, typeof(DataMemberAttribute), true);
                    if (!string.IsNullOrEmpty(dataMemberAttribute.Name))
                        name = dataMemberAttribute.Name;
                }

                nameToMember.Add(name, member);
            }

            return nameToMember;
        }

        static object ParseObject(Type type, string json)
        {
            object instance = FormatterServices.GetUninitializedObject(type);

            //The list is split into key/value pairs only, this means the split must be divisible by 2 to be valid JSON
            List<string> elems = Split(json);
            if (elems.Count % 2 != 0)
                return instance;

            Dictionary<string, FieldInfo> nameToField;
            Dictionary<string, PropertyInfo> nameToProperty;
            if (!fieldInfoCache.TryGetValue(type, out nameToField))
            {
                nameToField = CreateMemberNameDictionary(type.GetFields(BindingFlags.Instance | BindingFlags.Public | BindingFlags.FlattenHierarchy));
                fieldInfoCache.Add(type, nameToField);
            }
            if (!propertyInfoCache.TryGetValue(type, out nameToProperty))
            {
                nameToProperty = CreateMemberNameDictionary(type.GetProperties(BindingFlags.Instance | BindingFlags.Public | BindingFlags.FlattenHierarchy));
                propertyInfoCache.Add(type, nameToProperty);
            }

            for (int i = 0; i < elems.Count; i += 2)
            {
                if (elems[i].Length <= 2)
                    continue;
                string key = elems[i].Substring(1, elems[i].Length - 2);
                string value = elems[i + 1];

                FieldInfo fieldInfo;
                PropertyInfo propertyInfo;
                if (nameToField.TryGetValue(key, out fieldInfo))
                    fieldInfo.SetValue(instance, ParseValue(fieldInfo.FieldType, value));
                else if (nameToProperty.TryGetValue(key, out propertyInfo))
                    propertyInfo.SetValue(instance, ParseValue(propertyInfo.PropertyType, value), null);
            }

            return instance;
        }
    }
}


'@  -ReferencedAssemblies 'System.dll','System.Xml.dll','System.Data.dll','System.Core.dll','System.Runtime.Serialization.dll'
}

$debug_flag = [bool]$PSBoundParameters['debug'].IsPresent -bor $debug.ToBool()

if ($debug) {
  write-host 'Using "ConvertTo-HashTable"'
  $data = (Get-Content -Path $filelist_file) -join "`n"
    if ($debug){
      Write-host ('data: "{0}"' -f $data)
    }
  $json_obj = $data | convertfrom-json
  $response = $json_obj | ConvertTo-HashTable
  if ( -not ($response['status'] -eq 'OK' ) ) {
     write-host ('Exit response status: {0}' -f $response['status'])
     exit
  }
  write-host 'Response:'
  write-host $response
  $result = $response['result']

  $count = $result.Count
  write-host 'Result:'
  write-host $result

  write-host ('downloading {0} files' -f $count)

  # NOTE: need to ennfoce precedence
  # "because [System.Object[]] does not contain a method named 'op_Subtraction'."
  0..($count - 1)| foreach-object {
    $i = $_
    $filename = $result[$i]
    write-host ('downloading config[{0}]: {1}' -f $i, ($result[$i]))
  }
}
if ($debug){
  write-host 'Using "Utils.JSONParser"'
}
$data_json = get-content -path (resolve-path -path $filelist_file)
$response = [Utils.JSONParser]::FromJson($data_json)

if ( -not ($response['status'] -eq 'OK' ) ) {
   write-host ('Exit response status: {0}' -f $response['status'])
   exit
}
if ($debug){
  write-output 'Response:'
  $response | format-list
}
$result = $response['result']
$count = $result.count

if ($debug){
  write-output 'Result:'  
  $result | format-list
}
if ($count -gt 0) {
  if ($debug){
    write-host ('downloading {0} files' -f $count)
  }
  # NOTE: need to enforce precedence to avoid error
  # "because [System.Object[]] does not contain a method named 'op_Subtraction'."
  0..($count - 1)| foreach-object {
    $i = $_
    $filename = $result[$i]
    if ($debug){
      write-output ('config[{0}]:' -f $i)
    }
    write-output ('downloading file: "{0}"' -f ($result[$i]))
  }
} else { 
  write-host ('nothing to download ')
}

# https://java2blog.com/powershell-convert-epoch-time-to-datetime/

$epochTime = 1615395869
$dateTimeObject = (New-Object DateTime 1970,1,1,0,0,0).AddSeconds($epochTime)
Write-Output $dateTimeObject

($d = get-date)|out-null
$d = $d.AddMinutes((-1 *$interval))
# NOTE: the ToUnixTimeSeconds() API requires
# PSVersion 5.1.14409.1005
# will not work under
# PSVersion 5.0.10586.117
try {
 $last_run_epoch = $([DateTimeOffset]$d).ToUnixTimeSeconds() 
} catch [Exception] {
  # https://stackoverflow.com/questions/4192971/in-powershell-how-do-i-convert-datetime-to-unix-time
  $d1 = Get-Date -Date "01/01/1970"
  $last_run_epoch = [Convert]::ToInt32( (new-timespan -start $d1 -end $d).TotalSeconds )
}
# The epoch logic
# https://unix.stackexchange.com/questions/2987/how-do-i-convert-an-epoch-timestamp-to-a-human-readable-format-on-the-cli

[string]$config_path = ([System.IO.Path]::Combine((resolve-path -path '.').path,$local_config_path))

# NOTE: not -include
$count = @(get-childitem -path $config_path -filter '*.json' -erroraction silentlycontinue).count

if ($count -gt 0 ){
  $url = ( $list_url -replace '{hostname}', $hostname ) -replace '{last_run_epoch}', $last_run_epoch 
  write-host ('Will query new configurations: "{0}"' -f $url)
} else {
  $url = ( $list_url -replace '{hostname}', $hostname ) -replace '\?newer=.*$', ''
  write-host ('Will query all configurations: "{0}"' -f $url)
}
