using System;
using System.ComponentModel;
using System.Linq;

namespace Utils {
    public enum ResponseHeaders {
        [Description("Cache-Control")]
        CacheControl = 0,

        [Description("Connection")]
        Connection = 1,

        [Description("Date")]
        Date = 2,

        [Description("Keep-Alive")]
        KeepAlive = 3,

        [Description("Pragma")]
        Pragma = 4,

        [Description("Trailer")]
        Trailer = 5,

        [Description("Transfer-Encoding")]
        TransferEncoding = 6,

        [Description("Upgrade")]
        Upgrade = 7,

        [Description("Via")]
        Via = 8,

        [Description("Warning")]
        Warning = 9,

        [Description("Allow")]
        Allow = 10,

        [Description("Content-Length")]
        ContentLength = 11,

        [Description("Content-Type")]
        ContentType = 12,

        [Description("Content-Encoding")]
        ContentEncoding = 13,

        [Description("Content-Langauge")]
        ContentLanguage = 14,

        [Description("Content-Location")]
        ContentLocation = 15,

        [Description("Content-MD5")]
        ContentMd5 = 16,

        [Description("Range")]
        ContentRange = 17,

        [Description("Expires")]
        Expires = 18,

        [Description("Last-Modified")]
        LastModified = 19,

        [Description("Accept-Ranges")]
        AcceptRanges = 20,

        [Description("Age")]
        Age = 21,

        [Description("Etag")]
        ETag = 22,

        [Description("Location")]
        Location = 23,

        [Description("Proxy-Authenticate")]
        ProxyAuthenticate = 24,

        [Description("Retry-After")]
        RetryAfter = 25,

        [Description("Server")]
        Server = 26,

        [Description("Set-Cookie")]
        SetCookie = 27,

        [Description("Vary")]
        Vary = 28,

        [Description("WWW-Authenticate")]
        WwwAuthenticate = 29,

    }
}
