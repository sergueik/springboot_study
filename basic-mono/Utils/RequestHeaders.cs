using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Text;

namespace Utils {
    public enum RequestHeaders {
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

        [Description("Content-Range")]
        ContentRange = 17,

        [Description("Expires")]
        Expires = 18,

        [Description("Last-Modified")]
        LastModified = 19,

        [Description("Accept")]
        Accept = 20,

        [Description("Accept-Charset")]
        AcceptCharset = 21,

        [Description("Accept-Encoding")]
        AcceptEncoding = 22,

        [Description("Accept-Langauge")]
        AcceptLanguage = 23,

        [Description("Authorization")]
        Authorization = 24,

        [Description("Cookie")]
        Cookie = 25,

        [Description("Expect")]
        Expect = 26,

        [Description("From")]
        From = 27,

        [Description("Host")]
        Host = 28,

        [Description("If-Match")]
        IfMatch = 29,

        [Description("If-Modified-Since")]
        IfModifiedSince = 30,

        [Description("If-None-Match")]
        IfNoneMatch = 31,

        [Description("If-Range")]
        IfRange = 32,

        [Description("If-Unmodified-Since")]
        IfUnmodifiedSince = 33,

        [Description("Max-Forwards")]
        MaxForwards = 34,

        [Description("Proxy-Authorization")]
        ProxyAuthorization = 35,

        [Description("Referer")]
        Referer = 36,

        [Description("Range")]
        Range = 37,

        [Description("TE")]
        Te = 38,

        [Description("Translate")]
        Translate = 39,

        [Description("User-Agent")]
        UserAgent = 40,

    }
}
