### Info

### Background 

Both Java and .NET have open-source, cross-platform libraries that read and write `.mdb` and `.accdb` files directly without needing [Microsoft Access or Windows](https://en.wikipedia.org/wiki/Microsoft_Access) [Open Database Connectivity](https://en.wikipedia.org/wiki/Open_Database_Connectivity) Microsoft relational database engine that evolved from the Jet Database Engine powers Microsoft Access and enables external applications to read and write Access files __ODBC__ [Access Database Engine](https://en.wikipedia.org/wiki/Access_Database_Engine) __ACE__ drivers installed

#### Java

[UCanAccess](https://spannm.github.io/ucanaccess/10-ucanaccess.html)nd [Jackcess](https://jackcess.sourceforge.io/summary.html) - pure-Java [Java Database Connectivity](https://en.wikipedia.org/wiki/Java_Database_Connectivity) __JDBC__ driver that lets you query and modify Access databases using standard SQL. It runs on Windows, Linux, and macOS with zero native dependencies

### .Net
`JustyBase.UCanAccessCs`: an c# port - on [nuget](https://www.nuget.org/packages/JustyBase.UCanAccessCs) -  exposes Access files through standard ADO.NET abstractions (DbConnection, DbCommand, DbDataReader) using an in-memory SQLite mirror. a pure .NET ADO.NET provider and file-format implementation for Microsoft Access .mdb/.accdb databases — no Access, ODBC, or ACE required NOTE:  limited to __.Net 10__. Onky 50 commits on [github](https://github.com/justybase/JustyBase.UCanAccessCs)

[OfficeIMO.Access](https://officeimo.com/products/access/) on [nuget](https://www.nuget.org/packages/OfficeIMO.Access) - all .Net typical platforms are supported. Reads Access MDB and ACCDB databases, inspect queries, forms and VBA, and create Jet 4 or ACE table databases without Microsoft Access.

### See Also

  * [UCanAccess](https://foojay.io/today/ucanaccess-java-ms-access-jdbc-guide/)  - the Modern Pure-Java Bridge to Microsoft Access 
