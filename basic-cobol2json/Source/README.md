# Cobol To Json
## Introduction

The CobolToJson (subproject of JRecord) package will convert Cobol Data files to Json.

The project supports several Cobol dialects + number of other options. CobolToJson depends on the following projects:

* **cb2xml** - Converts a Cobol Copybook to Json.
* **JRecord** - Read/Write Cobol data files using a Cobol Copybook in Java. It also supplies the Csv file handling. 

This project will take a Cobol-Copybook and Cobol-Data-File

Cobol-Copybook:

~~~Cobol
        03  DTAR020-KCODE-STORE-KEY.
            05 DTAR020-KEYCODE-NO      PIC X(08).
            05 DTAR020-STORE-NO        PIC S9(03)   COMP-3.
        03  DTAR020-DATE               PIC S9(07)   COMP-3.
        03  DTAR020-DEPT-NO            PIC S9(03)   COMP-3.
        03  DTAR020-QTY-SOLD           PIC S9(9)    COMP-3.
        03  DTAR020-SALE-PRICE         PIC S9(9)V99 COMP-3.
~~~

Cobol-Data-File Field values: 

    69684558	20	40118	280	1	19.00
    69684558	20	40118	280	-1	-19.00
    69684558	20	40118	280	1	5.01
    69694158	20	40118	280	1	19.00
    69694158	20	40118	280	-1	-19.00
    69694158	20	40118	280	1	5.01
    63604808	20	40118	170	1	4.87
    62684671	20	40118	685	1	69.99
    62684671	20	40118	685	-1	-69.99
    64634429	20	40118	957	1	3.99
    66624458	20	40118	957	1	0.89    
    
And the script to convert the Cobol Data file to an Json file:

      ../lib/Cobol2Json.bat/sh   -cobol cobol/DTAR020.cbl ^
                                 -font cp037         -fileOrganisation FixedWidth ^
                                 -input in/DTAR020.bin   ^
                                 -output out/DTAR020_A.json
 
And the Json output 

~~~json  
 {
   "DTAR020" : [ {
     "DTAR020-KCODE-STORE-KEY" : {
       "DTAR020-KEYCODE-NO" : "69684558",
       "DTAR020-STORE-NO" : 20
     },
     "DTAR020-DATE" : 40118,
     "DTAR020-DEPT-NO" : 280,
     "DTAR020-QTY-SOLD" : 1,
     "DTAR020-SALE-PRICE" : 19.00
   }, {
     "DTAR020-KCODE-STORE-KEY" : {
       "DTAR020-KEYCODE-NO" : "69684558",
       "DTAR020-STORE-NO" : 20
     },
     "DTAR020-DATE" : 40118,
     "DTAR020-DEPT-NO" : 280,
     "DTAR020-QTY-SOLD" : -1,
     "DTAR020-SALE-PRICE" : -19.00
   }, {
     "DTAR020-KCODE-STORE-KEY" : {   
~~~

## Java Interface
      
The prefered way to access CobolToJson is via the **CobolJsonConversion** class:

~~~java
    CobolJsonConversion.newCobolJsonConversion("me/cobol/DTAR020.cbl")
        .setFileOrganization(IFileStructureConstants.IO_FIXED_LENGTH)
        .setSplitCopybook(CopybookLoader.SPLIT_NONE)
        .setFont("cp037")
        .setTagFormat(IReformatFieldNames.RO_UNDERSCORE)

       .singleRecordToJsonObject()
           .setCobolRecord(line.getData())
           .toJsonString()
~~~ 
           
First you specify the Cobol-Copybook and its attribtes (e.g. setFont, setFileOrganisation) Then
you start the conversion dialog. There are 3 conversion dialogs, all have a setInput source method and a execute method.
The 3 conversion dialogs are:

* **singleRecordToJsonObject()** - Takes a single cobol record and converts it to JSON. The input could be a JRecord-Line
or the first record from a File.
* **multipleRecordsToJsonArray()** - Takes multiple Cobol Records and converts it to a JSON array of Objects.
The input could be a List of JRecord-Lines, a Cobol-Data-File or a JRecord Line-Reader.
* **jsonObjectToSingleRecord()** - Convert a JSON-Object to a single Cobol record
* **jsonArrayToMultipleRecords()** - Convert a JSON array to either a list or file of Cobol records.

There is also the older Cobol2Json interface

    Cobol2Json.newCobol2Json(CobolCopybook)
    Cobol2Json.newCobol2Json(CobolCopybookInputStream, Copybook name)
    Cobol2Json.newCb2Xml2Json(Cb2xmlCopybook)
    Cobol2Json.newCb2Xml2Json(Cb2xmlCopybookInputStream, CopybookName)
 
 

A sample call:
          
~~~java
        Cobol2Json.newCobol2Json(Cbl2JsonCode.getFullName("DTAR020.cbl"))
             .setFont("cp037")
             .setFileOrganization(Constants.IO_FIXED_LENGTH)
             .setSplitCopybook(CopybookLoader.SPLIT_NONE)
             .setTagFormat(IReformatFieldNames.RO_UNDERSCORE)
             .cobol2json(
                 "G:/Temp/DTAR020_tst1.bin",
                 "G:/Temp/DTAR020_tst1.bin.json");
~~~


Other methods include:

* **singleCobolRecord2json**  Convert a single Cobol record to a Json object (File or Writer).
* **singleCobolRecord2jsonString**   Convert cobol record (array of bytes) to a java String. It will help you convert son data to / from a Cobol System. 

    
## Batch Scripts

These are the options you can use when calling <b>Cobol2Json</b> programs to convert Cobol data files to Json Files:

          -cobol	- Cobol  copybook used to "interpret" the data (you must supply either a cobol or cb2xml copybook  
          -cb2xml	- Cb2xml copybook used to "interpret" the data  
 
          -input	- Input file  
          -output	- Output file  
          -font  	- Characterset used in the Cobol data file (e.g. IBM037 for US-EBCIDIC)  
 
          -dropCopybookName	- (true/false) wether to drop the cobol copybook name from the start of the Json Tags  
 
          -tagFormat     	- How Cobol Variable names are reformated to Json tags:  
               Asis       	- Use the Cobol Variable name  
               Underscore 	- Convert - to _,         COBOL-VAR-NAME ==> COBOL_VAR_NAME  
               CamelCase  	- Convert to Camel Case,  COBOL-VAR-NAME ==> cobolVarName  

          -fileOrganisation	- "file organization" of the Cobol data file  
               Text    		- Standard Windows/Unix text file (single byte character-set)  
               FixedWidth 	- File where lines (records) are the same length no \n  
               Mainframe_VB	- Mainframe VB, file consists of <record-length><record-data>  
               GNUCobol_VB	- GNU Cobol VB, file consists of <record-length><record-data>  
 
          -dialect	- Cobol Dialect  
               Mainframe	- Mainframe cobol  
               Futjitsu 	- Fujitsu PC cobol  
               GNUCobol 	- GNU Cobol (little endian, ie intel)  
               GNUCobolBE	- GNU Cobol (big endian, ie IBM, Sun(oracle))  
 
         -split	- Option for Splitting Copybook into seperate records  
               None      	- No Split  
               01        	- Split on 01  
               Highest	- On Highest Repeating  
 
         -recordSelection	- Record Selection, can be used multiple time,  
                                format: -recordSelection RecordName field=value
 
         -recordParent   	- Record Parent, can be used multiple time,  
                               format: -recordParent    RecordName ParentRecord
 
 

### Convert a Simple Single Record Cobol file to Json

The RecordEditor CodeGen of the RecordEditor will generate bat/shell scripts from a Cobol copybook

    ../lib/Cobol2Json.bat     -cobol G:/Users/BruceTst01/RecordEditor_HSQL/CopyBook/Cobol/DTAR020.cbl ^
                                 -fileOrganisation FixedWidth ^
                                 -font cp037 ^
                             -input  G:/Users/BruceTst01/RecordEditor_HSQL/SampleFiles/DTAR020.bin   ^
                             -output out/DTAR020.bin.json

                             
### Convert a Multi Record Cobol file to Json

This converts a multi record file into Json. Please note the use of

* **split**   used to define how the Cobol Copybook is split into seperate records.
* **recordSelection** Defines how to determine one record from another.

     ../lib/Cobol2Json.bat    -cobol G:/Users/Bruce01/RecordEditor_HSQL/Copybook/Cobol/amsPoDownload.cbl ^
                                 -fileOrganisation Text ^
                                 -split 01 ^
                                 -recordSelection PO-Record  Record-Type=H1 ^
                                 -recordSelection Product-Record  Record-Type=D1 ^
                                 -recordSelection Location-Record  Record-Type=S1 ^
                             -input  G:/Users/Bruce01/RecordEditor_HSQL/SampleFiles/Ams_PODownload_20041231.txt   ^
                             -output out/Ams_PODownload_20041231.txt.json
 

## Related

Projects related to Cobol to Json include:

* [cb2xml](https://github.com/bmTas/cb2xml) - Used for Cobol Copybook analysis by the CobolToJson project.
* [JRecord](https://github.com/bmTas/JRecord) - Used Cobol-IO (using a Cobol Copybook) by the CobolToJson project.
* [RecordEditor](http://record-editor.sourceforge.net/) - can edit Cobol-Data files and generate CobolToJson scripts. 

### Other Options

Other options for accesssing Cobol data files include

* [CobolToCsv](https://sourceforge.net/projects/coboltocsv/) - use the sister project to convert your Cobol files to/from Csv. CobolToCsv can handle simple single-record-type files.
* [CobolToXml](https://sourceforge.net/projects/coboltoxml/) - use the sister project to convert your Cobol files to/from Xaml. CobolToXml can handle multi-record-type files.
* You can do your own JRecord programming (with the help of the CodeGen utility).
* It should be possible to do other generic data conversions e.g. Cobol ==>> Avro. 
