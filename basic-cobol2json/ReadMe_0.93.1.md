## Cobol To Xml 0.93.1

This project will convert a **Cobol Data** File to/from a **Json** file. The project offers both a batch and java/JVM interface.
This project grew out of the [JRecord](https://sourceforge.net/projects/jrecord/) project.


### Downloads

* CobolToJson_0.93.1.zip
                                                                                                                                                                                     

                     
### Usage

     ../lib/Cobol2Json.bat     -cobol G:/Users/BruceTst01/RecordEditor_HSQL/CopyBook/Cobol/DTAR020.cbl ^
                               -fileOrganisation FixedWidth ^
                               -font cp037 ^
                           -input  G:/Users/BruceTst01/RecordEditor_HSQL/SampleFiles/DTAR020.bin   ^
                           -output out/DTAR020.bin.json

or in java

      Cobol2Json.newCobol2Json(Cbl2JsonCode.getFullName("cobol/amsPoDownload.cbl"))
                  .setFileOrganization(Constants.IO_BIN_TEXT)
                  .setSplitCopybook(CopybookLoader.SPLIT_01_LEVEL)
                  .setTagFormat(IReformatFieldNames.RO_UNDERSCORE)

                     .setRecordSelection("PO-Record", Cobol2Json.newFieldSelection("Record-Type","H1"))
                     .setRecordSelection("Product-Record", Cobol2Json.newFieldSelection("Record-Type","D1"))
                     .setRecordSelection("Location-Record", Cobol2Json.newFieldSelection("Record-Type","S1"))

                  .cobol2json("Ams_PODownload_20041231.txt",
                              "G:/Temp/amsPoDownload_records.json");

To Convert a json file to Cobol

      Cobol2Json.newCobol2Json(Cbl2JsonCode.getFullName("DTAR020.cbl"))
                  .setFont("cp037")
                  .setFileOrganization(Constants.IO_FIXED_LENGTH)
                  .setSplitCopybook(CopybookLoader.SPLIT_NONE)
                 
                  .jsonArrayToCobolFile(
                            "G:/Temp/DTAR020_tst1_normal.json",
                            "G:/Temp/DTAR020_tst1_normal_json.bin");
                            
### Generating Code Gen Script / java program

The RecordEditor can generate CodeGen Script / programs from a Cobol Copybook and Sample file.
The RecordEditor will analyse the fiule and try and determine the appropriate attributes.

See [Wiki Description](https://sourceforge.net/p/coboltojson/wiki/CodeGeneration/) for more details.
                            

### Changes

#### Version 0.93.1

* Adding option for
    * Suppressing Cobol Fields / Groups
    * Selecting which redefine Field / Group to print
    * Formatting fields
* Adding option to write a JSON schema for the Generated json
* Adding option to write a a sample json file
* Add option to `flatten` the structure


#### Version 0.90.4

* Use JRecord 0.90.4 and latest cb2xml.
* Add option to convert Json to Cobol Data file or Cobol Byte array.

#### Version 0.81.5

* Moved from JRecord 
* Includes the latest JRecord
