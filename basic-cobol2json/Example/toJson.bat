rem Cobol to JSon conversion

java -jar ../lib/Cobol2Json.jar -cobol cobol/DTAR020.cbl ^
                                    -fileOrganisation FixedWidth ^
                                    -font cp037 ^
                                -input  in/DTAR020.bin   ^
                                -output out/DTAR020.bin1.json

java -jar ../lib/Cobol2Json.jar -cobol cobol/amsPoDownload.cbl ^
                                 -fileOrganisation Text ^
                                 -split 01 ^
                                 -recordSelection PO-Record  Record-Type=H1 ^
                                 -recordSelection Product-Record  Record-Type=D1 ^
                                 -recordSelection Location-Record  Record-Type=S1 ^
                                   -recordParent Product-Record  PO-Record ^
                                   -recordParent Location-Record Product-Record  ^
                           -input  in/Ams_PODownload_20041231.txt   ^
                           -output out/Ams_PODownload_20041231.txt.tree.json

pause                                                         