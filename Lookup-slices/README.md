**Searching for Life patterns**  

Source code for my Master's project.

Project1 is assorted experiments, which may be removed later.

Project2 is the prototype for the solver engine.

It uses the `GridWalker` algorithm to try and prove that a given pattern is a Garden of Eden.

In order to run the program you need to download all compressed files and uncompress all files in the same directory.
Uncompress all files (you need about 3GB of storage).

**Program executable**   
`ExperimentsProgram.7z` is the main executable.  
`LookupTables.zip.001`  
`LookupTables.zip.002`  
`LookupTables.zip.003`  
`LookupTables.zip.004`  
Is a multi-part archive containing the Lookup tables and the indexes. It contains the following files:  

`lookupCompressed_Correct.bin` - A lookup table data containing slices for every 5x5 future bitmap  
`lookup_Correct.idx` - The index for this lookup table  
`lookupCorners_Compressed.bin` - An (optional) lookup table data for offset slices for every 5x5 future bitmap  
`LookupCorners.idx` - The index for this lookup table  
`lookupCounts.bin` - Ancestor counts for (the number of past 7x7 patterns that precede) every 5x5 future bitmap  
`lookupUnknown.bin` - Lookup table data for 5x5 future bitmaps with unknown pixels.  

**Source code**  
The source code is contained in `Unit2.Pas` and `UnitTests.pas`.  
`Unit2.pas` contains the UI and all components of the solver engine.  
`UnitTests.pas` contains unit tests for most components of the GridWalker algorithm.  
The other source files are part of earlier projects, which will eventually be cleaned up.  

The code is written to compile with Delphi Rio 10.3 Community Edition.
This can be downloaded from: https://www.embarcadero.com/products/delphi/starter







