# Simh_tapes Ada Package
Simh_tape is an Ada package for handling magnetic tape images in the
[standard format](http://simh.trailing-edge.com/docs/simh_magtape.pdf) used by 
[SimH](http://simh.trailing-edge.com/) and many other computer simulators and emulators.

Low-level subprograms include...
 * `Read_Meta_Data()` and `Write_Meta_Data()` for handling headers, trailers, and inter-file gaps
 * `Read_Record_Data()` and `Write_Record_Data()` for handling data blocks (without their associated headers and trailers)
 * `Rewind()` and `Space_Forward()` for positioning the virtual tape image

Higher-level subprograms include...
 * `Scan_Image()` for examining/verifying a magnetic tape image file
 * `Dump_All_Files()` to extract each file found on the tape image as a numbered blob file 

An example command-line program is included `simhtapetool` which uses the package to provide a command line interface for...
 * creating a new SimH tape image file (from a simple CSV definition and source files)
 * dumping each file found on a tape image to a separate blob file
 * scanning the tape image for validity
