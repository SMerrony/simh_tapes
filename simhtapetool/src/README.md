# SimhTapeTool

SimhTapeTool is a (potentially useful) demonstration of the Ada 
`Simh_Tapes` package for handling SimH standard magnetic tape images.

```
Usage: bin/simhtapetool [OPTIONS]...

  -h, --help       Display this help text
  -c, --create     Create a new SimH Magnetic Tape Image file
  -d, --definition CSV Definition for a new image
  -e, --extract    Extract each file from the tape image
  -s, --scan       Scan SimH Magnetic Tape Image file for correctness, listing files
  ```

## List/Scan
To list the contents of a tape image: `simhtapetool -s [imagename]`

## Extract
To extract the contents of a tape image: `simhtapetool -e [imagename]`

Each file found on the tape will be extracted into the current directory as `file0, file1, file2, etc.`

## Create
To create a new image you need to create a simple CSV definition of the image in the following format:
```
filename0,blocksize
filename1,blocksize
etc.
```
The block size (in bytes) can be different for each file.

Create the image like this: `simhtapetool -c [imagename] -d [CSVdefinitionName]`
