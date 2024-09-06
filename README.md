# INPE-MBUFRTOOLS
    TOOLS for encoding and decoding Meteorological data in WMO-BUFR format

## 1- Main tools in this package
-BUFRCONTENT : List the contents of one or more BUFR files
-BUFRSPLIT : Separates the messages from a BUFR file to different files.
-BUFRDUMP : Decodes a BUFR file and converts it into a text file
-BUFRGEN: Encodes a BUFR file using a text file as input. ( This text file must have the same format of the decoded text files by BUFRDUMP)
-BUFR2CSV: Decodes a BUFR file and converts it into a text file separated by comma (Suitable to be read in MS-EXCEL, LibreOffice Calc, etc)

## 2 - Compiling MBUFRTOOLS

### 2.1 Instructions for compiling
#### 2.1.1 – Set the Fortran compiler.
The file “makefile.comp” must contain the entrance for Fortran compiler installed in your computer as
well as the appropriated compilation parameters.
To create the “makefile.comp”, copy or rename one of the makefile_<compiler> to makefile.comp.
See the examples bellow
    cp makefile_g95 makefile.comp (for g95 compiler)
    cp makefile_ifort makefile.comp ( for Intel Fortran compiler)
    cp makefile_gfortran makefile.comp (for gfortran compiler)
    cp makefile_cray makefile.comp (for compilation in cray environment)

#### 2.1.2 Run the command "make" to compile

# 3 - Environment Variables
It is necessary to configure the following environment variables
    MBUFR_TABLES = <Directory Location of necessary tables in the system>
    PATH=<Directory Location of binary files in the system >

## 3.1 Setting the environment variables on Linux (bash environment)
Example:
Include in .bashrc file the followed lines
    export MBUFR_TABLES=/home/user/mbufrtools/bufrtables
    export PATH=/home/user/mbuufrtools/bin
type source .bashrc to update the modification

