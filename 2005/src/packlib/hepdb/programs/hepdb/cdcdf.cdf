*
* $Id: cdcdf.cdf,v 1.1 1996/03/12 13:08:27 cernlib Exp $
*
* $Log: cdcdf.cdf,v $
* Revision 1.1  1996/03/12 13:08:27  cernlib
* Build hepdb programs: hepdb, cdserv, cdnew, and cdmove
*
* Revision 1.1.1.1  1996/02/28 16:24:08  mclareni
* Hepdb, cdlib, etc
*
*
*----------------------------------------------------------------
*
* HEPDB command definition file (CDF)
*
*----------------------------------------------------------------
>NAME CDCDF
>MENU HEPDB
>Guidance
HEPDB shell commands
 
*----------------------------------------------------------------
>command ALIAS
>Action  CDKALI
>PARAMETERS
+
PATH  'Pathname ' C D=' '
ALIAS 'Alias    ' C D=' '
CHOPT 'Options '  C D=' '
-D 'delete the alias definition for the specified path'
-P 'print the alias definition for the specified path'
-R 'retrieve and print the equivalence name for the specified alias'
-S 'set the alias for the specified path for the current session'
-U 'as S, but also update database'
>Guidance
 
Use the ALIAS command to delete, print, retrieve, set or update
alias definitions. The action of the command depends on the option
specified by the CHOPT argument.
 
*----------------------------------------------------------------
>command CDHELP
>Action  CDKHLP
>PARAMETERS
+
PATH  'Pathname ' C D=' '
FILE  'File     ' C D=' '
CHOPT 'Options '  C D=' '
-D 'delete the help information for the specified path'
-P 'print the help information for the specified path'
-R 'read the help information for specified path and enter into database'
-W 'write the help information for specified path into the file'
 
*----------------------------------------------------------------
>command CDTEXT
>Action  CDKTXT
>PARAMETERS
+
PATH  'Pathname ' C D=' '
FILE  'File     ' C D=' '
CHOPT 'Options '  C D=' '
-D 'delete the text information for the specified path'
-P 'print the text information for the specified path'
-R 'read the text information for specified path and enter into database'
-W 'write the text information for specified path into the file'
 
*----------------------------------------------------------------
>command COUNT
>Action  CDOBJC
>PARAMETERS
+
PATH  'Pathname ' C D=' '
CHOPT 'Options '  C D=' '
-D 'display number of subdirectories at each level'
-O 'display number of objects at each level'
-L 'display lowest level only, i.e. directories with no subdirectories'
-Z 'display only directories with no (zero) objects'
>Guidance
Use the COUNT command to count the number of objects in the specified
directory or directories.
 
*----------------------------------------------------------------
>command FILES
>Action  CDFILC
>Guidance
Use the FILES command to display information on the database files
that are currently open.
 
*----------------------------------------------------------------
>command LOGLEVEL
>Action  CDLOGC
>PARAMETERS
+
PATH  'Pathname' C D=' '
LEVEL 'Loglevel' I D=0
CHOPT 'Options ' C D=' '
>GUIDANCE
Use the LOGLEVEL command to set the HEPDB logging level.
.
Syntax:  LOGLEVEL path loglevel options
 
*----------------------------------------------------------------
>command OPEN
>Action  CDOPNC
>PARAMETERS
PREFIX   'Prefix  ' C D=' '
+
FILE     'Filename' C D=' '
CHOPT    'Options ' C D='X'
-X 'The specified file is in Zebra RZ exchange data format
-N 'The specified file is in Zebra RZ native data format
>GUIDANCE
Use the OPEN command to open a HEPDB file by specifying the database
prefix.
.
Syntax:  OPEN PREFIX FILENAME CHOPT
 
*----------------------------------------------------------------
>command CLOSE
>Action CDCLSH
>PARAMETERS
PREFIX 'Prefix of database file' C D=' '
+
CHOPT  'Options                ' C D=' '
-A 'Close all files (prefix is ignored)'
>guidance
Use the CLOSE command to close a HEPDB file opened by the OPEN
command.
*----------------------------------------------------------------
>command RZOPEN
>Action  CDRZOP
>PARAMETERS
NAME     'Topdir  ' C D=' '
FILE     'Filename' C D=' '
+
CHOPT    'Options ' C D='X'
-X 'The specified file is in Zebra RZ exchange data format
-N 'The specified file is in Zebra RZ native data format
>GUIDANCE
Use the OPEN command to open any RZ file, including an HEPDB file
.
Syntax:  RZOPEN TOPDIR FILENAME CHOPT
 
*----------------------------------------------------------------
>command RZCLOSE
>Action  CDCLSR
>PARAMETERS
+
NAME     'Topdir  ' C D=' '
CHOPT    'Options ' C D=' '
>GUIDANCE
Use the CLOSE command to close a file opened with RZOPEN
Syntax:  CLOSE NAME FILE CHOPT
 
*----------------------------------------------------------------
>command OUTPUT
>Action  CDOUTP
>PARAMETERS
+
FILE     'Filename' C D=' '
CHOPT    'Options ' C D=' '
-C 'close file and redirect output to terminal'
-P 'preserve case of file'
-R 'replace existing file'
-S 'switch back to previously opened file'
-T 'redirect output back to terminal'
>GUIDANCE
Use the OUTPUT command to redirect output to a specified file.
.
Syntax:  OUTPUT FILE
 
*----------------------------------------------------------------
>command WINDOW
>Guidance
Use the WINDOW command to set the insertion time window.
Objects that were inserted outside the specified window
will be invisible via the LS command.
 
Syntax: WINDOW start-end
 
The start and end times should be specified in the format
 
yymmdd.hhmm
 
Use the command WINDOW -R to reset to the default behaviour
of no check on insertion time.
 
>Action CDWIND
>Parameters
+
RANGE 'date and time range' C D='-'
CHOPT 'options            ' C D=' '
-R 'Reset the time window to the default (no check)'
*----------------------------------------------------------------
>command VERSION
>Guidance
Use the VERSION command to display the version of the HEPDB
software that you are running.
Syntax:  VERSION
>Action  CDVERC
 
>MENU \HDBCAT
>Guidance
Commands to manipulate HEPDB databases
 
*----------------------------------------------------------------
>command CD
>Guidance
Use the CD command to change the current default directory.
.
Syntax:  CD path
>Action CDCDRC
>PARAMETERS
PATH 'path' C D=' '
+
CHOPT 'options ' C D=' '
-A 'all of below'
-Q 'show quota for new directory'
-S 'show number of subdirectories'
-T 'show creation & modification times'
-U 'show usage information'
 
*----------------------------------------------------------------
>command LD
>Action  CDLDC
>PARAMETERS
+
PATH   'Pathname       '  C D='*'
NLEVEL 'Number of levels to display' I D=1
CHOPT  'Options'          C D=' '
-H 'Write a header showing the command and options'
-C 'List the creation date & time of the directory/ies'
-M 'List the modification date & time of the directory/ies'
-O 'Display the number of objects'
-R 'List subdirectories recursively'
-S 'Display number of subdirectories'
-T 'Display the tags for the directory/ies'
-V 'Generate a "very wide" listing (132 columns)'
-W 'Generate a "wide" listing (80 columns)'
>GUIDANCE
Use the LD command to display subdirectories below the specified
level.
 
*----------------------------------------------------------------
>command LS
>Action  CDLSC
>PARAMETERS
+
PATH   'Pathname         ' C D=' '
KSN    'Key serial number' I D=0
BANK   'Bankname         ' C D=' '
ILNK1  'Index of the first link to be printed' I D=0
ILNK2  'Index of the last link to be printed' I D=0
IDAT1  'Index of the first word to be printed' I D=0
IDAT2  'Index of the last word to be printed' I D=0
CHOPT  'Options          ' C D=' '
-C 'display object count'
-D 'display key definitions'
-E 'display the experiment keys'
-G 'display keys in a generic fashion (RZPRNK)'
-K 'display all keys'
-L 'list only lowest level (end node) directories (D)'
-M 'show maxima and minima of validity range pairs'
-N 'display number of data words'
-P 'display pathname (D)'
-S 'display the system keys'
-T 'display insertion date and time (RZ value)'
-U 'display user keys'
-V 'display validity range pairs'
-Z 'dump ZEBRA bank with DZSHOW'
>GUIDANCE
Use the LS command to display the contents of a directory
If option Z is specified, the name of the bank(s) to display
may be given. If multiple banks are to be displayed, their names
should be separated by commas. Wild cards are permitted in
bank names.
.
Syntax:  LS <options>
 
*----------------------------------------------------------------
>command PWD
>Guidance
Use the PWD command to print the current (working) directory.
Syntax: PWD
>Action CDKPWD
>PARAMETERS
+
CHOPT 'Options ' C D=' '
-A 'Display alias name of current directory'
 
*----------------------------------------------------------------
>command SEARCH
>Action CDSEAC
>Guidance
Use the SEARCH command to search for something
>PARAMETERS
PATH 'path name' C D='*'
+
CHOPT 'Options  ' C D=' '
 
*----------------------------------------------------------------
>command STATUS
>Guidance
Use the STATUS command to print usage statistics on the
specified directory down NLEVEL levels.
Syntax:  STATUS path nlevel
>Action  CDRZST
>Parameters
+
PATH   'Path name ' C D=' '
NLEVEL 'number of levels' I D=99
CHOPT 'Options  ' C D=' '
 
*----------------------------------------------------------------
>command TREE
>Guidance
Use the TREE command to draw a directory tree starting at the
specified directory down NLEVEL levels.
Syntax:  TREE path nlevel
>Action  CDTREE
>Parameters
+
PATH   'Path name ' C D=' '
NLEVEL 'number of levels' I D=99
CHOPT  'options         ' C D=' '
-A 'show alias name (if any) for each directory'
-N 'show the number of objects for each directory if non-zero'
-O 'show the number of objects for each directory'
-S 'show the number of subdirectories for each directory'
-C 'show the date and time the directory was created'
-M 'show the date and time of the last modification (RZ)'
 
*----------------------------------------------------------------
>command SELECT
>Guidance
Use the SELECT command to specify the instant of validity for which
objects are required.
Syntax:  SELECT
>Action  CDSELE
>Parameters
ISEL1 'Primary selection'   I D=0
+
ISEL2 'Secondary selection' I D=0
ISEL3 'Tertiary selection'  I D=0
 
*----------------------------------------------------------------
>command ZOOM
>Action  CDZOOM
>PARAMETERS
+
PATH 'Pathname' C D=' '
>Guidance
Use the ZOOM command to descend the specified (wild-carded)
directory tree to the first lowest level directory that
contains one or more entries
.
e.g. ZOOM DATA
.
     ZOOM *DST/*/P88*
.
>MENU \HDBMAN
>Guidance
Utility commands
 
*----------------------------------------------------------------
>command DIR
>Guidance
Use the DIR command to issue a call to RZLDIR for the specified
path. This command is normally used for debug purposes only.
.
>Action CDRZLD
>Parameters
+
PATH 'path-name' C D=' '
CHOPT 'options ' C D=' '
-T 'list also subdirectory tree'
-X 'eXtended listing - show also OFFSET,REC1,REC2'
 
*----------------------------------------------------------------
>command EXTRACT
>Action  CDEXTQ
>PARAMETERS
+
PATH   'Pathname'        C D='*/*'
OUTPUT 'Output filename' C D=' '
CHOPT  'Options        ' C D='C'
>GUIDANCE
Use the EXTRACT command to copy a subset of the HEPDB catalogue
 
*----------------------------------------------------------------
>command MERGE
>Action  CDMERG
>PARAMETERS
+
INPUT  'Input filename' C D=' '
PATH   'Pathname'        C D='*/*'
CHOPT  'Options        ' C D='C'
>GUIDANCE
Use the MERGE command to merge an update file created by
the EXTRACT command into the specified path.
 
*----------------------------------------------------------------
>command MKDIR
>Guidance
Use the MKDIR command to create a directory
Syntax: MKDIR directory_name.
>Action CDMKDR
>PARAMETERS
DIRNAM 'directory' C D=' '
 
*----------------------------------------------------------------
>command RM
>Guidance
Use the RM command to logically remove an entry from the catalogue
Syntax:  RM
>Action  CDRMC
>PARAMETERS
+
 
*----------------------------------------------------------------
>command RMDIR
>Guidance
Use the RMDIR command to remove a directory from the catalogue.
.
N.B. directories can only be removed if
.
  1) They contain no subdirectories
.
  2) They contain no entries
.
If you wish to remove a complete directory tree that contains
no entries, use the RMTREE command.
.
Syntax:  RMDIR PATH
>Action  CDRMDC
>PARAMETERS
PATH 'directory' C D=' '
 
*----------------------------------------------------------------
>command RMTREE
>Guidance
Use the RMTREE command to remove a complete directory tree.
.
N.B. if any of the directories below the named directory
contain entries, then the command will be refused.
.
Syntax:  RMTREE PATH
>Action  CDRMTC
>PARAMETERS
PATH 'directory' C D=' '
 
*----------------------------------------------------------------
>command PACK
>Guidance
Use the PACK command to convert a date/time combination into
its packed representation.
.
If IDATE=ITIME=0, then the current date and time will be used.
.
Syntax:  PACK YYMMDD HHMM[SS]
>Action  CDKPAK
>PARAMETERS
+
DATE 'date in YYMMDD format' I D='0'
TIME 'time in HHMM/HHMMSS format' I D='0'
CHOPT 'options ' C D='M'
-M 'pack date/time using CDPKTM (default)'
-S 'pack date/time using CDPKTS'
 
*----------------------------------------------------------------
>command UNPACK
>Guidance
Use the UNPACK command to convert a date/time combination into
its packed representation
.
Syntax:  UNPACK packed-time
>Action  CDKUPK
>PARAMETERS
PACK 'packed date/date' I D='0'
CHOPT 'options ' C D='M'
-M 'unpack date/time using CDPKTM (default)'
-S 'unpack date/time using CDPKTS'
 
