*
* $Id: fmcdf.cdf,v 1.3 1997/01/07 11:20:31 jamie Exp $
*
* $Log: fmcdf.cdf,v $
* Revision 1.3  1997/01/07 11:20:31  jamie
* allow -I option through shell
*
* Revision 1.2  1996/08/01 11:07:55  jamie
* Changes to support FMVERI On/Off
*
* Revision 1.1.1.1  1996/03/07 15:18:09  mclareni
* Fatmen
*
*
*----------------------------------------------------------------
*
* FATMEN command definition file (CDF)
*
*----------------------------------------------------------------
>NAME FMCDF
>MENU FATMEN
>Guidance
FATMEN shell commands
 
*----------------------------------------------------------------
>Command INIT
>Guidance
Use the INIT command to initialise the FATMEN system for the specified
group or throng, e.g. ALEPH
.
Syntax INIT GROUP or THRONG
>Action FMTHRG
>PARAMETERS
GROUP 'Group or Throng name' C D=' '
*----------------------------------------------------------------
>Command END
>Action  FMKEND
>PARAMETERS
+
>Guidance
Use the END command when no more processing with current
FATMEN catalogue is required.
 
*----------------------------------------------------------------
>Command LOGLEVEL
>Action  FMLOGC
>PARAMETERS
+
LOGLEVEL 'Loglevel' I D=0
>Guidance
Use the LOGLEVEL command to set the FATMEN logging level.
Syntax:  LOGLEVEL loglevel
 
*----------------------------------------------------------------
>Command GIME
>Guidance
Use the GIME command to reaccess the disk of the service
machine which maintains the FATMEN catalogue. This command
has no parameters.
Syntax: GIME
>Action FMGIME
 
*----------------------------------------------------------------
>Command UPDATE
>Guidance
Use the UPDATE command to control how updates are sent to the FATMEN
server.
.
IFLAG can have the following values:
.
-1 = reset to system defaults
.
 0 = reset as per MAX and NGROUP and send any outstanding updates
to the server
.
 1 = reset as per MAX and NGROUP and purge any outstanding updates
Syntax:  UPDATE MAX NGROUP IFLAG
>Action  FMUPDC
>PARAMETERS
MAX    'maximum number of updates' I D=999
NGROUP 'number of updates to send together' I D=0
IFLAG  'flag to purge/send/reset'  I D=0
*----------------------------------------------------------------
>Command VERSION
>Guidance
Use the VERSION command to display the version of the FATMEN
software that you are running.
Syntax:  VERSION
>Action  FMVERS
>MENU \FMCAT
>Guidance
Commands to manipulate the FATMEN catalogue
 
*----------------------------------------------------------------
>Command CD
>Guidance
Use the CD command to change the current default directory.
.
Syntax:  CD path
>Action FMCD
>PARAMETERS
PATH 'path' C D='PWD'
+
CHOPT 'options ' C D=' '
-A 'all of below'
-Q 'show quota for new directory'
-S 'show number of subdirectories'
-T 'show creation & modification times'
-U 'show usage information'
*----------------------------------------------------------------
>Command FC
>Action  FMFC
>PARAMETERS
+
FILE 'File or pathname' C D='*'
OUT*PUT 'Output filename' C D='TTY'
CHOPT 'Options' C D=' '
-D 'display number of subdirectories at each level'
-F 'display number of files at each level'
-L 'display lowest level only, i.e. directories with no subdirectories'
-Z 'display only directories with no (zero) files'
>Guidance
Use the FC command to count the number of files in a directory.
FC will also count the number of files which match the specified pattern
.
Syntax:  FC <pattern>
 
*----------------------------------------------------------------
>Command LD
>Action  FMLD
>PARAMETERS
+
FILE 'Pathname' C D='*'
OUT*PUT 'Output filename' C D='TTY'
NLEVEL 'Number of levels to display' I D=1
CHOPT 'Options' C D=' '
-H 'write header line in output file'
-R 'list subdirectories recursively'
-V 'very wide' listing. As W, but 132 columns'
-W 'wide. Subdirectories are displayed in multi-column (80) format.'
>Guidance
Use the LD command to display the contents of a directory.
.
Syntax:  LD pathname <options>
.
Output: Redirect the output to the specified file
.
Nlevel: Number of levels to descend (with option R)
 
*----------------------------------------------------------------
>Command LS
>Action  FMLSC
>PARAMETERS
+
FILE   'File or pathname'  C D='*'
KSN    'Key serial number' I D=0
OUT*PUT 'Output filename  ' C D='TTY'
NAMES  'Names filename   ' C D=' '
CHOPT  'Options' C D=' '
-A 'list all attributes, except options Q and Z.'
-B 'brief listing - one line (80 columns) per entry'
-C 'display comment field associated with file'
-D 'listing suitable for generated a macro to delete entry'
-E 'extended listing - one line (132 columns) per entry'
-F 'list file attributes, such as start/end record and block'
-G 'list the full generic name of each file'
-H 'write header line in output file'
-I 'output sorted by Increasing generic name'
-J 'show just those files that are accessible'
-K 'list keys associated with this file (copy level, media type, location)'
-L 'list logical attributes, such as FATMEN file format _
(ZEBRA exchange etc.)'
-M 'list media attributes, such as VSN, VID, file sequence number for tape _
files, host type and operating system for disk files.'
-N 'lists dataset name on disk/tape of this file'
-O 'list owner, node and job of creator etc.'
-P 'list physical attributes, such as record format etc.'
-Q 'query TMS for corresponding volume'
-R 'show where the data Reside'
-S 'lists security details of this file (protection)'
-T 'list date and time of creation, last access etc.'
-U 'list user words.'
-V 'very wide listing. As W, but 132 columns'
-W 'wide listing. Filenames (part of generic name following _
last slash) are displayed in multi-column (80) format.'
-X 'each generic name displayed only once'
-Y 'verify bank contents using FMVERI.'
-Z 'dump ZEBRA bank with DZSHOW.'
>Guidance
Use the LS command to display the contents of a directory
or display information on a given file within the current or specified
directory.
.
Syntax:  LS file <options>
.
Output: Redirect the output to the specified file
 
*----------------------------------------------------------------
>Command MODIFY
>Guidance
Use the MOD command to modify an existing catalogue entry. Note
that certain fields cannot be changed using the MOD command,
notably the VSN, VID and FSEQ for tape files and the FILEID and
HOSTNAME for disk files. Use the MV command if you wish to modify
these fields.
.
e.g.
.
MOD FRED FSIZE=180
.
MOD TOM  -KL
.
>Action FMKMOD
>Parameters
GNAME 'Generic name         ' C D=' '
+
KSN    'Key serial number    ' I D=0
LOCCOD 'Location code        ' I D=0
DATREP 'Data representation  ' I D=0
MEDTYP 'Media type           ' I D=0
FFORM  'FATMEN file format   ' C D=' '
RECFM  'Record format        ' C D=' '
RECL   'Record length (words)' I D=0
BLOCK  'Block  length (words)' I D=0
FSIZE  'File size (MB)       ' I D=0
COMM   'Comment              ' C D=' '
CHOPT  'Options              ' C D=' '
-A 'modify all matching entries'
-I 'interactive mode : command line is ignored. Each field _
in the bank is displayed and a new value prompted for _
Alternatively, one or more of the following options may _
be given, in which case only the corresponding fields will _
be modified.'
-K 'modify keys. Note that if you modify the media type, _
a new entry will be created corresponding to the new media _
type, and the old entry will remain. To change the media type, _
the MV command is preferred.'
-F 'modify file description'
-L 'modify logical description'
-P 'modify physical description'
-T 'modify time values'
-C 'modify creator information'
-U 'modify user area'
*----------------------------------------------------------------
>Command NICK
>Action FMNIKK
>Guidance
Use the NICK command to search in the specified or default
names file for a given nickname
.
Syntax: NICK nickname
>PARAMETERS
NICK      'Nickname  ' C D=' '
+
NAMESFILE 'Names file' C D=' '
CHOPT     'Options   ' C D=' '
*----------------------------------------------------------------
>Command PWD
>Guidance
Use the PWD command to print the current (working) directory.
.
Syntax: PWD
>Action FMPWD
*----------------------------------------------------------------
>Command SCAN
>Guidance
Use the SCAN command to scan the FATMEN catalogue for the
files specified. This command will only display entries
that satisfy the current selection on location code, media
type and copy level/data representation.
.
If option D is specified, files will be listed in reverse
order. This is particularly efficient when deleting many
entries from a directory.
.
>Action FMSKAN
>PARAMETERS
+
PATH 'path name' C D='*'
NLEVEL 'Number of levels' I D=1
DSN  'Fileid/DSN' C D=' '
HOST 'Hostname  ' C D=' '
VID  'VID       ' C D=' '
USER 'Username  ' C D=' '
NMAT*CH '# to match' I D=0
CRE*ATED   'date range' C D=' '
CAT*ALOGED 'date range' C D=' '
ACC*ESSED  'date range' C D=' '
UFORM     'user file format' C D=' '
COMM*ENT   'comment' C D=' '
OUT*PUT 'Output filename ' C D='TTY'
STR*ING 'string to append to the RM command' C D=' '
CHOPT 'Options' C D=' '
-A 'list all attributes, except options Q and Z.'
-B 'brief listing - one line (80 columns) per entry'
-C 'display comment field associated with file'
-D 'listing suitable for generated a macro to delete entry'
-E 'extended listing - one line (132 columns) per entry'
-F 'list file attributes, such as start/end record and block'
-G 'list the full generic name of each file'
-H 'write header line in output file'
-I 'output sorted by Increasing generic name'
-J 'show just those files that are accessible'
-K 'list keys associated with this file (copy level, media type, location)'
-L 'list logical attributes, such as FATMEN file format _
(ZEBRA exchange etc.)'
-M 'list media attributes, such as VSN, VID, file sequence number for tape _
files, host type and operating system for disk files.'
-N 'lists dataset name on disk/tape of this file'
-O 'list owner, node and job of creator etc.'
-P 'list physical attributes, such as record format etc.'
-Q 'query TMS for corresponding volume'
-R 'show where the data Reside'
-S 'lists security details of this file (protection)'
-T 'list date and time of creation, last access etc.'
-U 'list user words.'
-V 'very wide listing. As W, but 132 columns
-W 'wide listing. Filenames (part of generic name following _
last slash) are displayed in multi-column (80) format.'
-X 'each generic name displayed only once'
-Y 'verify bank contents using FMVERI.'
-Z 'dump ZEBRA bank with DZSHOW.'
*----------------------------------------------------------------
>Command SEARCH
>Guidance
Use the SEARCH command to print the generic names of files which
match the specified criteria. Character fields may include the
* or % wild cards.
.
e.g. SEARCH * VID=I* # search current working directory for entries
# with VID's beginning with I.
.
If NMATCH is non-zero, SEARCH will stop after NMATCH matches have
been found.
.
Set the loglevel to <0 to stop the printing of the names of
files and directories searched.
.
Date and time ranges may be given, as in the following example:
which searches for files in the current directory that have been
accessed between 31st January, 1991 and midday on 30th June 1991
.
SEARCH * accessed=910131-910630.1200
.
Entries that match are displayed as with the LS command.
>Action FMSEAC
>PARAMETERS
PATH 'path name' C D='*'
+
DSN  'Fileid/DSN' C D=' '
HOST 'Hostname  ' C D=' '
VID  'VID       ' C D=' '
USER 'Username  ' C D=' '
NMAT*CH '# to match' I D=0
CRE*ATED   'date range' C D=' '
CAT*ALOGED 'date range' C D=' '
ACC*ESSED  'date range' C D=' '
UFORM     'user file format' C D=' '
COMM*ENT   'comment' C D=' '
OUT*PUT 'Output filename ' C D='TTY'
CHOPT 'Options' C D=' '
-A 'list all attributes, except options Q and Z.'
-B 'brief listing - one line (80 columns) per entry'
-C 'display comment field associated with file'
-D 'listing suitable for generated a macro to delete entry'
-E 'extended listing - one line (132 columns) per entry'
-F 'list file attributes, such as start/end record and block'
-G 'list the full generic name of each file'
-H 'write header line in output file'
-I 'output sorted by Increasing generic name'
-J 'show just those files that are accessible'
-K 'list keys associated with this file (copy level, media type, location)'
-L 'list logical attributes, such as FATMEN file format _
(ZEBRA exchange etc.)'
-M 'list media attributes, such as VSN, VID, file sequence number for tape _
files, host type and operating system for disk files.'
-N 'lists dataset name on disk/tape of this file'
-O 'list owner, node and job of creator etc.'
-P 'list physical attributes, such as record format etc.'
-Q 'query TMS for corresponding volume'
-R 'show where the data Reside'
-S 'lists security details of this file (protection)'
-T 'list date and time of creation, last access etc.'
-U 'list user words.'
-V 'very wide listing. As W, but 132 columns
-W 'wide listing. Filenames (part of generic name following _
last slash) are displayed in multi-column (80) format.'
-X 'each generic name displayed only once'
-Y 'verify bank contents using FMVERI.'
-Z 'dump ZEBRA bank with DZSHOW.'
*----------------------------------------------------------------
>Command TREE
>Guidance
Use the TREE command to draw a directory tree starting at the
specified directory down NLEVEL levels.
.
Syntax:  TREE path nlevel
>Action  FMTREK
>Parameters
+
PATH   'Path name ' C D=' '
NLEVEL 'number of levels' I D=99
OUT*PUT 'Output filename' C D='TTY'
 
*----------------------------------------------------------------
>Command VIEW
>Guidance
Use the VIEW command to edit a local disk file.
This command will be extended to support tape and remote files.
.
Syntax:  VIEW file <key>
>Action  FMEDIT
>PARAMETERS
FILE 'file' C D=' '
+
KSN 'Key serial number' I D=0
*----------------------------------------------------------------
>Command ZOOM
>Action  FMZOOM
>PARAMETERS
+
FILE 'Pathname' C D='*'
>Guidance
Use the ZOOM command to descend the specified (wild-carded)
directory tree to the first lowest level directory that
contains one or more files.
.
e.g. ZOOM DATA
.
     ZOOM *DST/*/P88*
.
If no wild-cards are present in the pathname, the characters /*
are automatically appended. To zoom down a tree starting with
.
*ALLD, use ZOOM *ALLD/*
.
>MENU \FMDATA
>Guidance
Commands to manipulate the data
 
*----------------------------------------------------------------
>Command COPY
 
>Action  FMCOPC
>PARAMETERS
GNAME 'Generic name' C D=' '
+
KS1       'Input Key serial number' I D=0
KS2       'Output Key serial number' I D=0
POOL      'TMS Pool    ' C D=' '
LIB*RARY  'TMS Library ' C D=' '
VSN       'VSN         ' C D=' '
VID       'VID         ' C D=' '
FSEQ      'FSEQ        ' I D=1
NODE      'Remote node ' C D=' '
FILE      'Remote file ' C D=' '
TRANS*PORT 'Transport mechanism' C D='TCPIP' OPTION
-TCPIP 'Use TCP/IP as transport'
-DECNET 'Use DECnet as transport (between VAX/VMS systems only)'
-CHEOPS 'queue for transfer via Olympus satellite'
LOC*COD    'Location code        ' I D=0
DAT*REP    'Data representation  ' I D=0
MED*TYP    'Media type           ' I D=0
CHOPT     'Options     ' C D=' '
-C 'perform copy using STAGE CHANGE'
-F 'perform copy using FZ input/output (permits conversion)'
-K 'keep network connection open between transfers'
-L 'Lock output tape volume using TMS LOCK command'
-P '(default) perform PHYSICAL copy - no data or format conversion'
-S 'STAGE IN the input file'
-Z 'STAGE OUT the output file'
>Guidance
Use the COPY command to copy the data referenced by a generic name.
N.B. if you wish to copy just a catalogue entry, use the command CP.
.
Syntax: COPY GNAME <options>
 
*----------------------------------------------------------------
>Command DUMP
>Guidance
Use the DUMP command to request a TAPEDUMP of the VID
corresponding to the specified generic name.
.
Syntax: DUMP generic-name key-serial-number
 
>Action  FMDUMC
>PARAMETERS
GENAM 'generic-name' C D=' '
+
KSN 'Key serial number' I D=0
BYTES 'Number of bytes/block to dump' I D=320
BLOCKS 'Number of blocks/file to dump' I D=1
FILES  'Number of files to dump' I D=1
CODE   'Character code (EBCDIC or ASCII)' C D='EBCDIC' OPTION
-ASCII 'Tape labels encoded in ASCII (AL)'
-EBCDIC 'Tape labels encoded in EBCDIC (SL)'
 
*----------------------------------------------------------------
>Command FIND
>Guidance
Use the FIND command to FIND the specified file and associate
it with the specified logical unit. If required, the file is
first staged to disk.
.
SYNTAX: FIND generic-name logical-unit
>Action FMFNDC
>PARAMETERS
FILE  'NAME OF FILE' C D=' '
LUNU  'Logical unit (nn/FTnnFlll/IOFILEnn etc.)' C D='0'
+
CHOPT 'Options' C D=' '
-D 'Make a duplicate into the SMCF robot (CERN only)'
-E 'When used with option T, add END option to SETUP command'
-H 'Stage the wHole tape, e.g. 1-E on VM, /FULLTAPE on VMS etc.'
-I 'Deselect -G option for SHIFT s/w'
-K 'KEEP option on STAGE OUT'
-L 'Override DCB in tape label with information in catalogue'
-N 'Do not add dataset name to stage command'
-Q 'Queue stage request, e.g. STAGE (NOWAIT'
-S 'Update FATMEN catalogue with file size returned from stage'
-V 'Verify file size in FATMEN catalogue against information _
returned by stage
-Y 'Do not issue stage command but write to file on unit LUNU'
 
*----------------------------------------------------------------
>Command MAKE
>Guidance
Use the MAKE command to access the specified file and associate
it with the specified logical unit in WRITE mode. For tape files,
this corresponds to output staging.
.
SYNTAX: MAKE FILE LUNU
>Action FMMAKC
>PARAMETERS
FILE 'NAME OF FILE' C D=' '
LUNU 'Logical unit (nn/FTnnFlll/IOFILEnn etc.)' C D='0'
 
>MENU \FMUTIL
>Guidance
Utility commands
 
*----------------------------------------------------------------
>Command CLR
>Guidance
Use the CLR command to clear the screen on a 3270 type terminal
.
Syntax: CLR
>Action FMCLR
 
*----------------------------------------------------------------
>Command CP
>Guidance
Use the CP command to copy a file entry.
.
N.B. if you wish to copy the DATA, use the command COPY.
.
The new entry will be added to the catalogue using the
'FMPUT' subroutine. Thus, if an attempt is made to copy
TO an entry with the same
name and hostname/DSN (disk files) or VID/VSN/FSEQ (tape files),
then this will be refused SILENTLY by the server.
.
Syntax: CP file1 file2 [key]
 
>Action  FMCPC
>PARAMETERS
FROM 'file1' C D=' '
TO   'file2' C D=' '
+
KSN  'key  ' I D=0
LOC*COD 'Location code        ' I D=0
DAT*REP 'Data representation  ' I D=0
MED*TYP 'Media type           ' I D=0
VSN    'vsn                ' C D=' '
VID    'vid                ' C D=' '
FSEQ   'fseq               ' I D=0
DSN    'fileid             ' C D=' '
HOST   'host               ' C D=' '
 
*----------------------------------------------------------------
>Command DIR
>Guidance
Use the DIR command to issue a call to RZLDIR for the specified
path. This command is normally used for debug purposes only.
.
Syntax: DIR path options
>Action FMRZLD
>Parameters
+
PATH 'path-name' C D=' '
OUTPUT 'output file name ' C D='TTY'
CHOPT 'options ' C D=' '
-A 'RZLDIR option A (list also objects created with A option in RZOUT/RZVOUT)'
-T 'List also directory tree'
-X 'eXtended listing'
 
*----------------------------------------------------------------
>Command EXTRACT
>Action  FMEXTR
>PARAMETERS
+
PATH   'Pathname'        C D='*/*'
OUTPUT 'Output filename' C D=' '
CHOPT  'Options        ' C D='C'
>Guidance
Use the EXTRACT command to copy a subset of the FATMEN catalogue
to a FZ alpha exchange format file.
Only those files that match the specified path and file name and
the current keys selection will be written to the output file.
e.g.
.
set/location 2
extr */m* my.fa
.
will only write information about files with location code 2.
All files begining with the letter M in all directories will
be processed.
 
*----------------------------------------------------------------
>Command LN
>Guidance
Use the LN command to make a link or alias to a generic name.
.
N.B. This commands uses the Unix syntax where TARGET-->SOURCE(?)
>Action  FMLNC
>PARAMETERS
SOURCE 'source file' C D=' '
TARGET 'target file' C D=' '
+
CHCOMM 'comment    ' C D=' '
IW1    'user word 1' I D=0
IW2    'user word 2' I D=0
IW3    'user word 3' I D=0
IW4    'user word 4' I D=0
IW5    'user word 5' I D=0
IW6    'user word 6' I D=0
IW7    'user word 7' I D=0
IW8    'user word 8' I D=0
IW9    'user word 9' I D=0
IW10   'user word 10' I D=0
CHOPT  'options    ' C D=' '
-C 'set the comment of the link to CHCOMM'
-U 'set the user word vector'
*----------------------------------------------------------------
>Command MEDIA
>Guidance
Use the MEDIA command to set or list attributes of a given
type of media.
e.g.
.
MEDIA 2 3480 CT1 200 38K M SL
.
would set the attributes of FATMEN media type 2 to
.
model:               3480
.
generic device type: CT1 (used on STAGE/SETUP requests)
.
capacity:            200 MB (maximum size on STAGE command)
.
density:             38K
.
label  :             SL
.
MEDIA with no arguments lists the current settings
.
MEDIA 3 lists the settings for media type 3
.
>Action FMSMDK
>PARAMETERS
+
MEDIA 'FATMEN media type' I D=2
TYPE  'device type      ' C D='3480'
MODEL 'generic device   ' C D='CT1 '
SIZE  'capacity in MB   ' C D='200 '
DENS  'density          ' C D='38K '
MNTP  'mount-type ' C D='M' OPTION
-M 'manually mounted volume'
-R 'robotically mounted volume'
LABL  'label-type ' C D='SL' OPTION
-SL 'Standard label tape (i.e. IBM label)'
-AL 'ANSI standard label (ascii)'
-NL 'No label'
 
*----------------------------------------------------------------
>Command MKDIR
>Guidance
Use the MKDIR command to create a directory.
.
Syntax: MKDIR directory_name.
>Action FMMKDR
>PARAMETERS
DIRNAM 'directory' C D=' '
*----------------------------------------------------------------
>Command MV
>Guidance
Use the MV command to 'move' or rename a file.
.
Syntax: MV file1 file2 ksn
>Action  FMMVC
>PARAMETERS
FROM   'source generic name' C D=' '
TO     'target generic name' C D=' '
+
KSN    'key serial number  ' I D=0
LOC*COD 'Location code        ' I D=0
DAT*REP 'Data representation  ' I D=0
MED*TYP 'Media type           ' I D=0
VSN    'vsn                ' C D=' '
VID    'vid                ' C D=' '
FSEQ   'fseq               ' I D=0
DSN    'fileid             ' C D=' '
HOST   'host               ' C D=' '
*----------------------------------------------------------------
>Command RM
>Guidance
Use the RM command to remove a file from the catalogue.
.
Syntax:  RM file <key> <dsn> <host> <vid> <user> <pool> <prot> <chopt>
>Action  FMRMC
>PARAMETERS
FILE 'file' C D=' '
+
KSN 'Key serial number' I D=0
DSN  'Fileid/DSN' C D=' '
HOST 'Hostname  ' C D=' '
VID  'VID       ' C D=' '
USER 'Username  ' C D=' '
POOL 'Pool to return tape to' C D=' '
PROT 'Protection group      ' C D=' '
CHOPT 'Options'         C D=' '
-A 'remove all occurances of this generic name'
-E 'if the entry points to a disk file, erase this file (if accessible)'
-I 'prompt before removing each matching entry _
Turned on automatically if option A is given'
-N 'do not prompt in case of option A. To be used with caution'
-F 'free tape associated with specified entry'
-G 'set protection group to group specified _
If no group is specified, then *None will be _
used, unless option P is given, in which case _
the POOL name will be used.'
-P 'when used with option F, allows privileged TMS _
user to free anyones tapes (within a group)'
-U 'unlock or write-enable tape'
-D 'delete TMS tag'
-B 'binary TMS tag'
-T 'text TMS tag'
 
*----------------------------------------------------------------
>Command RMDIR
>Guidance
Use the RMDIR command to remove a directory from the catalogue.
.
N.B. directories can only be removed if
.
  1) They contain no subdirectories
.
  2) They contain no files
.
If you wish to remove a complete directory tree that contains
no files, use the RMTREE command.
.
Syntax:  RMDIR DIRECTORY_NAME
>Action  FMRMDC
>PARAMETERS
FILE 'directory' C D=' '
*----------------------------------------------------------------
>Command RMLN
>Guidance
Use the RMLN command to remove a link from the catalogue.
.
Syntax:  RMLN chlink [lwrite] [chfile] [chopt]
>Action FMRMLC
>PARAMETERS
CHLINK '[wild-carded] generic name' C D=' '
+
LWRITE 'logical unit for output   ' I D=0
CHFILE 'file name for output      ' C D=' '
CHOPT  'options                   ' C D=' '
-P 'print names of dangling links'
-D 'write names of dangling links in "rm ksn" format'
-R 'remove dangling links'
-F 'redirect output to CHFILE on LWRITE'
*----------------------------------------------------------------
>Command RMTREE
>Guidance
Use the RMTREE command to remove a complete directory tree.
.
N.B. if any of the directories below the named directory
contain files, then the command will be refused.
.
Syntax:  RMTREE DIRECTORY_NAME
>Action  FMRMTC
>PARAMETERS
FILE 'directory' C D=' '
 
*----------------------------------------------------------------
>Command TOUCH
>Guidance
Use the TOUCH command to reinsert an existing entry in the catalogue.
If a negative key serial number is given, all matching entries
will be updated.
.
Syntax:  TOUCH GENERIC-NAME KEY-SERIAL-NUMBER CHOPT
>Action  FMTCHC
>PARAMETERS
FILE 'file' C D=' '
+
KSN 'Key serial number' I D=0
CHOPT  'Options'        C D=' '
-A 'set account field'
-C 'clear comment field'
-O 'reset owner, node and job of creator etc.'
-T 'update date and time of last access'
-U 'zero use count'
>MENU \TMS
>Guidance
Commands that interface to the TMS
 
*----------------------------------------------------------------
>Command ALLOCATE
>Guidance
Use the ALLOCATE command to add a new tape file to the FATMEN catalogue
A tape is allocated from the specified pool. The file sequence number
is always set to 1.
>Action FMKATT
>Parameters
POOL      'TMS Pool    ' C D=' '
LIBRARY   'TMS Library ' C D=' '
GNAME 'generic file name' C D=' '
DSN   'disk dataset name' C D=' '
FFORM 'file format (FZ, FA, FX, RZ, EP, UN, AS)' C D='UN'
CPLEV 'copy level (0=original, 1=copy etc.)' I D=0
HOSTN 'host name (e.g. CERNVM)' C D='thisnode'
+
COMM  'comment (in quotes)' C D='Added via FATMEN CLI'
RECFM 'record format (e.g. FB, VBS)' C D='    '
LRECL 'record length (in words)' I D=0
BLOCK 'block length (in words)' I D=0
FSIZE 'file size (in megabytes)' I D=0
MEDIA 'media type (2=3480,3=3420,4=8200,...)' I D=2
*----------------------------------------------------------------
>Command LOCK
>Action  FMLOKK
>PARAMETERS
GENAM 'generic name' C D=' '
+
KSN 'Key serial number' I D=0
CHOPT 'Options' C D=' '
>Guidance
Use the LOCK command to disable WRITE access to the tape on which
the file specified resides. If a negative key serial number is given,
all entries will be locked.
.
Syntax:  LOCK GENERIC-NAME KEY-SERIAL-NUMBER <CHOPT>
 
*----------------------------------------------------------------
>Command TAG
>Guidance
Use the TAG command to get, set or delete the TMS TAG
associated with the a tape volume that corresponds to
the specified generic name.
.
Syntax: TAG generic-name key-serial-number tag-text chopt
.
If option S is specified without any TAG text, the tag
is sent to the generic name.
>Action  FMTAGC
>PARAMETERS
FILE 'file' C D=' '
+
KSN 'Key serial number' I D=0
TAG    'Tag           ' C D=' '
CHOPT  'Options       ' C D=' '
-D 'delete the tag'
-G 'get & display the tag'
-S 'set the tag'
-B 'to select the BINARY  tag (stored as CHARACTER*255)'
-T 'to select the TEXT    tag (default)'
-V 'to select the VOLINFO tag (SYSTEM tag at IN2P3)'
*----------------------------------------------------------------
>Command UNLOCK
>Action  FMULCC
>PARAMETERS
GENAM 'generic name' C D=' '
+
KSN 'Key serial number' I D=0
CHOPT 'Options' C D=' '
>Guidance
Use the UNLOCK command to enable WRITE access to the tape on which
the file specified resides. If a negative key serial number is given,
all entries for the specified generic name will be unlocked.
.
Syntax:  UNLOCK GENERIC-NAME KEY-SERIAL-NUMBER <CHOPT>
*----------------------------------------------------------------
>Menu \ADD
>Guidance
Commands to add entries to the FATMEN catalogue
 
*----------------------------------------------------------------
>Command TAPE
>Guidance
Use the ADD/TAPE command to add a new tape file to the FATMEN catalogue.
>Action FMKADT
>Parameters
VSN   'volume serial number' C D=' '
VID   'visual identifier'    C D='VSN'
FSEQ  'file sequence number' I D=1
GNAME 'generic file name'    C D=' '
DSN   'dataset name'         C D=' '
FFORM 'file format (FZ, FA, FX, RZ, EP, UN, AS)' C D='UN'
CPLEV 'copy level (0=original, 1=copy etc.)' I D=0
HOSTN 'host name (e.g. CERNVM)' C D='THISNODE'
+
COMM  'comment (in SINGLE quotes)' C D='Added via FATMEN CLI'
RECFM 'record format (e.g. FB, VBS)' C D='    '
LRECL 'record length (in words)' I D=0
BLOCK 'block length (in words)' I D=0
FSIZE 'file size (in megabytes)' I D=0
MED*IA 'media type (2=3480,3=3420,4=8200,...)' I D=2
LOC*COD 'location code' I D=1
USER1  'user word 1 ' I D=0
USER2  'user word 2 ' I D=0
USER3  'user word 3 ' I D=0
USER4  'user word 4 ' I D=0
USER5  'user word 5 ' I D=0
USER6  'user word 6 ' I D=0
USER7  'user word 7 ' I D=0
USER8  'user word 8 ' I D=0
USER9  'user word 9 ' I D=0
USER10 'user word 10' I D=0
*----------------------------------------------------------------
>Command DISK
>Guidance
Use the ADD/DISK command to add a new disk file to the FATMEN catalogue.
>Action FMKADD
>Parameters
GNAME 'generic file name' C D=' '
DSN   'disk dataset name' C D=' '
FFORM 'file format (FZ, FA, FX, RZ, EP, UN, AS)' C D='UN'
CPLEV 'copy level (0=original, 1=copy etc.)' I D=0
HOSTN 'host name (e.g. CERNVM)' C D='thisnode'
+
COMM  'comment (in quotes)' C D='Added via FATMEN CLI'
RECFM 'record format (e.g. FB, VBS)' C D='    '
LRECL 'record length (in words)' I D=0
BLOCK 'block length (in words)' I D=0
FSIZE 'file size (in megabytes)' I D=0
LOC*COD 'location code' I D=1
USER1  'user word 1 ' I D=0
USER2  'user word 2 ' I D=0
USER3  'user word 3 ' I D=0
USER4  'user word 4 ' I D=0
USER5  'user word 5 ' I D=0
USER6  'user word 6 ' I D=0
USER7  'user word 7 ' I D=0
USER8  'user word 8 ' I D=0
USER9  'user word 9 ' I D=0
USER10 'user word 10' I D=0
*----------------------------------------------------------------
>Menu \SET
>Guidance
Commands to set FATMEN options
 
*----------------------------------------------------------------
>Command Location
>Guidance
Use the SET/LOCATION command to define the list of location codes
to be used to select datasets. Only datasets with a location code
in the specified range will be visible via LS commands, or accessible
through FIND or MAKE commands.
.
e.g.
.
set/loc 1,3,5-17 # set location codes
.
set/loc -1 # Clear location codes
>Action FMKLOC
>Parameters
RANGE 'range of location codes' C D='-1'
*----------------------------------------------------------------
>Command DATAREP
>Guidance
Use the SET/DATAREP command to define the list of data representation
codes
to be used to select datasets. Only datasets with a data representation
in the specified range will be visible via LS commands, or accessible
through FIND or MAKE commands.
.
e.g.
.
set/datarep 1,3,5-17 # set
.
set/datarep -1 # Clear
>Action FMKCPL
>Parameters
RANGE 'range of data representation values' C D='-1'
*----------------------------------------------------------------
>Command COPYLEVEL
>Guidance
Use the SET/COPYLEVEL command to define the list of copy levels
to be used to select datasets. Only datasets with a copy levels
in the specified range will be visible via LS commands, or accessible
through FIND or MAKE commands.
.
e.g.
.
set/copylevel 1,3,5-17 # set copy levels
.
set/copylevel -1 # Clear copy levels
>Action FMKCPL
>Parameters
RANGE 'range of copy levels' C D='-1'
*----------------------------------------------------------------
>Command MEDIATYPE
>Guidance
Use the SET/MEDIATYPE command to define the list of media types
to be used to select datasets. Only datasets with a media type
in the specified range will be visible via LS commands, or accessible
through FIND or MAKE commands.
.
e.g.
.
set/mediatype 1,3,5-17 # set  media types
.
set/mediatype -1 # Clear media types
>Action FMKMTP
>Parameters
RANGE 'range of media types' C D='-1'
*----------------------------------------------------------------
>Command VERIFY
>Guidance
Use the SET/VERIFY command to turn verification of FATMEN updates
on/off. If verification is turned off, updates are still checked,
and so warning messages are still displayed. 
.
>Action FMKVER
>Parameters
+
OPTION C 'Option' D='ON' R='ON,OFF'
*----------------------------------------------------------------
>Command USERWORDS
>Guidance
Use the SET/USERWORDS command to define the ranges for the user
words that will be used in subsequent SEARCH commands.
.
e.g.
.
set/user 1-10 3 5 17 99-103 6 72 777 888-999 42
>Action FMKUWD
>Parameters
+
UWORD1 'range or value for user word 1 ' C D=' '
UWORD2 'range or value for user word 2 ' C D=' '
UWORD3 'range or value for user word 3 ' C D=' '
UWORD4 'range or value for user word 4 ' C D=' '
UWORD5 'range or value for user word 5 ' C D=' '
UWORD6 'range or value for user word 6 ' C D=' '
UWORD7 'range or value for user word 7 ' C D=' '
UWORD8 'range or value for user word 8 ' C D=' '
UWORD9 'range or value for user word 9 ' C D=' '
UWORD10 'range or value for user word 10 ' C D=' '
 
*----------------------------------------------------------------
>Command DESTINATION
>Guidance
Use the SET/DESTINATION command to set the current destination site
for CHEOPS transfers.
>Action FMKDST
>Parameters
DEST 'destination site' C D=' '
 
*----------------------------------------------------------------
>Command SOURCE
>Guidance
Use the SET/SOURCE command to set the current source site for
CHEOPS transfers.
>Action FMKSRC
>Parameters
SOURCE 'source site' C D=' '
 
*----------------------------------------------------------------
>Command LOCCODES
>Guidance
Use the SET/LOCCODES command to load location code definitions
from the specified file.
>Action FMKLCC
>Parameters
FILE 'definition file' C D=' '
 
*----------------------------------------------------------------
>Menu \SHOW
>Guidance
Commands to show FATMEN options
 
*----------------------------------------------------------------
>Command Catalogues
>Guidance
Use the SHOW/CATALOGUES command to display a list of available
FATMEN catalogues.
>Action FMSCAT
 
*----------------------------------------------------------------
>Command Loccodes
>Guidance
Use the SHOW/LOCCODES command to show the list of location codes
and their meanings, as defined in the file FATMEN.LOCCODES or the
user definitions loaded using SET/LOCCODES.
>Action FMSLCC
 
*----------------------------------------------------------------
>Command Location
>Guidance
Use the SHOW/LOCATION command to show the list of location codes
that are currently in effect.  Only datasets with a location code
in the displayed range will be visible via LS commands, or accessible
through FIND or MAKE commands.
>Action FMSLOC
*----------------------------------------------------------------
>Command DATAREP
>Guidance
Use the SHOW/DATAREP command to show the list of data representations
that are currently in effect.  Only datasets with a data representation
in the displayed range will be visible via LS commands, or accessible
through FIND or MAKE commands.
>Action FMSCPL
*----------------------------------------------------------------
>Command COPYLEVEL
>Guidance
Use the SHOW/COPYLEVEL command to show the list of copy levels
that are currently in effect.  Only datasets with a copy level
in the displayed range will be visible via LS commands, or accessible
through FIND or MAKE commands.
>Action FMSCPL
*----------------------------------------------------------------
>Command MEDIATYPE
>Guidance
Use the SHOW/MEDIATYPE command to show the list of media types
that are currently in effect.  Only datasets with a media type
in the displayed range will be visible via LS commands, or accessible
through FIND or MAKE commands.
>Action FMSMTP
*----------------------------------------------------------------
>Command USERWORDS
>Guidance
Use the SHOW/USERWORDS command to show the list of ranges for
the 10 user words that are currently in effect.
Only entries which match the current user word ranges will
be found be the SEARCH command.
>Action FMSUWD
 
*----------------------------------------------------------------
>Command DESTINATION
>Guidance
Use the SHOW/DESTINATION command to show current destination site
for CHEOPS transfers
>Action FMSDST
 
*----------------------------------------------------------------
>Command SOURCE
>Guidance
Use the SHOW/SOURCE command to show current source site for
CHEOPS transfers
>Action FMSSRC
*----------------------------------------------------------------
>Command VERIFY
>Guidance
Use the SHOW/VERIFY command to display whether verification of 
FATMEN updates is turned on or off. If verification is turned off, 
updates are still checked, and so warning messages are still displayed. 
.
>Action FMSVER
*----------------------------------------------------------------
>Command UPDATES
>Guidance
Use the SHOW/UPDATES command to show how many catalogue updates
have been queued, and how many tokens remain
>Action FMSUPD
