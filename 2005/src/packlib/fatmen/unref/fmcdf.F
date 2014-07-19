*
* $Id: fmcdf.F,v 1.1.1.1 1996/03/07 15:18:24 mclareni Exp $
*
* $Log: fmcdf.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:24  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
*----------------------------------------------------------------
*
* FATMEN comand definition file (CDF)
*
*----------------------------------------------------------------
>NAME FMCDF
>MENU FATMEN
>Guidence
Summary of FATMEN shell comands:
 
*----------------------------------------------------------------
>Comand ALLOCATE
>Guidence
Use the ALLOCATE comand to add a new tape file to the FATMEN catalogue
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
>Comand CD
>Guidence
Use the CD comand to change the current default directory.
Syntax:  CD path
Options: A - all of below
         Q - show quota for new directory
         S - show number of subdirectories
         T - show creation & modification times
         U - show usage information
>Action FMCD
>PARAMETERS
PATH 'path' C D='PWD'
+
CHOPT 'options ' C D=' '
*----------------------------------------------------------------
>Comand CLR
>Guidence
Use the CLR comand to clear the screen on a 3270 type terminal
Syntax: CLR
>Action FMCLR
*----------------------------------------------------------------
>Comand COPY
 
>Action  FMCOPC
>PARAMETERS
GNAME 'Generic name' C D=' '
+
KS1       'Input Key serial number' I D=0
KS2       'Output Key serial number' I D=0
POOL      'TMS Pool    ' C D=' '
LIBRARY   'TMS Library ' C D=' '
VSN       'VSN         ' C D=' '
VID       'VID         ' C D=' '
FSEQ      'FSEQ        ' I D=1
NODE      'Remote node ' C D=' '
FILE      'Remote file ' C D=' '
TRANSPORT 'Transport mechanism' C D='TCPIP'
CHOPT     'Options     ' C D=' ' R='CFKLSTZ'
>Guidence
Use the COPY comand to copy the data referenced by a generic name.
N.B. if you wish to copy just a catalogue entry, use the comand CP.
Syntax: COPY GNAME <options>
Transport:
  TCPIP  (default)
  DECnet (between VAX/VMS systems only)
  CHEOPS (queue for transfer via Olympus satellite)
Options:
  C - perform copy using STAGE CHANGE
  K - keep network connection open between transfers
  L - Lock output tape volume using TMS LOCK comand
  S - STAGE IN the input file
*----------------------------------------------------------------
>Comand CP
>Guidence
Use the CP comand to copy a file entry.
N.B. if you wish to copy the DATA, use the comand COPY
Syntax: CP file1 file2
 
>Action  FMCPC
>PARAMETERS
FROM 'file1' C D=' '
TO   'file2' C D=' '
*----------------------------------------------------------------
>Comand DIR
>Guidence
Use the DIR command to issue a call to RZLDIR for the specified
path. This command is normally used for debug purposes only.
Syntax: DIR path options
Options: T - list also subdirectory tree
>Action FMRZLD
>Parameters
+
PATH 'path-name' C D=' '
OUTPUT 'output file name ' C D='TTY'
CHOPT 'options ' C D=' '
*----------------------------------------------------------------
>Comand DUMP
>Guidence
Use the DUMP comand to request a TAPEDUMP of the VID
corresponding to the specified generic name.
Syntax: DUMP generic-name key-serial-number
 
>Action  FMDUMC
>PARAMETERS
GENAM 'generic-name' C D=' '
+
KSN 'Key serial number' I D=0
BYTES 'Number of bytes/block to dump' I D=320
BLOCKS 'Number of blocks/file to dump' I D=1
FILES  'Number of files to dump' I D=1
CODE   'Character code (EBCDIC or ASCII)' C D='EBCDIC'
*----------------------------------------------------------------
>Comand END
>Action  FMKEND
>PARAMETERS
+
>GUIDANCE
Use the END command when no more processing with current
FATMEN catalogue is required.
*----------------------------------------------------------------
>Comand EXTRACT
>Action  FMEXTR
>PARAMETERS
+
PATH   'Pathname'        C D='*/*'
OUTPUT 'Output filename' C D=' '
CHOPT  'Options        ' C D='C'
>GUIDANCE
Use the EXTRACT command to copy a subset of the FATMEN catalogue
to a FZ alpha exchange format file.
Only those files that match the specified path and file name and
the current keys selection will be written to the output file.
e.g.
 
set/location 2
extr */m* my.fa
 
will only write information about files with location code 2.
All files begining with the letter M in all directories will
be processed.
*----------------------------------------------------------------
>Comand FC
>Action  FMFC
>PARAMETERS
+
FILE 'File or pathname' C D='*'
OUTPUT 'Output filename' C D='TTY'
CHOPT 'Options' C D=' '
>GUIDANCE
Use the FC comand to count the number of files in a directory.
FC will also count the number of files which match the specified pattern
Syntax:  FC <pattern>
Options:
  D - display number of subdirectories at each level
  F - display number of files at each level
  L - display lowest level only, i.e. directories with no subdirectories
  Z - display only directories with no (zero) files
*----------------------------------------------------------------
>Comand FIND
>Guidence
Use the FIND comand to FIND the specified file and associate
it with the specified logical unit. If required, the file is
first staged to disk.
SYNTAX: FIND generic-name logical-unit
>Action FMFNDC
>PARAMETERS
FILE 'NAME OF FILE' C D=' '
LUNU 'Logical unit (nn/FTnnFlll/IOFILEnn etc.)' C D='0'
*----------------------------------------------------------------
>Comand GIME
>Guidence
Use the GIME comand to reaccess the disk of the service
machine which maintains the FATMEN catalogue. This comand
has no parameters.
Syntax: GIME
>Action FMGIME
*----------------------------------------------------------------
>Comand INIT
>Guidence
Use the INIT comand to initialise the FATMEN system for the specified
group or throng, e.g. ALEPH
Syntax INIT GROUP or THRONG
>Action FMTHRG
>PARAMETERS
GROUP 'Group or Throng name' C D=' '
*----------------------------------------------------------------
>Comand LD
>Action  FMLD
>PARAMETERS
+
FILE 'Pathname' C D='*'
OUTPUT 'Output filename' C D='TTY'
NLEVEL 'Number of levels to display' I D=1
CHOPT 'Options' C D=' '
>GUIDANCE
Use the LD comand to display the contents of a directory
Syntax:  LD pathname <options>
Output: Redirect the output to the specified file
Nlevel: Number of levels to descend (with option R)
Options:
  H - write header line in output file
  R - list subdirectories recursively
  V - 'very wide' listing. As W, but 132 columns
  W - 'wide'. Subdirectories are displayed in multi-column (80) format.
*----------------------------------------------------------------
>Comand LOCK
>Action  FMLOCC
>PARAMETERS
GENAM 'generic name' C D=' '
+
KSN 'Key serial number' I D=0
CHOPT 'Options' C D=' '
>GUIDANCE
Use the LOCK comand to disable WRITE access to the tape on which
the file specified resides. If a negative key serial number is given,
all entries will be locked.
Syntax:  LOCK GENERIC-NAME KEY-SERIAL-NUMBER <CHOPT>
*----------------------------------------------------------------
>Comand LOGLEVEL
>Action  FMLOGC
>PARAMETERS
+
LOGLEVEL 'Loglevel' I D=0
>GUIDANCE
Use the LOGLEVEL comand to set the FATMEN logging level.
Syntax:  LOGLEVEL loglevel
*----------------------------------------------------------------
>Comand LS
>Action  FMLSC
>PARAMETERS
+
FILE   'File or pathname' C D='*'
OUTPUT 'Output filename'  C D='TTY'
CHOPT  'Options'          C D=' '
>GUIDANCE
Use the LS comand to display the contents of a directory
or display information on a given file within the current or specified
directory
Syntax:  LS file <options>
Options:
  A - list all attributes, except options Q and Z.
  B - brief listing - one line (80 columns) per entry
  C - display comment field associated with file
  D - listing suitable for generated a macro to delete entry
  E - extended listing - one line (132 columns) per entry
  F - list file attributes, such as start/end record and block
  G - list the full generic name of each file
  H - write header line in output file
  I - output sorted by Increasing generic name
#if defined(CERNLIB_NEVER)
  j - <free>
#endif
  K - list keys associated with this file (copy level, media type, location)
  L - list logical attributes, such as FATMEN file format
      (ZEBRA exchange etc.)
  M - list media attributes, such as VSN, VID, file sequence number for tape
      files, host type and operating system for disk files.
  N - lists dataset name on disk/tape of this file
  O - list owner, node and job of creator etc.
  P - list physical attributes, such as record format etc.
  Q - query TMS for corresponding volume
#if defined(CERNLIB_NEVER)
  r - <free>
#endif
  S - lists security details of this file (protection)
  T - list date and time of creation, last access etc.
  U - list user words.
  V - 'very wide' listing. As W, but 132 columns
  W - 'wide' listing. Filenames (part of generic name following
      last slash) are displayed in multi-column (80) format.
  X - each generic name displayed only once
  Y - verify bank contents using FMVERI.
  Z - dump ZEBRA bank with DZSHOW.
Output: Redirect the output to the specified file
*----------------------------------------------------------------
>Comand MAKE
>Guidence
Use the MAKE comand to access the specified file and associate
it with the specified logical unit in WRITE mode. For tape files,
this corresponds to output staging.
SYNTAX: MAKE  .
>Action FMMAKC
>PARAMETERS
FILE 'NAME OF FILE' C D=' '
LUNU 'Logical unit (nn/FTnnFlll/IOFILEnn etc.)' C D='0'
*----------------------------------------------------------------
>Comand MEDIA
>Guidence
Use the MEDIA command to set or list attributes of a given
type of media.
e.g.
 
MEDIA 2 3480 CT1 200 38K M SL
 
would set the attributes of FATMEN media type 2 to
 
model:               3480
generic device type: CT1 (used on STAGE/SETUP requests)
capacity:            200 MB (maximum size on STAGE command)
density:             38K
label  :             SL
 
MEDIA with no arguments lists the current settings
 
MEDIA 3 lists the settings for media type 3
 
>Action FMSMDK
>PARAMETERS
+
MEDIA 'FATMEN media type' I D=2
TYPE  'device type      ' C D='3480'
MODEL 'generic device   ' C D='CT1 '
SIZE  'capacity in MB   ' C D='200 '
DENS  'density          ' C D='38K '
MNTP  'mount-type (M/R) ' C D='M'
LABL  'label-type (SL/AL/NL) ' C D='SL'
 
*----------------------------------------------------------------
>Comand MKDIR
>Guidence
Use the MKDIR comand to create a directory
Syntax: MKDIR directory_name.
>Action FMMKDR
>PARAMETERS
DIRNAM 'directory' C D=' '
*----------------------------------------------------------------
>Comand MV
>Guidence
Use the MV comand to 'move' or rename a file
Syntax: MV file1 file2
>Action  FMMVC
>PARAMETERS
FROM 'file1' C D=' '
TO   'file2' C D=' '
*----------------------------------------------------------------
>Comand PWD
>Guidence
Use the PWD comand to print the current (working) directory.
Syntax: PWD
>Action FMPWD
*----------------------------------------------------------------
>Comand RM
>Guidence
Use the RM comand to remove a file from the catalogue
Syntax:  RM file <key> <dsn> <host> <vid> <user> <pool> <prot> <chopt>
*
*     Options: A - remove all occurances of this generic name
*              I - prompt before removing each matching entry
*              F - free tape associated with specified entry
*              G - set protection group to group specified
*                  If no group is specified, then *None will be
*                  used, unless option P is given, in which case
*                  the POOL name will be used.
*              P - when used with option F, allows privileged TMS
*                  user to free anyones tapes (within a group)
*              U - 'unlock' or write-enable tape
*
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
*----------------------------------------------------------------
>Comand RMDIR
>Guidence
Use the RMDIR comand to remove a directory from the catalogue.
N.B. directories can only be removed if
  1) They contain no subdirectories
  2) They contain no files
 
If you wish to remove a complete directory tree that contains
no files, use the RMTREE command.
 
Syntax:  RMDIR DIRECTORY_NAME
>Action  FMRMDC
>PARAMETERS
FILE 'directory' C D=' '
*----------------------------------------------------------------
>Comand RMTREE
>Guidence
Use the RMTREE comand to remove a complete directory tree.
N.B. if any of the directories below the named directory
contain files, then the command file be refused.
Syntax:  RMTREE DIRECTORY_NAME
>Action  FMRMTC
>PARAMETERS
FILE 'directory' C D=' '
*----------------------------------------------------------------
>Comand SEARCH
>Guidence
Use the search comand to print the generic names of files which
match the specified criteria. Character fields may include the
* or % wild cards.
 
e.g. SEARCH * VID=I* # search current working directory for entries
# with VID's beginning with I.
 
If NMATCH is non-zero, SEARCH will stop after NMATCH matches have
been found.
 
Set the loglevel to <0 to stop the printing of the names of
files and directories searched.
 
Date and time ranges may be given, as in the following example:
which searches for files in the current directory that have been
accessed between 31st January, 1991 and midday on 30th June 1991
 
SEARCH * accessed=910131-910630.1200
 
Entries that match are displayed as with the LS comand.
>Action FMSEAC
>PARAMETERS
PATH 'path name' C D='*'
+
DSN  'Fileid/DSN' C D=' '
HOST 'Hostname  ' C D=' '
VID  'VID       ' C D=' '
USER 'Username  ' C D=' '
NMATCH '# to match' I D=0
CREATED   'date range' C D=' '
CATALOGED 'date range' C D=' '
ACCESSED  'date range' C D=' '
UFORM     'user file format' C D=' '
COMMENT   'comment' C D=' '
OUTPUT 'Output filename ' C D='TTY'
CHOPT 'Options  ' C D=' '
  H - write header line in output file
*----------------------------------------------------------------
>Comand TAG
>Guidence
Use the TAG comand to get, set or delete the TMS TAG
associated with the a tape volume that corresponds to
the specified generic name.
 
Syntax: TAG generic-name key-serial-number tag-text chopt
 
Options: D - delete the tag
         G - get & display the tag
         S - set the tag
 
         B - to select the BINARY tag (stored as CHARACTER*255)
         T - to select the TEXT   tag (default)
 
If option G is specified without any TAG text, the tag
is sent to the generic name.
>Action  FMTAGC
>PARAMETERS
FILE 'file' C D=' '
+
KSN 'Key serial number' I D=0
TAG    'Tag           ' C D=' '
CHOPT  'Options       ' C D=' ' R='BDGST'
*----------------------------------------------------------------
>Comand TOUCH
>Guidence
Use the TOUCH comand to reinsert an existing entry in the catalogue.
If a negative key serial number is given, all matching entries
will be updated.
Syntax:  TOUCH GENERIC-NAME KEY-SERIAL-NUMBER CHOPT
Options:
  O - reset owner, node and job of creator etc.
  T - update date and time of last access
  U - zero use count
>Action  FMTCHC
>PARAMETERS
FILE 'file' C D=' '
+
KSN 'Key serial number' I D=0
CHOPT  'Options'        C D=' ' R='OTU'
*----------------------------------------------------------------
>Comand TREE
>Guidence
Use the TREE command to draw a directory tree starting at the
specified directory down NLEVEL levels.
Syntax:  TREE path nlevel
>Action  FMTREK
>Parameters
+
PATH   'Path name ' C D=' '
NLEVEL 'number of levels' I D=99
OUTPUT 'Output filename' C D='TTY'
*----------------------------------------------------------------
>Comand UNLOCK
>Action  FMULCC
>PARAMETERS
GENAM 'generic name' C D=' '
+
KSN 'Key serial number' I D=0
CHOPT 'Options' C D=' '
>GUIDANCE
Use the UNLOCK comand to enable WRITE access to the tape on which
the file specified resides. If a negative key serial number is given,
all entries for the specified generic name will be unlocked.
Syntax:  UNLOCK GENERIC-NAME KEY-SERIAL-NUMBER <CHOPT>
*----------------------------------------------------------------
>Comand UPDATE
>Guidence
Use the UPDATE comand to control how updates are sent to the FATMEN
server.
Syntax:  UPDATE MAX NGROUP IFLAG
>Action  FMUPDC
>PARAMETERS
MAX    'maximum number of updates' I D=999
NGROUP 'number of updates to send together' I D=0
IFLAG  'flag to purge/send/reset'  I D=0
*----------------------------------------------------------------
>Comand VERSION
>Guidence
Use the VERSION comand to display the version of the FATMEN
software that you are running.
Syntax:  VERSION
>Action  FMVERS
*----------------------------------------------------------------
>Comand VIEW
>Guidence
Use the VIEW comand to edit a local disk file.
This comand will be extended to support tape and remote files.
Syntax:  VIEW file <key>
>Action  FMEDIT
>PARAMETERS
FILE 'file' C D=' '
+
KSN 'Key serial number' I D=0
*----------------------------------------------------------------
>Comand ZOOM
>Action  FMZOOM
>PARAMETERS
+
FILE 'Pathname' C D='*'
>Guidence
Use the ZOOM command to descend the specified (wild-carded)
directory tree to the first lowest level directory that
contains one of more files
 
e.g. ZOOM DATA
     ZOOM *DST/*/P88*
 
If no wild-cards are present in the pathname, the characters /*
are automatically appended. To zoom down a tree starting with
 
*ALLD, use ZOOM *ALLD/*
 
*----------------------------------------------------------------
>Menu ADD
*----------------------------------------------------------------
>Comand TAPE
>Guidence
Use the ADD/TAPE comand to add a new tape file to the FATMEN catalogue
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
MEDIA 'media type (2=3480,3=3420,4=8200,...)' I D=2
*----------------------------------------------------------------
>Comand DISK
>Guidence
Use the ADD/DISK comand to add a new disk file to the FATMEN catalogue
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
*----------------------------------------------------------------
>Menu \SET
*----------------------------------------------------------------
>Comand Location
>Guidance
Use the SET/LOCATION comand to define the list of location codes
to be used to select datasets. Only datasets with a location code
in the specified range will be visible via LS comands, or accessible
through FIND or MAKE comands.
e.g.
set/loc 1,3,5-17 # set location codes
set/loc -1 # Clear location codes
>Action FMKLOC
>Parameters
RANGE 'range of location codes' C D='-1'
*----------------------------------------------------------------
>Comand COPYLEVEL
>Guidance
Use the SET/COPYLEVEL comand to define the list of copy levels
to be used to select datasets. Only datasets with a copy levels
in the specified range will be visible via LS comands, or accessible
through FIND or MAKE comands.
e.g.
set/loc 1,3,5-17 # set copy levels
set/loc -1 # Clear copy levels
>Action FMKCPL
>Parameters
RANGE 'range of copy levels' C D='-1'
*----------------------------------------------------------------
>Comand MEDIATYPE
>Guidance
Use the SET/MEDIATYPE comand to define the list of media types
to be used to select datasets. Only datasets with a media type
in the specified range will be visible via LS comands, or accessible
through FIND or MAKE comands.
e.g.
set/loc 1,3,5-17 # set  media types
set/loc -1 # Clear media types
>Action FMKMTP
>Parameters
RANGE 'range of media types' C D='-1'
*----------------------------------------------------------------
>Comand USERWORDS
>Guidance
Use the SET/USERWORDS comand to define the ranges for the user
words that will be used in subsequent SEARCH commands.
e.g.
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
>Menu \SHOW
*----------------------------------------------------------------
>Comand Location
>Guidance
Use the SHOW/LOCATION comand to show the list of location codes
that are currently in effect.  Only datasets with a location code
in the displayed range will be visible via LS comands, or accessible
through FIND or MAKE comands.
>Action FMSLOC
*----------------------------------------------------------------
>Comand COPYLEVEL
>Guidance
Use the SHOW/COPYLEVEL comand to show the list of copy levels
that are currently in effect.  Only datasets with a copy level
in the displayed range will be visible via LS comands, or accessible
through FIND or MAKE comands.
>Action FMSCPL
*----------------------------------------------------------------
>Comand MEDIATYPE
>Guidance
Use the SHOW/MEDIATYPE comand to show the list of media types
that are currently in effect.  Only datasets with a media type
in the displayed range will be visible via LS comands, or accessible
through FIND or MAKE comands.
>Action FMSMTP
*----------------------------------------------------------------
>Comand USERWORDS
>Guidance
Use the SHOW/USERWORDS comand to show the list of ranges for
the 10 user words that are currently in effect.
Only entries which match the current user word ranges will
be found be the SEARCH command.
>Action FMSUWD
* Sequence for FATMEN user comands
#include "fatmen/fmucdf.inc"
* Ignoring t=pass
