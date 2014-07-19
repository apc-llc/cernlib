*
* $Id: zftpcdf.cdf,v 1.1.1.1 1996/03/08 15:44:20 mclareni Exp $
*
* $Log: zftpcdf.cdf,v $
* Revision 1.1.1.1  1996/03/08 15:44:20  mclareni
* Cspack
*
*
>NAME FTPDEF
 
*----------------------------------------------------------------
>Menu ZFTP
>Guidance
To transfer Text files, RZ or FZ files using TCP/IP.
 
*----------------------------------------------------------------
>Command LOGLEVEL
>Parameters
Level 'Log level ' I D='0'
>Guidance
Use the LOGLEVEL command to set the level of logging/debug of
the ZFTP command.
>Action ZFLOGC
 
*----------------------------------------------------------------
>Command MV
>Parameters
SOURCE 'Source file' C D=' '
TARGET 'Target file' C D=' '
+
CHOPT  'Options    ' C D=' '
>Action ZFTPMV
>Guidance
Move (rename) the file SOURCE to TARGET on the remote machine
*----------------------------------------------------------------
>Command LMV
>Parameters
SOURCE 'Source file' C D=' '
TARGET 'Target file' C D=' '
+
CHOPT  'Options    ' C D=' '
>Action ZFTLMV
>Guidance
Move (rename) the file SOURCE to TARGET on the local machine
*----------------------------------------------------------------
>Command GETA
>Parameters
REMOTE 'Remote file name' C D='REM1'
+
LOCAL  'Local file name' C D=' '
CHOPT 'CHOPT' C D=' '
-A 'local file has already been opened'
-C 'respect case of file name (otherwise translate to lower case on Unix)'
-S 'statistics on the file transfer are printed'
-V 'remote file is created with V format (IBM)'
>Guidance
Transfer a text file REMOTE from the remote machine
to a local file called LOCAL.  If a local file name is
not specified, the file will have the same name as on
the remote machine.
.
Note that on IBM, the text file is created RECFM=F,LRECL=80
unless the option V is specified.
.
e.g.
.
GETA FATMEN.CARDS -s # Transfer FATMEN card pam and print statistics
>Action GETA
 
*----------------------------------------------------------------
>Command PUTA
>Parameters
LOCAL  'Local file name' C D='REM1'
+
REMOTE 'Remote file name' C D=' '
CHOPT 'CHOPT' C D=' '
-A 'local file has already been opened'
-C 'respect case of file name (otherwise translate to lower case on Unix)'
-S 'statistics on the file transfer are printed'
-V 'remote file is created with V format (IBM)'
>Guidance
Transfer the text file LOCAL to the remote machine
If a remote file name is not given, the file will have
the same name as on the local machine.
.
Note that on VM systems the text file is created RECFM=F,LRECL=80
unless the option V is specified.
.
e.g.
.
PUTA CSPACK.CARDS CSPACK/CSPACK.CAR # Store file in directory CSPACK
>Action PUTA
 
*----------------------------------------------------------------
>Command GETB
>Parameters
REMOTE 'Remote file name' C D='REM1'
+
LOCAL  'Local file name' C D=' '
LRECL 'Record length in bytes' I D=0
CHOPT 'CHOPT' C D=' '
-A 'local file has already been opened'
-C 'respect case of file name (otherwise translate to lower case on Unix)'
-R 'replace local file if it already exists'
-S 'statistics on the file transfer are printed'
>Guidance
Transfer a binary file REMOTE from the remote machine
to a local file called LOCAL. If a local file name is
not specified, the file will have the same name as on
the remote machine. The file must contain
fixed length blocks (EPIO or FZ exchange format).
FORTRAN sequential I/O is used on both the local
and remote node.
.
See also the GETX command.
.
e.g.
.
GETB FXFILE.DAT = 32400 -s
>Action GETB
 
*----------------------------------------------------------------
>Command PUTB
>Parameters
LOCAL  'Local file name' C D='LOC1'
+
REMOTE 'Remote file name' C D=' '
LRECL 'Record length in bytes' I D=0
CHOPT 'CHOPT' C D=' '
-A 'local file has already been opened'
-C 'respect case of file name (otherwise translate to lower case on Unix)'
-R 'replace remote file if it already exists'
-S 'statistics on the file transfer are printed'
>Guidance
Transfer a binary file LOCAL to the remote machine
If a remote file name is not given, the file will have
the same name as on the local machine.
The file must contain fixed length blocks (EPIO
or FZ exchange format).
FORTRAN sequential I/O is used on both the local
and remote node.
.
See also the GETX command.
.
e.g.
.
PUTB EPIO.DAT
>Action PUTB
 
*----------------------------------------------------------------
>Command GETD
>Parameters
REMOTE 'Remote file name' C D='REM1'
+
LOCAL  'Local file name' C D=' '
LRECL  'Record length (in bytes)' I D=0
CHOPT 'CHOPT' C D=' '
-A 'local file has already been opened'
-C 'respect case of file name (otherwise translate to lower case on Unix)'
-R 'replace local file if it already exists'
-S 'statistics on the file transfer are printed'
>Guidance
Transfer a direct-access file REMOTE from the remote machine
to a local file called LOCAL.
If LOCAL is not given, REMOTE is assumed.
>Action GETD
 
*----------------------------------------------------------------
>Command PUTD
>Parameters
LOCAL  'Local file name' C D='REM1'
+
REMOTE 'Remote file name' C D=' '
LRECL  'Record length (in bytes)' I D=0
CHOPT 'CHOPT' C D=' '
-A 'local file has already been opened'
-C 'respect case of file name (otherwise translate to lower case on Unix)'
-R 'replace remote file if it already exists'
-S 'statistics on the file transfer are printed'
>Guidance
Transfer a direct-access file LOCAL to the remote machine
The name of the text file on the remote machine is REMOTE.
If REMOTE is not given, LOCAL is assumed.
>Action PUTD
 
*----------------------------------------------------------------
>Command GETP
>Parameters
REMOTE 'Remote PAM name' C D='REM1'
+
LOCAL  'Local PAM name' C D=' '
CHOPT 'CHOPT' C D=' '
-C 'respect case of file name (otherwise translate to lower case on Unix)'
-R 'replace local file if it already exists'
-S 'statistics on the file transfer are printed'
>Guidance
Transfer a binary PAM file REMOTE from the remote machine
to a local file called LOCAL.
If LOCAL is not given, REMOTE is assumed.
>Action GETP
 
*----------------------------------------------------------------
>Command PUTP
>Parameters
LOCAL  'Local PAM name' C D='LOC1'
+
REMOTE 'Remote PAM name' C D=' '
CHOPT 'CHOPT' C D=' '
-C 'respect case of file name (otherwise translate to lower case on Unix)'
-R 'replace remote file if it already exists'
-S 'statistics on the file transfer are printed'
>Guidance
Transfer a binary PAM file LOCAL to the remote machine
>Action PUTP
 
*----------------------------------------------------------------
>Command GETFZ
>Parameters
REMOTE 'Remote file name' C D='REM1'
+
LOCAL  'Local file name' C D=' '
RRECL 'Record length (in bytes)' I D=0
RFORM 'Remote Format of remote FZ file' C D=' ' OPTION
-A 'ASCII exchange format file'
-D 'Use Fortran direct access I/O'
-X 'Binary exchange format file'
-Z 'Native format file (default)'
LRECL 'Local Record length (in bytes)' I D=0
LFORM 'Format of local FZ file' C D=' ' OPTION
-A 'ASCII exchange format file'
-D 'Use Fortran direct access I/O'
-X 'Binary exchange format file'
-Z 'Native format file (default)'
CHOPT 'CHOPT' C D=' '
-A 'local file has already been opened'
-C 'respect case of file name (otherwise translate to lower case on Unix)'
-S 'statistics on the file transfer are printed'
>Guidance
GETFZ allows a FZ file to be retrieved from a remote machine.
Transfer of exchange format files is achieved more efficiently
using GETB (for binary exchange format) or GETA (alpha format).
.
GETFZ permits format conversion. For native format files, the
record length must be specified (in bytes). For exchange format
files, the record length defaults to 80 for alpha-format files
or is determined from the file itself for binary exchange format
files.
>Action GETFZ
 
*----------------------------------------------------------------
>Command PUTFZ
>Parameters
LOCAL  'Local file name' C D='REM1'
+
REMOTE 'Remote file name' C D=' '
LRECL 'Local Record length (in bytes)' I D=0
LFORM 'Format of local FZ file' C D=' ' OPTION
-A 'ASCII exchange format file'
-D 'Use Fortran direct access I/O'
-X 'Binary exchange format file'
-Z 'Native format file (default)'
RRECL 'Remote Record length (in bytes)' I D=0
RFORM 'Format of remote FZ file' C D=' ' OPTION
CHOPT 'CHOPT' C D=' '
-A 'local file has already been opened'
-C 'respect case of file name (otherwise translate to lower case on Unix)'
-S 'statistics on the file transfer are printed'
>Guidance
PUTFZ allows a FZ file to be sent to a remote machine.
Transfer of exchange format files is achieved more efficiently
using PUTB (for binary exchange format) or PUTA (alpha format).
.
PUTFZ permits format conversion. For native format files, the
record length must be specified (in bytes). For exchange format
files, the record length defaults to 80 for alpha-format files
or is determined from the file itself for binary exchange format
files.
>Action PUTFZ
 
*----------------------------------------------------------------
>Command GETRZ
>Parameters
REMOTE 'Remote file name' C D='REM1'
+
LOCAL  'Local file name' C D=' '
CHOPT 'CHOPT' C D=' ' R=' ,R,S,L,T,X,N'
-A 'local file has already been opened'
-C 'respect case of file name (otherwise translate to lower case on Unix)'
-E 'the input file is in EXCHANGE mode'
-S 'statistics on the file transfer are printed'
-N 'the local file will be in NATIVE format'
-X 'the local file will be in EXCHANGE format'
-R 'the local file will have RELATIVE organisation (VAX)'
-S 'RZSTAT is called on the received file.'
-L 'RZLDIR is called.'
-T 'RZLDIR is called with option "T".'
>Guidance
Transfer a RZ file REMOTE from the remote machine
to a local RZ file called LOCAL.
If LOCAL is not given, REMOTE is assumed.
The RZ file is created on the local computer with the same
parameters as on the remote machine.
>Action GETRZ
 
*----------------------------------------------------------------
>Command PUTRZ
>Parameters
LOCAL  'Local file name' C D='REM1'
+
REMOTE 'Remote file name' C D=' '
CHOPT 'CHOPT' C D=' ' R=' ,R,S,L,T,N,X'
-A 'local file has already been opened'
-C 'respect case of file name (otherwise translate to lower case on Unix)'
-E 'the input file is in EXCHANGE mode'
-S 'statistics on the file transfer are printed'
-N 'the remote file will be in NATIVE format'
-X 'the remote file will be in EXCHANGE format'
-R 'the remote file will have RELATIVE organisation (VAX)'
-S 'RZSTAT is called on the received file.'
-L 'RZLDIR is called.'
-T 'RZLDIR is called with option "T".'
>Guidance
Transfer the local RZ file LOCAL to the remote machine
The name of the RZ file on the remote machine is REMOTE.
.
If REMOTE is not given, LOCAL is assumed.
.
The RZ file is created on the remote computer with the same
parameters as on the local machine.
>Action PUTRZ
 
*----------------------------------------------------------------
>Command GETX
>Parameters
REMOTE 'Remote file name' C D='REM1'
+
LOCAL  'Local file name' C D=' '
LRECL 'Record length in bytes' I D=0
CHOPT 'CHOPT' C D=' '
-A 'local file has already been opened'
-C 'respect case of file name (otherwise translate to lower case on Unix)'
-S 'statistics on the file transfer are printed'
>Guidance
Transfer a binary file REMOTE from the remote machine
to a local file called LOCAL. If a local file name is
not specified, the file will have the same name as on
the remote machine. The file must contain
fixed length blocks (EPIO or FZ exchange format)
with no FORTRAN control words. This command
can be used to transfer 'true' exchange format
files, e.g. those written on Unix systems with
C or FORTRAN direct-acess I/O. See also the GETB command.
If LOCAL is not given, REMOTE is assumed.
e.g.
GETX FXFILE.DAT = 32400 -s
>Action GETX
 
*----------------------------------------------------------------
>Command PUTX
>Parameters
LOCAL  'Local file name' C D='LOC1'
+
REMOTE 'Remote file name' C D=' '
LRECL 'Record length in bytes' I D=0
CHOPT 'CHOPT' C D=' '
-A 'local file has already been opened'
-C 'respect case of file name (otherwise translate to lower case on Unix)'
-S 'statistics on the file transfer are printed'
>Guidance
Transfer a binary file LOCAL to the remote machine
If a remote file name is not given, the file will have
the same name as on the local machine.
The file must contain fixed length blocks (EPIO
or FZ exchange format). This command
can be used to transfer 'true' exchange format
files, e.g. those written on Unix systems with
C or FORTRAN direct-acess I/O. See also the GETB command.
e.g.
PUTX EPIO.DAT
>Action PUTX
 
*----------------------------------------------------------------
>Command MGET
>Parameters
REMOTE 'Remote file name' C D='REM1'
+
LOCAL  'Local file name' C D=' '
CHOPT 'CHOPT' C D=' '
>Guidance
Transfer all files matching the specified remote file name
to the local system. The file name given may contain
*, to match one or more characters, or %, to match a single
character.
.
By default the transfer is performed
using GETA, unless the file name has a known extension.
.
e.g.
.
*.PAM         --> GETP
*.CETA, *.CET --> GETB, LRECL=3600
*.CMZ,  *.RZ  --> GETRZ
.
Options are passed to the appropriate transfer routine.
>Action MGET
 
*----------------------------------------------------------------
>Command MPUT
>Parameters
LOCAL  'Local file name' C D='LOC1'
+
REMOTE 'Remote file name' C D=' '
CHOPT 'CHOPT' C D=' '
>Guidance
Transfer all files matching the specified local file name
to the remote system. The file name given may contain
*, to match one or more characters, or %, to match a single
character.
By default the transfer is performed
using PUTA, unless the file name has a known extension.
.
e.g.
.
*.PAM         --> GETP
*.CETA, *.CET --> GETB, LRECL=3600
*.CMZ,  *.RZ  --> GETRZ
.
e.g.
.
MPUT *.CAR *.CARDS -s # Transfer all local .CAR files to .CARDS files
                      # and print statistics on each transfer
.
Options are passed to the appropriate transfer routine.
>Action MPUT
 
*----------------------------------------------------------------
>Command RSHELL
>Parameters
COMMAND  'Command to remote machine' C
>Guidance
The text in COMMAND is transmitted for execution to the
remote machine.
e.g.
RSHELL EXEC TELL ONEALE Coffee?
>Action RSHELL
 
*----------------------------------------------------------------
>Command OPEN
>Parameters
MACHINE  'Name of remote machine' C
+
CHOPT    'Character options     ' C D=' '
-V 'remote node runs VM/CMS - use REXEC to start server'
-D 'use DECnet rather than TCP/IP (VAX/VMS systems only)'
>Guidance
Opens a communication with the remote machine named MACHINE.
The name specified must be defined in the local TCP/IP hosts
table or in the names server.
.
The machine name may be given as a character string,
e.g. VXCRNA, or as an internet address, e.g. 128.141.1.100
This command will prompt you for user authentification.
Give a user name and a password(not echoed).
.
e.g.
.
OPEN VXCRNA
>Action RLOGIN
 
*----------------------------------------------------------------
>Command CLOSE
>Guidance
Close communication with the current remote host.
>Action RCLOSE
 
*----------------------------------------------------------------
>Command CD
>Guidance
Change remote working directory.
.
e.g.
.
CD DISK$CERN:[JAMIE.ZFTP] # VMS
CD JAMIE.202              # VM
CD /usera/jamie/tcpaw     # Unix
.
To obtain a write link to a disk belonging to another user
on VM/CMS, use, for example:
.
CD JAMIE.202 write-pass -W
.
>Action ZFTPCD
>Parameters
+
Pathname 'Pathname' C D=' '
Password 'Password' C D=' '
CHOPT    'Options ' C D=' '
-C 'Respect case of remote directory'
-R 'Obtain read-link to specified mini-disk (VM only)'
-W 'Obtain write-link to specified mini-disk (VM only)'
 
*----------------------------------------------------------------
>Command LCD
>Guidance
Change local working directory. If no parameters are given,
the current working directory is displayed.
e.g.
LCD DISK$CERN:[JAMIE.ZFTP] # VMS
LCD JAMIE.202              # VM
LCD /usera/jamie/tcpaw     # Unix
>Action ZFTLCD
>Parameters
+
Pathname 'Pathname' C D=' '
Password 'Password' C D=' '
CHOPT    'Options ' C D=' '
-C 'Respect case of remote directory'
-R 'Obtain read-link to specified mini-disk (VM only)'
-W 'Obtain write-link to specified mini-disk (VM only)'
 
*----------------------------------------------------------------
>Command PWD
>Guidance
Print remote working directory.
>Action ZFTPWD
 
*----------------------------------------------------------------
>Command LPWD
>Guidance
Print local working directory.
>Action ZFTLCD
>Parameters
+
Pathname 'Pathname' C D=' '
Password 'Password' C D=' '
CHOPT    'Options ' C D=' '
 
*----------------------------------------------------------------
>Command LS
>Guidance
Use the LS command to list a remote directory.
e.g.
LS *.CMZ
>Action ZFTPLS
>Parameters
+
Pathname 'Pathname' C D=' '
CHOPT    'Options'  C D=' '
-L 'Generate "long" listing. This corresponds to the L_
option of LISTFILE on VM, the -l flag of ls on Unix_
and to the output of the ftp dir subcommand on VMS'
 
*----------------------------------------------------------------
>Command LLS
>Guidance
Use the LLS command to list a local directory.
e.g.
LLS *.CMZ
>Action ZFTLLS
>Parameters
+
Pathname 'Pathname' C D=' '
CHOPT    'Options'  C D=' '
-L 'Generate "long" listing. This corresponds to the L_
option of LISTFILE on VM, the -l flag of ls on Unix_
and to the output of the ftp dir subcommand on VMS'
 
*----------------------------------------------------------------
>Command RM
>Guidance
Use the RM command to delete a remote file.
e.g.
RM TEST.DAT
>Action ZFTPRM
>Parameters
Filename 'Filename' C D=' '
 
*----------------------------------------------------------------
>Command LRM
>Guidance
Use the LRM command to delete a local file.
e.g.
LRM TEST.DAT
>Action ZFTLRM
>Parameters
Filename 'Filename' C D=' '
 
*----------------------------------------------------------------
>Command LOCSYS
>Guidance
Display local operating system
>Action XZLSYS
 
*----------------------------------------------------------------
>Command SYSTEM
>Guidance
Display remote operating system
>Action XZRSYS
 
*----------------------------------------------------------------
>Command SVERSION
>Guidance
Print version of server program
>Action XZSVER
 
*----------------------------------------------------------------
>Command VERSION
>Guidance
Print version of ZFTP program
>Action ZFVERS
 
*----------------------------------------------------------------
>Command RFRF
>Parameters
FZFILE 'FZ file name' C D=' '
RZFILE 'RZ file name' C D=' '
+
LRECL  'RZ file record length' I D=0
QUOTA  'Quota for output file' I D=0
CHOPT  'CHOPT' C D=' '
-A 'the input file is in FZ alpha format'
-S 'display statistics on the RZ file'
-X 'the RZ file will be created in eXchange mode'
-C 'respect case of input/output file names'
-R 'replace output file if it already exists'
>Guidance
This command converts an FZ exchange format file to an
RZ file on the LOCAL machine. No network transfer is
performed. The FZFILE must be the output of a previous
RTOF command, or have been created using the RTOX or RTOA
programs. On Unix systems, this file will be read with
FORTRAN direct-access and will hence be transferable and
readable on other systems.
.
By default, the output RZ file will have the same record
length as the original RZ file. However, if LRECL is
specified then this value will be used instead.
>Action ZFRFRF
 
*----------------------------------------------------------------
>Command RTOF
>Parameters
RZFILE 'RZ file name' C D=' '
FZFILE 'FZ file name' C D=' '
+
LRECL  'FZ file record length' I D=3600
CHOPT 'CHOPT' C D=' '
-A 'the output file will be in FZ alpha format'
-S 'display statistics on the RZ file'
-C 'respect case of input/output file names'
-R 'replace output file if it already exists'
-X 'input file is in exchange format'
>Guidance
This command converts an RZ file into an FZ exchange mode
format file on the LOCAL machine. No network transfer is
performed. By default a binary exchange mode FZ file is
created. On Unix systems, this file will be written with
FORTRAN direct-access and will hence be transferable and
readable on other systems.
>Action ZFRTOF
 
*----------------------------------------------------------------
>Command FZCOPY
>Parameters
FZIN   'input  FZ file name' C D=' '
FZOUT  'output FZ file name' C D=' '
+
IFORM 'Format of input FZ file' C D='X' OPTION
-A 'ASCII exchange format file'
-D 'Use Fortran direct access I/O'
-N 'Native data format but exchange file format'
-X 'Binary exchange format file'
-Z 'Native format file (default)'
IRECL 'Input record length (in bytes)' I D=0
OFORM 'Format of output FZ file' C D='X' OPTION
-A 'ASCII exchange format file'
-D 'Use Fortran direct access I/O'
-N 'Native data format but exchange file format'
-X 'Binary exchange format file'
-Z 'Native format file (default)'
ORECL 'Output record length (in bytes)' I D=0
CHOPT 'CHOPT' C D=' '
>Guidance
This command copies an FZ file on the local machine. At
the same time, file format and data format conversion is
possible. Thus, FZCOPY can be used to convert a binary
native format file into a alpha exchange format file etc.
.
The format of the files are specified as follows:
.
    A - alpha exchange mode format           - RECL not needed
    N - native data but exchange file format - RECL not needed
    X - exchange format file                 - RECL not needed
    Z - native data and file format          - RECL must be specified
.
>Action ZFFZCP
 
*----------------------------------------------------------------
>Command RZCOPY
>Parameters
RZIN   'input  RZ file name' C D=' '
RZOUT  'output RZ file name' C D=' '
+
ORECL 'Output record length (in words)' I D=0
OQUOT 'Quota for the output file (D=take from input)' I D=0
CHOPT 'CHOPT' C D='X'
-N 'convert exchange RZ file into native RZ file'
-X 'convert native RZ file into exchange RZ file'
>Guidance
This command copies an RZ file on the local machine. At
the same time, the record length or data format may be
changed. Thus, RZCOPY can be used to convert a native
format RZ file with record length 512 into an exchange
format file with record length 8192.
.
If not specified, the output record length will be set equal
to that of the input file.
.
If not specified, the output quota will be taken from that
of the input file.
>Action ZFRZCP
 
*----------------------------------------------------------------
>Command CTOF
>Parameters
CFILE  'input  file name' C D=' '
FFILE  'output file name' C D=' '
+
LRECL 'record length (in bytes)' I D=0
CHOPT 'CHOPT' C D=' '
-X 'Zebra exchange format file - RECL not needed'
>Guidance
This command copies a file written with C or FORTRAN
direct-access I/O to one written with FORTRAN sequential I/O.
>Action ZFCTOF
 
*----------------------------------------------------------------
>Command FTOC
>Parameters
FFILE  'input  file name' C D=' '
CFILE  'output file name' C D=' '
+
LRECL 'record length (in bytes)' I D=0
CHOPT 'CHOPT' C D=' '
-X 'Zebra exchange format file - RECL not needed'
>Guidance
This command copies a file written with FORTRAN sequential
I/O to one written with FORTRAN direct-access I/O. The
output file may be read with C I/O or FORTRAN direct access.
>Action ZFFTOC
