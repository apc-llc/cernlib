*
* $Id: kuipcdf.cdf,v 1.5 1997/09/25 14:02:25 cremel Exp $
*
* $Log: kuipcdf.cdf,v $
* Revision 1.5  1997/09/25 14:02:25  cremel
* VECTOR WRITE (kxwriv.F): increase limitation of number of vectors in the
* list VLIST from 10 to 30 (same value as in kxreav.F for VECTOR/READ).
* Update HELP for VECTOR/WRITE and VECTOR/READ and mention explicitely this
* limitation. Improve error message in kxwriv.F and kxreav.F by
* adding explicitely VECTOR/WRITE and VECTOR/READ.
* + Increase KUIP version number to 2.07/16  (25/09/97).
*
* Revision 1.4  1997/06/18 11:52:58  couet
* - default format for V/WRITE changed
*
* Revision 1.3  1996/10/08 15:08:14  cremel
* Modify guidance for command /KUIP/FMESSAGE
*
* Revision 1.2  1996/10/08 13:20:01  cremel
* Add new KUIP command /KUIP/FMESSAGE to write a message string into a file.
* New corresponding action routine is kxfmessage (C code).
*
* Revision 1.1.1.1  1996/03/08 15:32:59  mclareni
* Kuip
*
*
*CMZ :  2.07/04 12/05/95  17.08.10  by  N.Cremel
*-- Author :
>Name KUIDF1

>Menu KUIP
>Guidance
Command Processor commands.

>Command HELP
>Parameters
+
ITEM   'Command or menu name or keyword(s)' C D=' '
OPTION 'Option' C D='N' R='EDIT,NOEDIT,KEYWORD'
-EDIT    The help text is written to a file and the editor is invoked,
-E       Same as 'EDIT'.
-NOEDIT  The help text is output on the terminal output.
-N       Same as 'NOEDIT'
-KEYWORD give access to all commands associated to that keyword(s).
-K       Same as 'KEYWORD'

>Keyword
FIND keyword search
>Guidance
Find help information by command name, menu name or keywords.
.
If ITEM is a valid command name (and there is only one such command)
then full explanation on that command is provided: syntax (as given by
the command USAGE), functionality, list of parameters with their attributes.
If ITEM also corresponds to other commands associated to it with a "keyword"
then a "See also" message, followed by the names of these commands
is given.
.
If ITEM is a menu (or a submenu) a dialogue is guiding the user in traversing
the tree command structure for getting full explanation on a specific
command from that tree.
.
If HELP is entered without parameters, the search start from the top level
menu and the user is guided in traversing the complete tree command structure.
.
'HELP -KEYWORD' (or 'HELP -K') followed by one or more keywords causes HELP
to give access to all commands associated to that (list of) keyword(s).
If the keyword corresponds to a valid command or (sub)menu name all
corresponding commands are accessible. This option is especially useful
when you do not know the exact name of a valid command or menu and you can
only describe it by its functionality (e.g. 'HELP -KEYWORD POSTSCRIPT').
.
N.B. If ITEM does not correspond to any valid command or menu name then
the option `-KEYWORD' is automatically invoked.
.
'HELP -EDIT' (or just 'CHELP -E') switches to edit mode: instead of writing
the help text to the terminal output, it is written into a temporary file
and the pager or editor defined by the command HOST_PAGER is invoked.
(On Unix workstations the pager can be defined to display the help text
asynchrously in a separated window.)
'CHELP -NOEDIT' (or just 'CHELP -N') switches back to standard mode.
The startup value is system dependent.

>Action kxfind%C

>Command CHELP
>Parameters
+
ITEM   'Command or menu path' C D=' '
OPTION 'View mode' C D='N'
-EDIT    The help text is written to a file and the editor is invoked,
-E       Same as 'EDIT'.
-NOEDIT  The help text is output on the terminal output.
-N       Same as 'NOEDIT'

>Guidance
Find help information only on valid command name or menu path.
.
A more general help facility, associated to keywords, is given by
the command HELP.
.
If ITEM is a command its full explanation is given:
syntax (as given by the command USAGE), functionality,
list of parameters with their attributes (prompt, type, default, range, etc.).
If ITEM='/' the help for all commands is given.
.
If CHELP is entered without parameters or ITEM is a submenu,
the dialogue style is switched
to 'AN', guiding the user in traversing the tree command structure.
.
'CHELP -EDIT' (or just 'CHELP -E') switches to edit mode:
instead of writing the help text to the terminal output,
it is written into a temporary file and the pager or editor defined
by the command HOST_PAGER is invoked.
(On Unix workstations the pager can be defined to display the help text
asynchrously in a separated window.)
'CHELP -NOEDIT' (or just 'CHELP -N') switches back to standard mode.
The startup value is system dependent.
>Action kxhelp%C

>Command USAGE
>Parameters
ITEM 'Command name' C
>Keyword
help syntax command
>Guidance
Give the syntax of a command.
If ITEM='/' the syntax of all commands is given.
>Action kxusage%C

>Command MANUAL
>Parameters
ITEM 'Command or menu path' C
+
OUTPUT 'Output file name' C D=' '
OPTION 'Text formatting system' C D=' '
-      plain text : plain text format
-LATEX LaTeX format (encapsulated)
-TEX   LaTeX format (without header)
>Keyword
LATEX TEX format
HELP command
>Guidance
Write on a file the text formatted help of a command.
If ITEM is a menu path the help for all commands linked to that menu
is written.
If ITEM='/' the help for the complete command tree is written.
If OUTPUT=' ' the text is written to the terminal.
.
The output file produced with option LATEX can be processed directly by LaTeX,
i.e. it contains a standard header defining the meta commands used for
formatting the document body.
With option TEX only the document body is written into the output file
which can be included by a driver file containing customized definitions
of the standard meta commands.
Example:
 MANUAL / MAN.TEX LATEX
will produce the file MAN.TEX containg the documentation of all
available commands in LaTeX format.
>Action kxmanual%C

>Command EDIT
>Parameters
FNAME 'File name' C
>Keyword
EDITOR file
>Guidance
Invoke the editor on the file.
The command HOST_EDITOR can be used to define the editor.
.
If FNAME does not contain an extension the default filetype '.KUMAC'
is supplied.
The search path defined by the command DEFAULTS is used to find an
already existing file.
If the file does not exist it is created with the given name.
>Action kxedit%C

>Command PRINT
>Parameters
FNAME 'File name' C
>Keyword
printer
>Guidance
Send a file to the printer.
The command HOST_PRINT can be used to define the host command for
printing the file depending on it file extension.
>Action kxprint%C

>Command PSVIEW
>Parameters
FNAME 'File name' C
>Keyword
PostScript
display viewer
>Guidance
Invoke the PostScript viewer on the file.
The command HOST_PSVIEWER can be used to define the PostScript viewer.
.
If FNAME does not contain an extension the default filetype '.PS'
is supplied.
>Action kxpsview%C

>Command LAST
>Parameters
+
N     'N last commands to be saved' I D=-99 R=-99:
FNAME 'File name' C D=' '
>Keyword
history file RECORDING RECORD
>Guidance
Perform various operations with the history file.
.
If FNAME is not specified, the current history file is assumed by default
(the startup history file name is LAST.KUMAC).
To change the history file the command LAST 0 NEW-FNAME must be entered.
.
If N.EQ.-99 (default case) the default host editor is called
to edit the current history file, containing all the commands of the session.
.
If N.LT.0 the last -N commands are printed on the screen.
On MVS this allows to edit and resubmit commands.
On workstations this allows to resubmit blocks of commands by mouse-driven
cut-and-paste operations.
.
If N.EQ.0 the history file FNAME is rewound and set as the current one
(the command LAST 0 FNAME itself is not recorded).
.
If N.GT.0 the last N commands of the session are saved in the
current history file.
.
See also the command RECORDING.
>Action kxlast%C

>Command MESSAGE
>Parameters
+
STRING 'Message string' C D=' ' Separate
>Keyword
string macro
>Guidance
Write a message string on the terminal.
A useful command inside a macro.
Several message strings can be given in the same command line,
each of them separated by one or more spaces
(the usual parameter separator);
therefore multiple blanks will be dropped
and only one will be kept.
If multiple blanks should not be dropped,
the string must be surrounded by single quotes.
>Action kxmessage%C

>Command FMESSAGE
>Parameters
+
STRING 'Message string' C D=' ' Separate
OUTPUT 'Output file name' C D=' '
>Keyword
string macro file put puts
>Guidance
Write a message string on OUTPUT file name.
OUTPUT file is opened in "append" mode (open for writing at end of
file, or create for writing).
If OUTPUT=' ' the text is written to the terminal.
If 'Message string' contains several words separated by one or more 
spaces the string must be surrounded by single quotes.
>Action kxfmessage%C

>Command SHELL
>Parameters
+
CMD 'Shell command string' C D=' '
>Keyword
SYSTEM
>Guidance
Execute a command of the host operating system.
The command string is passed to the command processor defined by HOST_SHELL.
If CMD=' ' the shell is spawned as interactive subprocess.
To return from the shell enter 'RETURN' (the full word, not just <CR>)
or 'exit' (depending on the operation system).
>Action kxshell%C

>Command WAIT
>Parameters
+
STRING 'Message string'    C D=' '
SEC    'Number of seconds' R D=0 R=0:
>Keyword
pause time timing
>Guidance
Make a pause (e.g. inside a macro).
Wait a given number of seconds (if SEC.GT.0)
or just until <CR> is entered (if SEC.EQ.0).
A message string is also written on the terminal before waiting.
>Action kxwait%C

>Command IDLE
>Parameters
SEC    'Number of seconds' I R=0:
+
STRING 'Command string'    C D=' '
>Keyword
keyboard time timing
>Guidance
Execute a command if program is idle.
The command string is executed if there was no keyboard activity
during SEC seconds.
>Action kxidle%C

>Command UNITS
>Keyword
file
>Guidance
List all Input/Output logical units currently open.
The files attached to them are also shown.
>Action KXUNIT

>Command EXIT
>Guidance
End of the interactive session.
>Action kxexit%C

>Command QUIT
>Guidance
End of the interactive session.
>Action kxexit%C

>Help_item FUNCTIONS
>User_help kxfunclist%C
>Keyword
SYSTEM DATE TIME MACHINE SHELL
VDIM VLEN dimension length
VECTOR
STRING EXIST
UPPER LOWER INDEX FORMAT
SIGMA
>Guidance
List of all KUIP System Functions.
.
       *** KUIP System Functions ***
.
The function name (and arguments) is literally replaced,
at run-time, by its current value.
At present, the following functions are available:
 $DATE  .......................  Current date in format DD/MM/YY
 $TIME  .......................  Current time in format HH.MM.SS
 $CPTIME  .....................  CP time elapsed since last call (in sec)
 $RTIME  ......................  Real time elapsed since last call (in sec)
 $VDIM(VNAME,IDIM)  ...........  Physical length of vector VNAME
                                 on dimension IDIM (1..3)
 $VLEN(VNAME,IDIM)  ...........  As above, but for the logical length
                                 (i.e. stripping trailing zeroes)
 $NUMVEC  .....................  Current number of vectors
 $VEXIST(VNAME)  ..............  Index of vector VNAME
                                 (1..$NUMVEC or 0 if VNAME does not exist)
 $SUBSTRING(STRING,IX,NCH)  ...  STRING(IX:IX+NCH-1)
 $UPPER(STRING)  ..............  STRING changed to upper case
 $LOWER(STRING)  ..............  STRING changed to lower case
 $LEN(STRING)  ................  Length of STRING
 $INDEX(STR1,STR2)  ...........  Position of first occurrence of STR2 in STR1
 $WORDS(STRING,SEP)  ..........  Number of words separated by SEP
 $WORD(STRING,K,N,SEP)  .......  Extract N words starting at word K
 $QUOTE(STRING)  ..............  Add quotes around STRING
 $UNQUOTE(STRING)  ............  Remove quotes around STRING
 $EXEC('macro args')  .........  EXITM value of EXEC call
 $DEFINED('var_name')  ........  List of defined macro variables
 $EVAL(Expression)  ...........  Result of the Expression computed by KUIP
 $SIGMA(Expression)  ..........  Result of the Expression computed by SIGMA
 $RSIGMA(Expression) ..........  As above but a decimal point is added to
                                 integer results
 $FORMAT(number,format)  ......  Format a number according to a Fortran
                                 format string, e.g.
                                 $FORMAT(1.5,F5.2) ==> ' 1.50'
                                 $FORMAT(123,I5.5) ==> '00123'
 $ARGS  .......................  Command line at program invocation
 $KEYNUM  .....................  Address of latest clicked key in style GP
 $KEYVAL  .....................  Value of latest clicked key in style GP
 $LAST  .......................  Latest command line executed
 $ANUM  .......................  Number of aliases
 $ANAM(I)  ....................  Name of I-th alias
 $AVAL(I)  ....................  Value of I-th alias
 $STYLE  ......................  Current style as defined by SET/STYLE
 $OS  .........................  Operating system name, e.g. UNIX or VMS
 $MACHINE  ....................  Hardware or Unix brand, e.g. VAX or HPUX
 $PID  ........................  Process ID
 $IQUEST(I)  ..................  Value of IQUEST(I) status vector
 $ENV(var)  ...................  Value of environment variable
 $FEXIST(file)  ...............  1 if file exists or 0 otherwise
 $SHELL(cmd,N)  ...............  N'th line of shell command output (Unix only)
 $SHELL(cmd,sep)  .............  Shell output with newlines replaced by sep
 $SHELL(cmd)  .................  Same as $SHELL(cmd,' ')

>Name KUIDF2

>Menu /KUIP

>Menu ALIAS
>Guidance
Operations with aliases.
Aliases are defined to provide shortcut abbreviations for the input line or
some part of it.
When encountered on an input line an alias is replaced by its
string value which can contain further aliases.
(Be careful not to define recursive aliases.)
.
To juxtaposition aliases, a double slash can be used as concatenation sign.
Inside quoted strings and for the ALIAS commands themselves
the alias substitution is inhibited.
Otherwise
 ALIAS/CREATE ALPHA BETA
 ALIAS/CREATE ALPHA BETA
whould create an recursive alias BETA and
 ALIAS/CREATE ALPHA BETA
 ALIAS/CREATE BETA GAMMA
 ALIAS/DELETE ALPHA
would delete the alias name BETA instead of ALPHA itself.

>Command CREATE
>Parameters
NAME  'Alias name' C
VALUE 'Alias value' C
+
CHOPT 'Option' C D=A
-A  create an Argument alias
-C  create a Command alias
-N  No alias expansion of value
>Guidance
Create an alias NAME which should be substituted by VALUE.
An alias name is a sequence of letters and digits starting with a letter.
The underscores ('_'), the at-sign ('@') and the dollar-sign ('$')
count as letters.
.
There are two types of aliases:
Command aliases are recognized only if they occur in the command position,
i.e. as the first token on the line.
Argument aliases are recognized anywhere on the command line
(except inside quoted strings)
if they are surrounded by one of the following separators:
 blank  /  ,  =  :  .  %  '  (  )
.
Also switch ON the alias translation, i.e. ALIAS/TRANSLATION ON.
If CHOPT='C' then the alias is a command alias, i.e. an alias that
will only be translated when it is the first token on a command line.
Example:
 Alias/Create GG Graph/Struct/Scratch
 Alias/Create FF File1/Name1/Name2
 GG FF/ID
is equivalent to
 Graph/Struct/Scratch File1/Name1/Name2/ID
.
 Alias/Create LS DIR C
is equivalent to
 DIR
only when LS is the first token on a command line.
In the following case LS will not be translated
 SHELL LS
.
Aliases occuring inside an value are expanded indepedent whether the value
is enclosed by quotes.
The option -N allows to suppress this implicit alias expansion.
>Action kxacreate%C

>Command LIST
>Parameters
+
NAME  'Alias name wildcard' C D='*'
>Guidance
List all aliases matching the wildcard (names and values).
>Action kxalist%C

>Command DELETE
>Parameters
NAME  'Alias name wildcard' C Loop
>Guidance
Delete the definition of aliases matching the wildcard.
NAME='*' deletes all aliases.
>Action kxadelete%C

>Command TRANSLATION
>Parameters
+
OPTION 'Option' C D='ON'
-?    show current setting
-ON   switch alias translation ON
-OFF  switch alias translation OFF
>Guidance
Switch ON/OFF the alias translation.
If OFF, alias definitions are not used in parsing the command lines.
It is automatically switched ON when an alias is created.
If OPTION='?' the current value is shown.
The startup value is OFF.
>Action kxatranslation%C

>Menu ../SET_SHOW
>Keyword
SHOW
>Guidance
Set or show various KUIP parameters and options.

>Command STYLE
>Parameters
+
OPTION 'Option' C D='?'
-?   show current style
-C   Command line : select Command line input
-AN  Menu with Numbers :_
     select general Alpha menu (with Numbers)
-AL  Menu with Letters :_
     select general Alpha menu (with Letters)
-G   Graphics menu hardware :_
     select Graphics menu (with hardware character fonts)
-GW  Graphics menu shadowed :_
     select Graphics menu (with shadowed Width effect)
-GS  Graphics menu Software :_
     select Graphics menu (with Software character fonts)
-GP  Panel keys :_
     select Graphics menu (with Panel keys only, i.e. no command tree menu)
-XM  Motif/X11 :_
     select Motif/X11 interface
SGYLEN 'max Y LENgth of each menu item box'  R D=0.025 R=0.005:0.25
SGSIZE 'space available for the application' R D=0.8   R=0:0.90
SGYSPA 'max Y length of space between menus' R D=0.02  R=-0.5:0.50
SGBORD 'X or Y border for menus'             R D=0.015 R=0:0.25
WKTYPE 'Graphics workstation type'           I D=0

>Keyword
dialog mode
>Guidance
Select the user dialog style (or working mode).
The startup value is 'C' (command mode).
The current value is returned by the system function $STYLE.
.
The G-styles are only available
if the application program is calling KUWHAG instead of KUWHAT.
When one of these options is choosen
the remaining parameters control the geometrical layout
of the menus on the screen and
the graphics workstation type (in case HIGZ was not initialized).
.
Style 'XM' is only available if the program is calling KUWHAM.
In that case switching to other styles is not possible.
>Action kxstyle%C

>Command PANEL
>Parameters
LINE 'Line number' R D=0
+
GKEY 'Graphics key value(s)' C D=' '
>Keyword
interface command key pixmap graphics Motif
STYLE
>Guidance
Set up a (user-definable) panel of commands with graphics keys.
These keys are associated to pre-defined commands (or list of commands),
which are generally corresponding to actions frequently executed.
.
The "panel interface" is available in "STYLE GP" and in KUIP/Motif
(but not in the basic command mode). Nevertheless the syntax of
the PANEL command is different in these two modes of interface.
The "panel interface" is a lot more powerful in KUIP/Motif,
which means that the command is more complex.
.
N.B. in "STYLE GP" only one panel of commands can be set up,
whereas in KUIP/Motif there is no limitation.
.
Syntax of the command in "STYLE GP" :
.
 PANEL x.y command
.
 where:
 x,y       is the key position (column and row number)
 command   is the complete command name (or list of commands)
           to be excuted when the button is pressed.
.
Examples:
 PANEL  0                        | reset the panel (in memory)
 PANEL  2.04 MESSAGE             | initialize 4th key of 2nd line to MESSAGE
 PANEL  2.04                     | clear 4th key of 2nd line
Note that the key number on the right of the decimal point must
always be defined with two digits.
.
Keys ending with a minus sign make an additional request of keyboard input;
the complete command line will be the key text, with a blank at the place
of the minus, concatenated with the additional keyboard input. Example:
 PANEL 1.03 'VEC/PRI-'        | entering VAB will execute VEC/PRI VAB.
Keys ending with a double minus sign behave as above but no blank is put
at the place of the double minus. Example:
 PANEL 1.03 'VEC/PRI V--'     | entering AB will execute VEC/PRI VAB
The dollar sign inside a key is replaced by additional keyboard input. Example:
 PANEL 1.03 'VEC/PRI V($)'    | entering 11:20 will execute VEC/PRI V(11:20)
.
Syntax of the command in "KUIP/Motif" :
.
All what is described above (for "STYLE GP") is still available.
But the (more) general syntax in "KUIP/Motif" is:
.
 PANEL x.y command [label] [pixmap]
.
 where:
 x,y                is the key position (column and row number)
 command            is the complete command name (or list of commands)
                    to be excuted when the button is pressed.
 label (optional)   is an alias name for this command. If specified,
                    it is used for the button label (when the appropriate
                    "View" option is selected) instead of the complete
                    command (which is generally too long for a "user-friendly"
                    button label.
 pixmap (optional)  has to be specified when you want to have graphical keys
                    instead of pure text labels.
.
In KUIP/Motif, the special value "0" for x.y (PANEL  0 ...) can be used
for different purposes (according to the 2nd parameter value):
.
 PANEL  0 D [title] [geometry]
.
can be use to display the current panel which is in memory with
(optionals) a given title and geometry (size and position).
.
 PANEL  0 C [title]
.
can be use to close the last panel, or the one corresponding to
the given title.
.
Examples:
 - PANEL  0 D 'This is my first panel' 500x300+500+600
displays the panel which has been set in memory by the key definition,
and sets the title to ``This is my first panel'', the window size to
``500x300'' (WxH) and the window position to ``500 600'' in x and y.
If no title and/or no geometry is specified one is given by default.

 - PANEL  0 C 'This is my first panel'
closes (destroys and erases from the screen) the panel whith title
``This is my first panel''.
If no title is specified the last created panel is closed by default.
.
As the "panel interface" is rather complex and powerful in KUIP-Motif,
if you want to know all the possibilities, we invite you to refer to
the KUIP User Guide (where you will also find picture illustrations).
>Action kxpanel%C


>Command NEWPANEL
>Parameters
LINE 'Number of lines' I D=5 R=1:30
COL 'Number of columns' I D=5 R=1:30
TITLE 'Panel Title' C D='NewPanel'
WIDTH 'Panel width (in pixels)' I D=300 R=10:
HEIGHT 'Panel height (in pixels)' I D=300 R=10:
XPOS 'X Position (in pixels)' I D=0 R=0:
YPOS 'Y Position (in pixels)' I D=0 R=0:
>Keyword
panel
>Guidance
Set up a new panel with empty keys.
This new panel must then be filled interactively.

>Action kxnewpan%C

>Command COMMAND
>Parameters
+
CHPATH 'Path name for command line' C D=' '
>Keyword
filter
>Guidance
Set a filter for the parsing of command lines.
If it has been called, it means that whenever a command line is entered,
if and only if it is not an existing command (not just ambiguous),
it is inserted into the CHPATH string, with $n (n=1..9) being replaced
by the n-th token of the command (tokens are separated by spaces),
or $* being replaced by the whole command line. Examples:
 COMMAND 'V/CR $*(10)'
 AA                     =>   V/CR AA(10)
 BB                     =>   V/CR BB(10)
 V/LIST                 =>   V/LIST
.
 COMMAND 'VECTOR/PLOT $1 555 $2'
 AA E                   =>   VECTOR/PLOT AA 555 E
 BB                     =>   VECTOR/PLOT BB 555
.
 COMMAND                =>   shows its current value
 COMMAND *              =>   reset (equivalent to COMMAND $*)
Note that COMMAND and subsequent command lines can be used inside macros,
excepted when producing macro statements (like EXEC, IF, GOTO, etc.).
For example, the above examples would work also inside macros, while
COMMAND 'EXEC $*' or COMMAND 'GOTO $1' will not.
>Action kxcommand%C

>Command APPLICATION
>Parameters
PATH  'Application name' C D=' '
+
CMDEX 'Exit command' C D='EXIT'
>Guidance
Set the application name.
This means that all input lines will be concatenated to the string PATH
(until the command specified by the parameter CMDEX is executed,
which resets the application to the null string). The value of CMDEX
may be specified if the default value EXIT has to be changed
(i.e. because already used by the application).
APPLICATION can also be inserted in a macro: in this case at least
4 characters must be specified (i.e. APPL).
>Action kxapplication%C

>Command ROOT
>Parameters
+
PATH  'Root directory' C D='/'
>Keyword
command search searching
>Guidance
Set the root for searching commands.
If PATH='?' the current root is shown.
This allows to access commands regardless of possible
ambiguities with different menus.
Commands are first searched starting from the current root:
if a command is found it is executed.
Only if a command is not found a second pass of search is done,
starting now from the top root of the command tree (i.e. '/').
>Action kxroot%C

>Command TIMING
>Parameters
+
OPTION 'Option' C D='ON' R='ON,OFF,ALL'
>Keyword
time
>Guidance
Set ON/OFF/ALL the timing of commands.
If ON, the real time and the CPU time for the latest
executed command (or macro) are presented.
If ALL, the time is shown for each command being executed within a macro.
The startup value is OFF.
>Action kxtiming%C

>Command PROMPT
>Parameters
PROMPT 'Prompt string' C D=' '
>Guidance
Set the prompt string for the command mode dialogue.
If PROMPT is blank the current prompt is left unchanged.
If PROMPT contains the character sequence '[]'
the current command number is inserted between the square brackets.
>Action kxprompt%C

>Command BREAK
>Parameters
+
OPTION 'Option' C D='ON' R='ON,OFF,TB,?'
>Keyword
keyboard interrupt interruption macro
>Guidance
Set ON/OFF the break handling.
If OPTION='?' the current value is shown.
The startup value is ON.
.
Hitting the keyboard interrupt (CTRL/C on VMS or CTRL/Q on the Apollo)
under break ON condition, the current command or macro execution
will be interrupted and the user will get again the application prompt.
.
BREAK TB switch ON the traceback of the routines called, with their
line numbers, when an error occurs. This allows the detection of
the routines which provoked the error.
>Action kxbreak%C

>Command COLUMNS
>Parameters
+
NCOL 'Number of columns for terminal output' I D=80 R=-1:
>Guidance
Set the maximum number of columns for terminal output.
If NCOL=0 the current number of columns is shown.
If NCOL=-1 the current number of columns is taken from the environment
variable COLUMNS.
If COLUMNS is undefined the startup value is 80.
>Action kxcolumns%C

>Command RECORDING
>Parameters
+
NREC 'Rate for recording on history file' I D=25 R=0:
>Keyword
RECORD history file LAST
>Guidance
Set the recording rate for the history file.
Every NREC commands of the session the current history file is updated.
If NREC=0 the history is not kept at all (i.e. the file is not written).
See also the command LAST.
>Action kxrecording%C

>Command HOST_EDITOR
>Parameters
+
EDITOR 'Host editor command' C D='?'
TOP    'Top position of the edit window'  I D=20 R=0:
LEFT   'Left position of the edit window' I D=20 R=0:
WIDTH  'Width of the edit window'         I D=0  R=0:
HEIGHT 'Height of the edit window'        I D=0  R=0:
DXPAD  'X offset for help PAD windows'    I D=30 R=0:
DYPAD  'Y offset for help PAD windows'    I D=20 R=0:
NPADS  'Maximum number of shifted pads'   I D=4  R=1:
>Keyword
EDITOR edit vi emacs TPU EDIT
>Guidance
Set the host command to invoke the editor.
The EDIT command will invoke this editor.
If EDITOR='?' the current host editor command is shown.
.
On Apollo the special value EDITOR='DM' invoke Display Manager pads.
The special values EDITOR='WINDOW' and 'PAD' can be used to specify
the window positions (in pixel units).
'WINDOW' defines the parameters for edit pads,
while 'PAD' defines the parameters for read-only pads
(e.g. used by 'HELP -EDIT').
.
On VMS the special values EDITOR='EDT' and 'TPU' invoke the callable
editors.
The startup time is considerably lower compared to spawning the editor
as a subprocess.
The callable EDT has one disadvantage though:
after an error, e.g. trying to edit a file in a non-existing directory,
subsequent calls will always fail.
The TPU call can be augmented by command line options, e.g.
 HOST_EDITOR TPU/DISP=DECW    | DECwindow interface to EVE
.
On Unix a variety of editors are available, e.g.
 HOST_EDITOR vi
 HOST_EDITOR 'emacs -geometry 80x48'
.
On Unix workstations it is possible to do asynchronous editing
via the KUIP edit server,
i.e. to start an editor in a separate window while the application
can continue to receive commands.
In order to do that the following conditions must be fulfilled:
 - The KUIP edit server 'kuesvr' must be found in the search path.
 - The editor command set by HOST_EDITOR must end with an ampersand ('&').
 - The environment variable 'DISPLAY' must be set.
.
The ampersand flags your intention to use the edit server if possible.
If the edit server cannot be used the ampersand will be ignored,
i.e. even with
 HOST_EDITOR 'vi &'
the KUIP/EDIT command will block until the editor terminates
if either the 'kuesvr' is not available or 'DISPLAY' is undefined.
When using the edit server the editor command is expected to create
its own window.
'vi' being a frequent choice, the above command is automatically
interpreted as
 HOST_EDITOR 'xterm -e vi &'
.
The startup value can be defined by the environment variable 'EDITOR'.
Otherwise it is set to a system dependent default:
'DM' (Apollo), 'EDT' (VMS), 'XEDIT' (VM/CMS), 'vi' (Unix).
>Action kxhosteditor%C


>Command HOST_PAGER
>Parameters
+
PAGER 'Host pager command' C D='?'
>Keyword
PAGER view read more
>Guidance
Set the host command to view a file in read-only mode.
If OPTION='?' the current host pager command is shown.
The 'HELP -EDIT' command will invoke this pager, e.g.
 HOST_PAGER more
.
On Unix workstations the pager can be asynchronous by creating a
separate window, e.g.
 HOST_PAGER 'xterm -e view &'
 HOST_PAGER 'ved &'
.
On Apollo the special value PAGER='DM' defines the use of
Display Manager read-only pads.
The pad positions can be adjusted by the HOST_EDITOR command.
.
The startup value can be defined by the environment variables
'KUIPPAGER' or 'PAGER'.
If neither of them is defined the value set by the HOST_EDITOR command
is used.
On VAX/VMS the startup value is 'TYPE/PAGE'.
>Action kxhostpager%C


>Command HOST_PRINTER
>Parameters
+
COMMAND 'Host printer command' C D='?'
FILETYPE 'File extension' C D=' '
>Keyword
PRINTER PRINT
>Guidance
Set the host commands for printing files with KUIP/PRINT.
The KUIP/PRINT command will use the host command matching the file
extension or use the default command defined for FILETYPE=' '.
.
If COMMAND='?' the currently set commands are shown.
If COMMAND=' ' the currently defined command is delete.
The command string can contain '$*' and '$-' to indicate the position
where the file name with/without file extension should be inserted.
For example,

 MANUAL / refman.tex latex
 HOST_PRINTER 'latex $* ; dvips $-' .tex
 KUIP/PRINT refman.tex

invokes the shell command 'latex refman.tex ; dvips refman'.
The predefined defaults are not guaranteed to work
since the actual print commands are very much installation dependent.

>Action kxhostprinter%C


>Command HOST_PSVIEWER
>Parameters
+
PSVIEWER 'Host PostScript Viewer command' C D='?'
>Keyword
PSVIEWER PostScript viewer
>Guidance
Set the host command to invoke the PostScript Viewer.
The PSVIEW command will invoke this PostScript Viewer.
If PSVIEWER='?' then the current viewer command is shown.
.
The startup value can be defined by the environment variables
'KUIPPSVIEWER' or 'PSVIEWER'.
.
On Unix workstations it is by default set to 'ghostview'.
On VAX/VMS the default commands is 'VIEW/FORM=PS/INTERFACE=DECWINDOWS'.

>Action kxhostpsviewer%C


>Command HOST_SHELL
>Parameters
+
SHELL 'Host shell command' C D='?'
>Keyword
SHELL
>Guidance
Set the default host shell invoked by the KUIP/SHELL command.
If OPTION='?' the current host shell is shown.
The startup value is taken from the 'SHELL' environment variable.

>Action kxhostshell%C


>Command RECALL_STYLE
>Parameters
+
OPTION 'Command recall and editing style' C D=?
-?    show current setting
-KSH  Korn shell : Emacs like command line editing
-KSHO Korn shell + Overwrite : like 'KSH' but overwrite instead of insert mode
-DCL  VAX/VMS DCL : DCL command line editing
-DCLO VAX/VMS DCL + Overwrite : like 'DCL' but overwrite instead of insert mode
-NONE disable command line editing
>Keyword
STYLE mode
>Guidance
Set the command recall and editing style.
If OPTION='?' the current style is shown.
The startup value is 'DCL' on VAX/VMS, 'NONE' on Cray and Apollo DM pads,
and 'KSH' on other systems.
.
If the terminal emulator returns ANSI escape sequences (hpterm doesn't!)
the up/down arrow keys can be used to recall items from the command history
list and the left/right arrow keys to move the cursor.
.
'KSH' style provides the following control keys for editing:
  ^A/^E   : Move cursor to beginning/end of the line.
  ^F/^B   : Move cursor forward/backward one character.
  ^D      : Delete the character under the cursor.
  ^H, DEL : Delete the character to the left of the cursor.
  ^K      : Kill from the cursor to the end of line.
  ^L      : Redraw current line.
  ^O      : Toggle overwrite/insert mode. Text added in overwrite mode
            (including yanks) overwrites existing text, while insert mode
            does not overwrite.
  ^P/^N   : Move to previous/next item on history list.
  ^R/^S   : Perform incremental reverse/forward search for string on
            the history list.  Typing normal characters adds to the
            current search string and searches for a match.  Typing
            ^R/^S marks the start of a new search, and moves on to
            the next match.  Typing ^H or DEL deletes the last
            character from the search string, and searches from the
            starting location of the last search.
            Therefore, repeated DELs appear to unwind to the match
            nearest the point at which the last ^R or ^S was typed.
            If DEL is repeated until the search string is empty the
            search location begins from the start of the history
            list. Typing ESC or any other editing character accepts
            the current match and loads it into the buffer,
            terminating the search.
  ^T      : Toggle the characters under and to the left of the cursor.
  ^U      : Kill from the prompt to the end of line.
  ^Y      : Yank previously killed text back at current location.
            Note that this will overwrite or insert, depending on
            the current mode.
  TAB     : By default adds spaces to buffer to get to next TAB stop
            (just after every 8th column).
  LF, CR  : Returns current buffer to the program.
.
'DCL' style provides the following control keys for editing:
  BS/^E   : Move cursor to beginning/end of the line.
  ^F/^D   : Move cursor forward/backward one character.
  DEL     : Delete the character to the left of the cursor.
  ^A      : Toggle overwrite/insert mode.
  ^B      : Move to previous item on history list.
  ^U      : Delete from the beginning of the line to the cursor.
  TAB     : Move to next TAB stop.
  LF, CR  : Returns current buffer to the program.
>Action kxrecallstyle%C

>Command VISIBILITY
>Parameters
CMD   'Command name' C D=' '
+
CHOPT '?, OFF, ON'     C D='?' R='?,OFF,ON'
>Guidance
Set or show the visibility attributes of a command.
.
If CHOPT='OFF':
 - the command it is not executable anymore
 - STYLE G draws a shadowed box on the command
 - HELP may be still requested on the command
The startup value is ON.
>Action kxvisibility%C

>Command DOLLAR
>Parameters
+
OPTION 'Substitution of environment variables' C D=?
-?       show current setting
-ON      enable substitution
-OFF     disable substitution
>Keyword
environment variable substite substitution
>Guidance
Set or show the status of environment variable substitution.
.
This command allows to enable/disable the interpretation of
environment variables in command lines.
The startup value is 'ON', i.e. "$var" is substituted by the variable value.
.
Note that the system function "$ENV(var)" allows using environment variables
even for 'DOLLAR OFF' .

>Action kxdollar%C


>Command FILECASE
>Parameters
+
OPTION 'Case conversion for filenames' C D=?
-?       show current setting
-KEEP    filenames are kept as entered on the command line
-CONVERT filenames are case converted
-RESTORE restore previous FILECASE setting
>Keyword
FILE filename name conversion Case upper lower
>Guidance
Set or show the case conversion for filenames.
.
This command has only an effect on Unix systems to select whether filenames
are kept as entered on the command line.
The startup value is 'CONVERT', i.e. filenames are converted to lowercase.
.
On other systems filenames are always converted to uppercase.
.
The 'RESTORE' option set the conversion mode to the value effective
before the last FILECASE KEEP/CONVERT command.
E.g. the sequence
     FILECASE KEEP; EDIT Read.Me; FILECASE RESTORE
forces case sensitivity for the EDIT command and restores the previous mode
afterwards.

>Action kxfilecase%C

>Command LCDIR
>Parameters
+
DIR*ECTORY 'Directory name' C D=' '
>Keyword
Directory dir
>Guidance
Set or show the local working directory.
.
The current working directory is set to the given path name or the
current directory is shown.
.
To show the current directory used LCDIR without argument.
'LCDIR ~' switches to the home directory.
'LCDIR .' switches back to the working directory at the time
the program was started.

>Action kxlcdir%C

>Name MACDEF

>Menu MACRO
>Guidance
Macro Processor commands.

>Command EXEC
>Parameters
MNAME 'Macro name' C
+
MARGS 'Macro arguments' C D=' ' Separate
>Guidance
Execute the command lines contained in the macro MNAME.
As a file can contain several macros, the character '#' is
used to select a particular macro inside a file as explained below.
.
If MNAME does not contain the character '#', the file MNAME.KUMAC
is searched and the first macro is executed (it may be an unnamed
macro if a MACRO statement is not found as first command line in the file).
.
If MNAME is of the form FILE#MACRO, the file named FILE.KUMAC is searched
and the macro named MACRO is executed.
.
Examples:
 EXEC ABC   to exec first (or unnamed) macro of file ABC.KUMAC
 EXEC ABC#M to exec macro M of file ABC.KUMAC
.
The command MACRO/DEFAULTS can be used to define a directory search path
for macro files.
>Action kxmexec%C

>Command LIST
>Parameters
+
MNAME 'Macro name pattern' C D=' '
>Guidance
List all macros in the search path defined by MACRO/DEFAULTS.
Macros are files with the extension KUMAC.
MNAME may be specified to restrict the list to the macros
containing such a string in the first part of their name.
For example,
 MACRO/LIST ABC
will list only macros starting with ABC.
>Action kxmlist%C

>Command TRACE
>Parameters
+
OPTION 'Option' C D='ON' R='ON,OFF'
LEVEL  'Level'  C D=' ' Option
-
-TEST
-WAIT
-FULL
-DEBUG
>Keyword
DEBUG
>Guidance
Set ON/OFF the trace of commands during macro execution.
If TRACE='ON' the next command is written on the terminal
before being executed.
If LEVEL='TEST' the command is only echoed but not executed.
If LEVEL='WAIT' the command WAIT is automatically inserted
after the execution of each command.
The startup values are OPTION='OFF' and LEVEL=' '.
>Action kxmtrace%C

>Command DEFAULTS
>Parameters
+
PATH 'Search path for macro files' C D='?'
OPTION 'Automatic EXEC' C D='?'
-?            show current setting
-Command      search for commands only
-C            same as 'Command'
-Auto         search for commands before macros
-A            same as 'Auto'
-AutoReverse  search for macros before commands
-AR           same as 'AutoReverse'
>Guidance
Set or show MACRO search attributes.
.
On Unix and VMS systems
PATH defines a comma separated list of directories in which the commands
KUIP/EDIT, MACRO/EXEC, and MACRO/LIST search for macro files.
For example,

 MACRO/DEFAULT '.,macro,~/macro'          | Unix

 MACRO/DEFAULT '[],[.macro],[macro]'      | VMS

defines to search files first in the current directory,
then in the subdirectory 'macro' of the current directory,
and last the subdirectory 'macro' of the home directory.
.
On VM/CMS system PATH defines a comma separated list of filemodes.
E.g.

 MACRO/DEFAULT '*'       | search all disks
 MACRO/DEFAULT 'A,C'     | search only disks A and C
.
If PATH='?' the currently defined search path is shown.
If PATH='.' the search path is undefined, i.e. files are search for
in the current directory (A-disk on VM/CMS) only.
The startup value is PATH='.'.
.
The search path is not applied if the file specification already
contains an explicit directory path or if it starts with a '-'
character (which is stripped off).
.
OPTION allows to define whether macros can be invoked by their name only
without prepending the KUIP/EXEC command:
 DEFAULT -Command
 CMD                     | CMD must be a command
 DEFAULT -Auto
 CMD                     | if CMD is not a command try EXEC CMD
 DEFAULT -AutoReverse
 CMD                     | try EXEC CMD first; if not found try command CMD
The startup value is 'Command' (also reset by PATH='.').
.
Important note:
.
Inside macros the DEFAULT -A (or -AR) logic is disabled, i.e.
DEFAULT -C is always assumed.
>Action kxmdefaults%C

>Command DATA
>Keyword
Application file
>Guidance
Application command to store immediate data into a file.
Example:

 Application DATA vec.dat
 1  2  3
 4  5  6
 7  8  9
 vec.dat
 vec/read x,y,z vec.dat

>Action kxmdata%C


>Menu GLOBAL
>Guidance
Operations on global variables.

>Command CREATE
>Parameters
NAME  'Variable name' C Loop
+
VALUE 'Initial value' C D=' '
TEXT  'Comment text'  C D=' '
>Keyword
variable
>Guidance
Create a global variable.
.
If used inside a macro the variable [name] is declared as global.
>Action kxgcreate%C

>Command IMPORT
>Parameters
NAME  'Variable name' C Loop
>Guidance
Import global variables.
.
If used inside a macro the variables listed are declared as global.
The name may contain '*' as a wildcard matching any sequence of characters.

>Action kxgimport%C

>Command DELETE
>Parameters
NAME  'Variable name' C Loop
>Guidance
Delete global variables.
.
The global variables listed are deleted.
The name may contain '*' as a wildcard matching any sequence of characters.

>Action kxgdelete%C

>Command LIST
>Parameters
+
NAME  'Variable name' C D='*'
FILE  'Output file'   C D=' '
>Guidance
List global variables.
.
If a file name is specified the output is the list of GLOBAL/CREATE
commands to define the selected global variables.
The default file extension is .kumac.
>Action kxglist%C


>Menu ../SYNTAX
>Keyword
statement
>Guidance
Explanation of KUIP macro language and syntax.
.
A macro is a set of command lines stored in a file, which can be created and
modified with any text editor.
.
In addition to all available KUIP commands the  special "macro statements"
listed below are valid only inside macros. Note that the statement keywords are
fixed. Aliasing such as "ALIAS/CREATE jump GOTO" is not allowed.

>Menu Expressions
>Keyword
parser
>Guidance
Explanation of KUIP expression syntax.
.
KUIP has a built-in parser for different kinds of expressions: arithmetic
expressions, boolean expressions, string expressions, and "garbage
expressions".

>Help_Item Arithmetic
>Guidance
Explanation of arithmetic expression syntax.
.
The syntactic elements for building arithmetic expressions are:
.
        expr ::=  number
                | vector-name                (for scalar vectors)
                | vector-name(expr)
                | vector-name(expr,expr)
                | vector-name(expr,expr,expr)
                | [variable-name]            (if value is numeric or
                                              the name of a scalar vector)
                | [variable-name](expr...)   (if value is a vector name)
                | alias-name                 (if value is numeric constant)
                | $system-function(...)
                | - expr
                | expr + expr
                | expr - expr
                | expr * expr
                | expr / expr
                | (expr)
                | ABS(expr)
                | INT(expr)
                | MOD(expr,expr)
.
They can be used in the macro statements DO, FOR, and EXITM, in macro variable
assignments, as system function arguments where a numeric value is expected, or
as the argument to the $EVAL function.
.
Note that all arithmetic operations are done in floating point, i.e., "5/2"
becomes "2.5". If a floating point result appears in a place where an integer
is expected, for example as an index, the value is truncated.

>Help_Item Boolean
>Guidance
Explanation of Boolean expression syntax.
.
Boolean expressions can only be used in the macro statements IF, WHILE, and
REPEAT. The possible syntactic elements are shown below.
.
         bool  ::= expr rel-op expr
                 | string eq-op string
                 | expr eq-op string
                 | .NOT. bool
                 | bool .AND. bool
                 | bool .OR. bool
                 | ( bool )
.
        rel-op ::= .LT. | .LE. | .GT. | .GE.
                 |  <   |  <=  |  >   |  >=
                 | eq-op
.
        eq-op  ::= .EQ. | .NE.
                 |  =   | <>

>Help_Item String
>Keyword
CASE FOR EXITM
>Guidance
Explanation of string expression syntax.
.
String expressions can be used in the macro statements CASE, FOR, and EXITM, in
macro variable assignments, as system function arguments where a string value
is expected, or as the argument to the $EVAL function. They may be constructed
from the syntactic elements shown below.
.
        string ::= quoted-string
                 | unquoted-string
                 | string // string             (concatenation)
                 | expr // string               (expr represented as string)
                 | [variable-name]
                 | alias-name
                 | $system-function(...)

>Help_Item Garbage
>Guidance
Explanation of "garbage" expression syntax.
.
Expressions which do not satisfy any of the other syntax rules we want to call
"garbage" expressions.  For example,
.
        s = $OS$MACHINE
.
is not a proper string expression. Unless they appear in a macro statement
where specifically only an arithmetic or a boolean expression is allowed, KUIP
does not complain about these syntax errors. Instead the following
transformations are applied:
.
   o  alias substitution
.
   o  macro variable replacement; values containing a
      blank character are implicitly quoted
.
   o  system function calls are replaced one by one with
      their value provided that the argument is a syntactically
      correct expression
.
   o  string concatenation

>Menu ../Variables
>Keyword
assign assignment
>Guidance
Explanation of KUIP macro variables.
.
Macro variables do not have to be declared. They become defined by an
assignment statement,
.
        name = expression
.
The right-hand side of the assignment can be an arithmetic expression, a string
expression, or a garbage expression (see MACRO/SYNTAX/Expressions). The
expression is evaluated and the result is stored as a string (even for
arithmetic expressions).
.
A variable value can be used in other expressions or in command lines by
enclosing the name in square brackets, [name]. If the name enclosed in brackets
is not a macro variable then no substitution takes place.

>Help_Item Numbered
>Keyword
SHIFT
>Guidance
Accessing macro arguments.
.
The EXEC command can pass arguments to a macro. The arguments are assigned to
the numbered variables [1], [2], etc., in the order given in the EXEC command.
The name of the macro, including the file specification, is assigned to [0].
.
A numbered variable cannot be redefined, i.e., an assignment such as "1 = foo"
is illegal.  See MACRO/SYNTAX/SHIFT.

>Help_Item Special
>Guidance
Predefined special macro variables.
.
For each macro the following special variables are always defined:
.
        [0]     Fully qualified name of the macro.
        [#]     Number of macro arguments
        [*]     List of all macro arguments, separated by blanks
        [@]     EXITM return code of the last macro called by
                the current one.  The value is "0" if the last
                macro did not supply a return code or no macro
                has been called yet.
.
As for numbered variables these names cannot be used on the left-hand side of
an assignment. The values or [#] and [*] are updated by the SHIFT statement.


>Help_Item Indirection
>Keyword
reference substitute substitution
>Guidance
Referencing a macro variable indirectly.
.
Macro variables can be referenced indirectly. If the variable [name] contains
the name of another variable the construct
.
        [%name]
.
is substituted by that other variable's value.  For example, this is another
way to traverse the list of macro arguments:
.
        DO i=1,[#]
          arg = [%i]
          ...
        ENDDO
.
There is only one level of indirection, i.e., the name contained in "name" may
not start with another "%".

>Help_Item Global
>Keyword
EXTERN
>Guidance
Declaring a global variable.
.
        EXTERN name ...
.
The variable names listed in the EXTERN statement are declared as
global variables.
If a name has not been defined with the GLOBAL/CREATE command,
it is created implicitly and initialized to the empty string.
The name list may contain wildcards, for example
.
        EXTERN *
.
makes all defined global variables visible.

>Help_Item READ
>Keyword
query
>Guidance
Reading a variable value from the keyboard.
.
        READ name  [ prompt ]
.
Variable values can be queried from the user during macro execution. The READ
statement prompts for the variable value. If name is already defined the
present value will be proposed as default.

>Help_Item SHIFT
>Guidance
Manipulation numbered variables.
.
The only possible manipulation of numbered variables is provided by the SHIFT
statement which copies [2] into [1], [3] into [2], etc., and discards the value
of the last defined numbered variable. For example, the construct
.
        WHILE [1] <> ' ' DO
          arg = [1]
          ...
          SHIFT
        ENDDO
.
allows to traverse the list of macro arguments.


>Menu ../Definitions
>Guidance
Statements for defining macros.

>Help_Item MACRO
>Guidance
Defining a macro.
.
A .kumac file may contain several macros.  An
individual macro has the form
.
        MACRO macro-name [ parameter-list ]
           statements
        RETURN
.
Each statement is either a command line or one of the macro constructs
described in this section (MACRO/SYNTAX).  For the first macro in the file the
MACRO header can be omitted.  For the last macro in the file the RETURN trailer
may be omitted.  Therefore a .kumac file containing only commands (like the
LAST.KUMAC) already constitutes a valid macro.

>Help_Item RETURN
>Guidance
Ending a macro definition
.
        RETURN [ value ]
The RETURN statement flags the end of the macro definition and not the end of
macro execution, i.e., the construct
.
        IF ... THEN
          RETURN         | error!
        ENDIF
.
is illegal.  See MACRO/SYNTAX/EXITM.
.
The value is stored into the variable [@] in the calling macro. If no value is
given it defaults to zero.

>Help_Item EXITM
>Guidance
Terminate macro execution and return to calling macro.
.
        EXITM [ value ]
.
In order to return from a macro prematurely the EXITM statement must be used.
The value is stored into the variable [@] in the calling macro. If no value is
given it defaults to zero.

>Help_Item STOPM
>Guidance
Terminate macro execution and return to command line prompt.
.
        STOPM
.
The STOPM statement unwinds nested macro calls and returns to
the command line prompt.

>Help_Item ENDKUMAC
>Guidance
Ignore rest of KUMAC file.
.
A logical "end of file" marker.  The KUIP parser will not read any part of a
.kumac file which appears after the "ENDKUMAC" command.


>Menu ../Branching
>Guidance
Macro statements for general flow control.

>Help_Item CASE
>Guidance
Select one of many branches.
.
        CASE expression IN
        (label)  statement  [ statements ]
        ...
        (label)  statement  [ statements ]
        ENDCASE
.
The CASE switch evaluates the string expression and compares it one by one
against the label lists until the first match is found. If a match is found the
statements up to the next label are executed before skipping to the statement
following the ENDCASE. None of the statements are executed if there is no match
with any label.
.
Each label is a string constant and the comparison witht the selection
expression is case-sensitive.
If the same statement sequence should be executed for distinct values a
comma-separated list of values can be used.
.
The "*" character in a label item acts as wildcard matching any string of zero
or more characters, i.e., "(*)" constitutes the default label.

>Help_Item GOTO_and_IF_GOTO
>Guidance
Unconditional and conditional branching.
.
        GOTO label
.
The simplest form of flow control is provided by the GOTO statement which
continues execution at the statement following the target "label:". If the jump
leads into the scope of a block statement, for example a DO-loop, the result is
undefined.
.
The target may be given by a variable containing the actual label name.
.
        IF expression GOTO label
.
This old-fashioned construct is equivalent to
.
        IF expression THEN
           GOTO label
        ENDIF

>Help_Item IF_THEN
>Guidance
Conditional execution of statement blocks.
.
        IF expression THEN
           statements
        ELSEIF expression THEN
           statements
        ...
        ELSEIF expression THEN
           statements
        ELSE
           statements
        ENDIF
.
The general IF construct executes the statements following the first IF/ELSEIF
clause for with the boolean expression is true and then continues at the
statement following the ENDIF. The ELSEIF clause can be repeated any number of
times or can be omitted altogether. If none of the expressions is true, the
statements following the optional ELSE clause are executed.

>Help_item ON_ERROR
>Guidance
Installing an error handler.
.
Each command returns a status code which should be zero if the
operation was successful or non-zero if any kind of error condition
occurred.
The status code can be tested by $IQUEST(1) system function.
.
        ON ERROR GOTO label
.
installs an error handler which tests the status code after each
command and branches to the given label when a non-zero value is
found.
The error handler is local to each macro.
.
        ON ERROR EXITM  [ expression ]
and
        ON ERROR STOPM
.
are short-hand notations for a corresponding EXITM or STOPM statement
at the targat label.
.
        ON ERROR CONTINUE
.
continues execution with the next command independent of the status
code.
This is the initial setting when entering a macro.
.
        OFF ERROR
.
An error handler can be deactivated by this statement.
.
        ON ERROR
.
An error handler can be reactivated by this statement.


>Menu ../Looping
>Guidance
Macro statements for construction loops.

>Help_Item DO
>Guidance
Loop incrementing a loop counter.
.
        DO loop = start_expr, finish_expr  [, step_expr ]
           statements
        ENDDO
.
The step size (setp_expr) defaults to "1". The arithmetic expressions involved
can be floating point values but care must be taken of rounding errors.
.
Note that "DO i=1,0" results in zero iterations and that the expressions are
evaluated only once.

>Help_Item FOR
>Guidance
Loop over items in an expression list.
.
        FOR name IN expr_1 [ expr_2 ... expr_n ]
           statements
        ENDFOR
.
In a FOR-loop the number of iterations is determined by the number of items in
the blank-separated expression list. The expression list must not be empty. One
by one each expression evaluated and assigned to the variable name before the
statements are executed.
.
The expressions can be of any type: arithmetic, string, or garbage expressions,
and they do not need to be all of the same type. In general each expression is
a single list item even if the result contains blanks.
.
The variable [*] is treated as a special case being equivalent to the
expression list "[1] [2] ... [n]" which allows yet another construct to
traverse the macro arguments:
.
        FOR arg IN [*]
           ...
        ENDFOR

>Help_Item REPEAT
>Guidance
Loop until condition becomes true.
.
        REPEAT
           statements
        UNTIL expression
.
The body of a REPEAT-loop is executed at least once and iterated until the
boolean expression evaluates to true.


>Help_Item WHILE
>Guidance
Loop while condition is true.
.
        WHILE expression DO
           statements
        ENDWHILE
.
The WHILE-loop is iterated while the boolean expression evaluates to true. The
loop body is not executed at all if the boolean expression is false already in
the beginning.

>Help_Item BREAKL
>Guidance
Terminate a loop.
.
        BREAKL [ level ]
.
Allows to terminate a loop prematurely. The BREAKL continues executing after
the end clause of a DO, FOR, WHILE, or REPEAT block, where "level"
indicates how many nested constructs to terminate.  The default value level=1
terminates the innermost loop construct.

>Help_Item NEXTL
>Guidance
Continue with next loop iteration.
.
        NEXTL [ level ]
.
Allows to continue with the next loop iteration without executing the rest
of the loop body.
Execution continues just before the end clause of a DO, FOR, WHILE, or
REPEAT block, where "level" indicates how many nested blocks to skip.
The default value level=1 skips to the end of the innermost loop construct.


>Name VECDEF

>Menu VECTOR
>Guidance
Vector Processor commands.
Vectors are equivalent to FORTRAN 77 arrays and they use
the same notation except when omitting indexes (see last line below).
Up to 3 dimensions are supported. Examples:
 Vec(20) (mono-dimensional with 20 elements)
may be addressed by:
 Vec          for all elements
 Vec(13)      for element 13-th
 Vec(12:)     for elements 12-th to last
 Vec(:10)     for elements first to 10-th
 Vec(5:8)     for elements 5-th to 8-th
.
 Vec(3,100) (2-dimensional with 3 columns by 100 rows):
may be addressed by:
 Vec(2,5:8)   for elements 5-th to 8-th in 2-nd column
 Vec(2:3,5:8) for elements 5-th to 8-th in 2-nd to 3-rd columns
 Vec(2,5)     for element 5-th in 2-nd column
 Vec(:,3)     for all elements in 3-rd row
 Vec(2)       for all elements in 2-nd column (SPECIAL CASE)
The latest line shows the special (and non-standard with FORTRAN 77)
notation such that missing indexes are substituted to the right.
.
An 'invisible' vector called '?', mono-dimensional and of length 100,
is always present. Is is used for communicating between user arrays
and KUIP vectors, being equivalenced with the real array VECTOR(100)
in the labeled common block /KCWORK/.

>Command CREATE
>Parameters
VNAME 'Vector name(length)' C
+
TYPE 'Vector type' C D='R' R='R,I'
VALUES 'Value list' C D=' ' Vararg Separate
>Guidance
Create a vector named VNAME (elements are set to zero).
The dimensions are taken from the name,
for example VEC(20), VEC(3,100), VEC(2,2,10).
Up to 3 dimensions are supported. Dimensions which are not specified
are taken to 1, for example VEC(10) ---> VEC(10,1,1) and VEC ---> VEC(1,1,1).
The vector may be of type Real or Integer.
A vector is filled at the same time if parameters are given
after the TYPE:
 VEC/CREATE V(10) R 1 2 3 4 5 66 77 88 99 111
 VEC/CREATE W(20) R 1 2 3
In the last example only the first three elements are filled.
Vector elements may be changed later with the command VECTOR/INPUT.
.
If many equal values have to be entered consecutively, one can specify just
one value and precede it by a repetition factor and an asterisk. Example:
 VEC/CREATE Z(20) R 5*1 2 4*3   --->   VEC/CREATE Z(20) R 1 1 1 1 1 2 3 3 3 3
Enter HELP VECTOR for more information on vector addressing.
>Action kxvcreate%C

>Command LIST
>Guidance
List all vectors (name, dimensions, type).
>Action KXLISV

>Command DELETE
>Parameters
VLIST 'Vector list' C D=' ' Loop
>Guidance
Delete from memory all vectors in the list VLIST.
The vectors are separated in the list by a comma and embedded blanks
are not allowed. An asterisk at the end of VLIST acts as wild-card:
 VEC/DEL AB*          --->  deletes all vectors starting by AB
 VEC/DEL *            --->  deletes all vectors
>Action KXDELV

>Command COPY
>Parameters
VNAM1 'Source vector name' C
VNAM2 'Destination vector name' C
>Guidance
Copy a vector into another one.
Mixed vector type copy is supported (e.g. Integer ---> Real and viceversa).
If VNAM2 does not exist it is created with the required dimensions,
not necessarily the same as the source vector if a sub-range was specified.
For example, if A is a 3 x 100 vector and B does not exist,
COPY A(2,11:60) B will create B as a 50 elements mono-dimensional vector;
a special (and non-standard with FORTRAN 77) notation is used such that,
still using the above vectors, COPY A(2,1:100) B and COPY A(2) B have the
same effect.
.
Note that VECTOR/COPY does not allow a range for the destination vector
not specifying consecutive elements (i.e. along the first dimension):
 VEC/COPY V(5)      W(3,4)     | O.K.
 VEC/COPY V1(2:3,5) V2(4:5,9)  | O.K.
 VEC/COPY V1(5,2:3) V2(4:5,9)  | O.K.
 VEC/COPY V1(3,3:4) V2(4,4:5)  | NOT allowed
 VEC/COPY V1(2:3,5) V2(2,4:5)  | NOT allowed
Enter HELP VECTOR for more information on vector addressing.
>Action KXCOPV

>Command INPUT
>Parameters
VNAME 'Vector name' C
+
VALUES 'Value list' C D=' ' Vararg Separate
>Guidance
Enter values into a vector from the terminal.
Example:
 VEC/INPUT V(6:10) 1.1 2.22 3.333 4.4444 5.55555
If many equal values have to be entered consecutively, one can specify just
one value and precede it by a repetition factor and an asterisk. Example:
 VEC/INPUT V 5*1 2 4*3   --->   VEC/INPUT V 1 1 1 1 1 2 3 3 3 3
Enter HELP VECTOR for more information on vector addressing.
>Action kxvinput%C

>Command PRINT
>Parameters
VNAME 'Vector name' C
+
DENSE 'Output density' I D=1 R=0,1,2
>Guidance
Write to the terminal the content of a vector.
Enter HELP VECTOR for more information on vector addressing.
.
If DENSE.EQ.0 the output is one vector element per line.
If DENSE.EQ.1 the output for a sequence of identical vector elements
is compressed to two lines stating the start and end indices.
If DENSE.EQ.2 the output for a sequence of identical vector elements
is compressed to a single line.
>Action kxvprint%C

>Command READ
>Parameters
VLIST  'Vector list' C
FNAME  'File name' C D=' '
+
FORMAT 'Format' C D=' '
OPT    'Options' C D='OC' R='OC,O, ,C'
MATCH  'Matching pattern' C D=' '
>Guidance
Enter values into vector(s) from a file.
A format can be specified, e.g. FORMAT='F10.5,2X,F10.5',
or the free format is used if FORMAT is not supplied.
.
If vector(s) are not existing they will be created
of the size as read from the file.
.
Vectors in the list VLIST (maximum 30) are separated by a comma and
embedded blanks are not allowed. If subscripts are present
in vector names, the smallest one is taken.
.
OPT is used to select between the following options:
 'OC'   file is Opened, read and then Closed (default case)
 'O'    file is Opened and then read (left open for further reading)
 ' '    file is read (already open, left so for further reading)
 'C'    file is read and then Closed (already open)
If the character 'Z' is present in OPT, the vector elements equal to zero
after reading are set to the latest non-zero element value
(for example reading 1 2 3 0 0 4 0 5 will give 1 2 3 3 3 4 4 5).
.
MATCH is used to specify a pattern string, restricting
the vector filling only to the records in the file which
verify the pattern. Example of patterns:
  /string/      match a string (starting in column 1)
 -/string/      do not match a string (starting in column 1)
  /string/(n)   match a string, starting in column n
  /string/(*)   match a string, starting at any column
Enter HELP VECTOR for more information on vector addressing.
>Action KXREAV

>Command WRITE
>Parameters
VLIST  'Vector list' C
+
FNAME  'File name' C D=' '
FORMAT 'Format' C D='(1X,G13.7)'
CHOPT  'Options' C D='OC' R='OC,O, ,C'
>Guidance
Write to a file the content of vector(s).
If FNAME=' ' the content is written to the terminal.
A format can be specified, e.g. FORMAT='F10.5,2X,F10.5',
or the default one is used if FORMAT is not supplied.
.
Vectors in the list VLIST (maximum 30) are separated by a comma and
embedded blanks are not allowed. If subscripts are present
in vector names, the smallest one is taken.
.
CHOPT is used to select between the following options:
 'OC'   file is Opened, written and then Closed (default case)
 'O'    file is Opened and then written (left open for further writing)
 ' '    file is written (already open, left so for further writing)
 'C'    file is written and then Closed (already open)
Enter HELP VECTOR for more information on vector addressing.
>Action KXWRIV

>Menu OPERATIONS
>Guidance
Simple arithmetic operations between vectors.
In all the operations only the minimum vector length is considered,
i.e. an operation between a vector A of dimension 10 and a vector B
of dimension 5 will involve the first 5 elements in both vectors.
If the destination vector does not exist,
it is created with the same length as the source vector.

>Command VBIAS
>Parameters
VNAM1 'Source vector name' C
BIAS  'Bias value' R
VNAM2 'Destination vector name' C
>Keyword
BIAS
>Guidance
VNAM2(I) = BIAS     + VNAM1(I)
>Action KXVOPE

>Command VSCALE
>Parameters
VNAM1 'Source vector name' C
SCALE 'Scale factor' R
VNAM2 'Destination vector name' C
>Keyword
SCALE
>Guidance
VNAM2(I) = SCALE    * VNAM1(I)
>Action KXVOPE

>Command VADD
>Parameters
VNAM1 'First source vector name' C
VNAM2 'Second source vector name' C
VNAM3 'Destination vector name' C
>Keyword
ADD
>Guidance
VNAM3(I) = VNAM1(I) + VNAM2(I)
>Action KXVOPE

>Command VMULTIPLY
>Parameters
VNAM1 'First source vector name' C
VNAM2 'Second source vector name' C
VNAM3 'Destination vector name' C
>Keyword
MULTIPLY
>Guidance
VNAM3(I) = VNAM1(I) * VNAM2(I)
>Action KXVOPE

>Command VSUBTRACT
>Parameters
VNAM1 'First source vector name' C
VNAM2 'Second source vector name' C
VNAM3 'Destination vector name' C
>Keyword
SUBTRACT
>Guidance
VNAM3(I) = VNAM1(I) - VNAM2(I)
>Action KXVOPE

>Command VDIVIDE
>Parameters
VNAM1 'First source vector name' C
VNAM2 'Second source vector name' C
VNAM3 'Destination vector name' C
>Keyword
DIVIDE
>Guidance
VNAM3(I) = VNAM1(I) / VNAM2(I)     ( or 0 if VNAM2(I)=0 )
>Action KXVOPE

