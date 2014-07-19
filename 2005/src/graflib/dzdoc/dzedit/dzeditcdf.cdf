*
* $Id: dzeditcdf.cdf,v 1.1.1.1 1996/03/04 16:13:24 mclareni Exp $
*
* $Log: dzeditcdf.cdf,v $
* Revision 1.1.1.1  1996/03/04 16:13:24  mclareni
* Dzdoc/Zebpack
*
*
 
>NAME DZEDEF
>MENU DZEDIT
>GUIDANCE
Maintain the DZDOC data base
 
>COMMAND CREATEDOC
>GUIDANCE
Create a new RZ-file from documentation card image file
>PARAMETERS
CHCARD 'Input card image file' C
CHRZF  'Output RZ-file' C
>ACTION DZEMRZ
 
>COMMAND UPDATEDOC
>GUIDANCE
Update an existing RZ-file with new documentation, this command
can also be used to put the documentation into any subdirectory
of an existing RZ-file.
>PARAMETERS
CHCARD 'Input card image file' C
CHRZF  'Output RZ-file' C
+
CHSUBD 'Subdirectory name (blank=topdir)' C
>ACTION DZEURZ
 
>COMMAND OPENRZFILE
>GUIDANCE
Open an existing RZ-file for later use with list/draw/export commands
(the file will be READONLY unless the 'U'=update option is given)
>PARAMETERS
CHRZF  'Input RZ-file' C
+
CHOOPT 'Option for RZFILE (U=Update)' C D='X'
>ACTION DZEOPL
 
>COMMAND CLOSERZFILE
>GUIDANCE
Close  an open RZ-file.
>ACTION DZDCLD
 
>COMMAND PURGEKEY
>GUIDANCE
Purge cycles of all keys keeping the last NKEEP.
If NKEEP < 0 keep just the highest cycle.
>PARAMETERS
+
NKEEP  'Number of cycles to be kept' I D=-1
>ACTION DZEPUR
 
>COMMAND DELETEKEY
>GUIDANCE
Delete a key specifying BankId and UpBankId and cycle number.
  ICYCLE >= highest: delete highest cycle,
          = 0:       delete lowest cycle,
          = -1, -2 : delete highest-1, -2..
Options:
          'C': delete all cycles
          'K': delete all keys,
          'S': delete all cycles smaller ICYCLE.
               (See also ZEBRA users guide: RZDELK)
>PARAMETERS
CHBSBK 'Hollerith Id of selected bank' C
CHBSUP 'Hollerith Id of its up-bank' C
+
ICYCLE 'Cycle number' I D=0
CHDOPT 'Delete option' C D='C'
>ACTION DZEDEL
 
>COMMAND LISTDIRECTORY
>GUIDANCE
List directory of an existing RZ-file
>PARAMETERS
CHRZF  'Input RZ-file' C
>ACTION DZEDRZ
 
>COMMAND LISTONEBANK
>GUIDANCE
List documentation for a selected bank or a group of banks.
An '*' (asterix) may be used as wild card character.
Exactly two times 4 characters are needed to define bank and up-bank.
only the last cycle of one entry is listed regardless of its date.
List of all cycles or selection by date of entering the RZ-file may
also be choosen. The date is an integer of the form YYMMDD.
List option:
          'P' PostScript file (default: simple text file)
          'I' use format of the bank descriptor cards
              (i.e. Input to CREATEDOC)
 
>PARAMETERS
CHBSBK 'Hollerith Id of selected bank' C
CHBSUP 'Hollerith Id of its up-bank' C
+
CHLIST 'File for listing' C D=' '
CHLOPT 'List option: P=PostScript, I=Input' C D=' '
CHOYNO 'List all cycles    ' C D='NO' R='NO,YES'
IDATCH 'List only after date' I D=0
>ACTION DZELSG
 
>COMMAND LISTALL
>GUIDANCE
List documentation for all banks
List option:
          'P' PostScript file (default: simple text file)
          'I' use format of the bank descriptor cards
              (i.e. Input to CREATEDOC)
>PARAMETERS
+
CHLIST 'File for listing' C D=' '
CHLOPT 'List option: P=PostScript, I=Input' C D=' '
CHOYNO 'List all cycles    ' C D='NO' R='NO,YES'
IDATCH 'List only after date' I D=0
>ACTION DZELAL
 
>COMMAND HTML
>GUIDANCE
List documentation in HTML format to be used by WWW.
Use wildcard '****' 'NONE' as Bankid / Upid to get all banks done.
List option:
          '1' put all into 1 single file, default is 1 file/bank
 
>PARAMETERS
CHBSBK 'Hollerith Id of selected bank' C
CHBSUP 'Hollerith Id of its up-bank' C
+
CHOPT  'Option: 1 = 1 single file' C D=' '
>ACTION DZEHTM
 
>COMMAND DRAWONETREE
>GUIDANCE
Draw tree below a selected bank. All banks belonging to the
tree will be actually lifted in memory each with three data
words. Word 1, 2, 3 are the number of data words, links and
structural links as described in the documentation, a -1 indicates
a variable number. A global title may be given which appears
on the front page of the document.
Output option:
               'P' PostScript file (default)
               'Q' quiet i.e. generate no output files
               'C' check consistency
               'M' Put as many down banks as posibble on one picture
                   (Per default 2nd level are only drawn if all fit)
 
>PARAMETERS
CHBSBK 'Hollerith Id of selected bank' C
CHBSUP 'Hollerith Id of its up-bank' C
+
CHMETA 'Name of temp plot file' C D=' '
CHSGML 'Name of temp text-file' C D=' '
CHPOST 'Name of PostScript file' C D=' '
CHOPT  'Option (P=PostScript) ' C D=' '
CTITLE 'Global title' C D='ZEBRA-Datastructures'
>ACTION DZEDRW
 
>COMMAND DRAWALL
>GUIDANCE
Draw tree below all top banks (i.e. banks having 'NONE' as
Up-bank. This also checks the consistency and completeness
of the documentation if option 'N' is not given.
Output option:
               'P' or ' ' PostScript file
               'Q'        no output (check only)
               'S'        suppress confirmation
               'M' Put as many down banks as posibble on one picture
                   (Per default 2nd level are only drawn if all fit)
>PARAMETERS
+
CHMETA 'Name of temp plot file' C D=' '
CHSGML 'Name of temp text-file' C D=' '
CHPOST 'Name of PostScript file' C D=' '
CHOPT  'Output:P=PostScript, N nocheck) ' C D=' '
CTITLE 'Global title' C D='ZEBRA-Datastructures'
>ACTION DZEDRA
 
>COMMAND DZDISP
>GUIDANCE
Display the last generated tree if graphics is available. Note
that the number of data words is three for each bank. Their
contents indicates the documented number of data words and links.
Use option C to get colored display.
>PARAMETERS
+
CHOPT 'Options: C = use Color' C D=' '
>ACTION DZEDSP
 
>COMMAND DZDDIV
>GUIDANCE
Call DZDDIV
>PARAMETERS
+
CHOPT 'Options: C = use Color' C D=' '
>ACTION DZEDSP
 
>COMMAND DZDIRZ
>GUIDANCE
Call DZDIRZ, display RZ-directory tree
>PARAMETERS
+
CHOPT 'Options: C = use Color' C D=' '
>ACTION DZEDSP
 
 
>COMMAND DZEQUIT
>GUIDANCE
Set flag to quit when returning from exec dzedit.kumac
>ACTION DZEQIT
 
>MENU MAKECODE
>GUIDANCE
Generate PATCHY KEEP sequences containing FORTRAN code. With option
'S' only the selected bank is treated, with option 'T' all banks
in the selected tree are treated. The KEEP sequences are named
with the hollerith Ids of the bank(trees) prefixed by mnemonics
like BOOK, LKOFF, DAOFF etc.
 
>COMMAND DATAOFFSETS
>GUIDANCE
Generate sequences of data word offsets for a selected bank (Option 'S')
or bank tree (Option 'T'). The names may optionally concatenated with
a max 8 character mnemonic like 'DO_'
With option 'Z' CHPFIX is appended, prefixed otherwise.
 
Option 'p' makes PARAMETER statements with offsets.
 
Option 'N' suppresses PARAMETER statements.
 
'2' allows to skip the first 2 char in data word name. (Allowed 1-9)
Option '='  (with '1')produces: IDATA4(I)=IQ(NOFF+4),
the sequence: '(I)=IQ(NOFF+' may be replaced by CHPFIX
 
Option 'C' produces COMMON/COBKID/...
 
Option 'H' produces CALL HBNAME(...
 
Example: a data word documented like this:
         *B.4 IDATA4     This is the 4th data word
         would produce (when putting CHPFIX to 'DO_':
         INTEGER DO_IDATA4
         PARAMETER (DO_IDATA4=4)
 
>PARAMETERS
CHBSBK 'Hollerith Id of selected bank' C
CHBSUP 'Hollerith Id of its up-bank' C
+
CHLIST 'File for listing' C D=' '
CHPFIX 'Prefix for data offset' C D=' '
CHOPTD 'Option, S single bank' C D='Tp'
IFW    'First word' I D=1
ILW    'Last word' I D=1000
>ACTION DZEDOF
 
>COMMAND LINKOFFSETS
>GUIDANCE
Generate sequences of link offsets for a selected bank (Option 'S')
or tree (Option 'T').
Exactly two times 4 characters are needed to define bank and up-bank.
The variable generated is named CHPFIX concatenated with the
bank Id.
Example: if a link to bank IDBK is described like this:
         *B.LINK
         ...
         *B.7 IDBK  Bank containing anything
         would produce:
         INTEGER LOIDBK
         PARAMETER (LOIDBK=7)
>PARAMETERS
CHBSBK 'Hollerith Id of selected bank' C
CHBSUP 'Hollerith Id of its up-bank' C
+
CHLIST 'File for listing' C D=' '
CHPFIX 'Prefix to BankId' C D='LO'
CHOPTL 'Option, S single bank' C D='T'
>ACTION DZELOF
 
>COMMAND LINKASSIGNMENT
>GUIDANCE
Generate sequences of link assignment statements for selected
bank (Option 'S') or tree (option 'T').
Exactly two times 4 characters are needed to define bank and up-bank.
The assumed linkoffset is named CHPFIX concatenated with the
bank Id.
Example: if a link to bank IDBK with Up-bank IDUP is described like:
         *B.UP IDUP
         *B.LINK
         ...
         *B.7  IDBK  Bank containing anything
         would produce:
         INTEGER LOIDBK
 
         LOIDBK=LQ(LOIDUP-7)
 
>PARAMETERS
CHBSBK 'Hollerith Id of selected bank' C
CHBSUP 'Hollerith Id of its up-bank' C
+
CHLIST 'File for listing' C D=' '
CHPFIX 'Prefix to BankId' C D='LO'
CHOPTL 'Option, S single bank' C D='T'
>ACTION DZELAS
 
>COMMAND BOOK
 
>GUIDANCE
Generate code to book a bank  (Option 'S') or bank tree (Option 'T').
With option 'B' (default) calls to MZBOOK are generated, with 'L'
calls to MZLIFT. For MZLIFT the bank parameters are put into arrays
MMIDBK which go into an own KEEP sequence.
The link to the bank 'BANK' is named LBANK, the uplink is assumed LUPBK,
if the up-bank is called UPBK.
For LBANK the declaration "INTEGER LBANK" is generated, for LUPBK it
is assumed to be done already. A call to MZFORM is generated if not
all data words are of the same type.
The link offset (JBIAS) is taken from the card *B.UP UPBK -JBIAS, if
JBIAS is given. If it is not given it is searched for in the
documentation of the Up-bank. If UPBK is "NONE" it is set to +1.
The following defaults are taken if the corresponding parameter is
not explicitly documented:
  IXDIV:  0 (i.e. div 2 in store 0)
  JBIAS:  no default
  NL:     0
  NS:     0
  ND:     0
  IOChar: no default, except if ND=0
  NZERO:  0
>PARAMETERS
CHBSBK 'Hollerith Id of selected bank' C
CHBSUP 'Hollerith Id of its up-bank' C
+
CHLIST 'File for listing' C D=' '
CHOPTB 'Option, S single bank' C D='TB'
>ACTION DZEBKK
 
>COMMAND LIFT
 
>GUIDANCE
Generate code to lift a bank  (Option 'S') or bank tree (Option 'T').
With option 'L' (default) calls to MZLIFT are generated, with 'B'
calls to MZBOOK. See also GUIDANCE for BOOK.
>PARAMETERS
CHBSBK 'Hollerith Id of selected bank' C
CHBSUP 'Hollerith Id of its up-bank' C
+
CHLIST 'File for listing' C D=' '
CHOPTB 'Option, S single bank' C D='TL'
>ACTION DZEBKK
 
