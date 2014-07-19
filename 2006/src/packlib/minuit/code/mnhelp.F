*
* $Id: mnhelp.F,v 1.2 1999/09/03 09:17:47 couet Exp $
*
* $Log: mnhelp.F,v $
* Revision 1.2  1999/09/03 09:17:47  couet
* - \Cind{} removed in the help of minuit. This was a Tex directive which very
*   likely has been forgotten during a Tex to f77 translation. This didn't
*   compile on RH6.
*
* Revision 1.1.1.1  1996/03/07 14:31:30  mclareni
* Minuit
*
*
#include "minuit/pilot.h"
      SUBROUTINE MNHELP(COMD,LOUT)
*.
*.         HELP routine for MINUIT interactive commands.
*.
*.      COMD ='*   '  prints a global help for all commands
*.      COMD =Command_name: print detailed help for one command.
*.          Note that at least 3 characters must be given for the command name.
*.
*.     Author: Rene Brun
*             comments extracted from the MINUIT documentation file.
*.
      CHARACTER*(*) COMD
      CHARACTER*3 CMD3
*.
*-- command name ASSUMED to be in upper case
*__________________________________________________________________
*--
*--  Global HELP: Summary of all commands
*--  ====================================
*--
      IF(COMD(1:1) .EQ. '*')THEN
         WRITE(LOUT,10000)
         WRITE(LOUT,10001)
         GO TO 99
      ENDIF
10000 FORMAT('   ==>List of MINUIT Interactive commands:',/,
     +' CLEar     Reset all parameter names and values undefined',/,
     +' CONtour   Make contour map of the user function',/,
     +' EXIT      Exit from Interactive Minuit',/,
     +' FIX       Cause parameter(s) to remain constant',/,
     +' HESse     Calculate the Hessian or error matrix.',/,
     +' IMPROVE   Search for a new minimum around current minimum',/,
     +' MIGrad    Minimize by the method of Migrad',/,
     +' MINImize  MIGRAD + SIMPLEX method if Migrad fails',/,
     +' MINOs     Exact (non-linear) parameter error analysis')
10001 FORMAT(' MNContour Calculate one MINOS function contour',/,
     +' PARameter Define or redefine new parameters and values',/,
     +' RELease   Make previously FIXed parameters variable again',/,
     +' REStore   Release last parameter fixed',/,
     +' SAVe      Save current parameter values on a file',/,
     +' SCAn      Scan the user function by varying parameters',/,
     +' SEEk      Minimize by the method of Monte Carlo',/,
     +' SET       Set various MINUIT constants or conditions',/,
     +' SHOw      Show values of current constants or conditions',/,
     +' SIMplex   Minimize by the method of Simplex')
*
      CMD3=COMD(1:3)
*__________________________________________________________________
*--
*--  Command CLEAR
*--  =============
*.
      IF(CMD3.EQ.'CLE')THEN
         WRITE(LOUT,10100)
         GO TO 99
      ENDIF
10100 FORMAT(' ***>CLEAR',/,
     +' Resets all parameter names and values to undefined.',/,
     +' Must normally be followed by a PARameters command or ',/,
     +' equivalent, in order to define parameter values.')
*__________________________________________________________________
*--
*--  Command CONTOUR
*--  ===============
*.
      IF(CMD3.EQ.'CON')THEN
         WRITE(LOUT,10200)
         GO TO 99
      ENDIF
10200 FORMAT(' ***>CONTOUR <par1>  <par2>  [devs]  [ngrid]',/,
     +' Instructs Minuit to trace contour lines of the user function',/,
     +' with respect to the two parameters whose external numbers',/,
     +' are <par1> and <par2>.',/,
     +' Other variable parameters of the function, if any, will have',/,
     +' their values fixed at the current values during the contour',/,
     +' tracing. The optional parameter [devs] (default value 2.)',/,
     +' gives the number of standard deviations in each parameter',/,
     +' which should lie entirely within the plotting area.',/,
     +' Optional parameter [ngrid] (default value 25 unless page',/,
     +' size is too small) determines the resolution of the plot,',/,
     +' i.e. the number of rows and columns of the grid at which the',/,
     +' function will be evaluated. [See also MNContour.]')
*__________________________________________________________________
*--
*--  Command END
*--  ===========
*.
      IF(CMD3.EQ.'END')THEN
         WRITE(LOUT,10300)
         GO TO 99
      ENDIF
10300 FORMAT(' ***>END',/,
     +' Signals the end of a data block (i.e., the end of a fit),',/,
     +' and implies that execution should continue, because another',/,
     +' Data Block follows. A Data Block is a set of Minuit data',/,
     +' consisting of',/,
     +'     (1) A Title,',/,
     +'     (2) One or more Parameter Definitions,',/,
     +'     (3) A blank line, and',/,
     +'     (4) A set of Minuit Commands.',/,
     +' The END command is used when more than one Data Block is to',/,
     +' be used with the same FCN function. It first causes Minuit',/,
     +' to issue a CALL FCN with IFLAG=3, in order to allow FCN to',/,
     +' perform any calculations associated with the final fitted',/,
     +' parameter values, unless a CALL FCN 3 command has already',/,
     +' been executed at the current FCN value.')
*__________________________________________________________________
*.
*--
*--  Command EXIT
*--  ============
      IF(CMD3 .EQ.'EXI')THEN
         WRITE(LOUT,10400)
         GO TO 99
      ENDIF
10400 FORMAT(' ***>EXIT',/,
     +' Signals the end of execution.',/,
     +' The EXIT command first causes Minuit to issue a CALL FCN',/,
     +' with IFLAG=3, to allow FCN to perform any calculations',/,
     +' associated with the final fitted parameter values, unless a',/,
     +' CALL FCN 3 command has already been executed.')
*__________________________________________________________________
*--
*--  Command FIX
*--  ===========
*.
      IF(CMD3.EQ.'FIX')THEN
         WRITE(LOUT,10500)
         GO TO 99
      ENDIF
10500 FORMAT(' ***>FIX} <parno> [parno] ... [parno]',/,
     +' Causes parameter(s) <parno> to be removed from the list of',/,
     +' variable parameters, and their value(s) will remain constant',/,
     +' during subsequent minimizations, etc., until another command',/,
     +' changes their value(s) or status.')
*__________________________________________________________________
*--
*--  Command HESSE
*--  =============
*.
      IF(CMD3.EQ.'HES')THEN
         WRITE(LOUT,10600)
         GO TO 99
      ENDIF
10600 FORMAT(' ***>HESse  [maxcalls]',/,
     +' Calculate, by finite differences, the Hessian or error matrix.',
     +/,'  That is, it calculates the full matrix of second derivatives'
     +,/,' of the function with respect to the currently variable',/,
     +' parameters, and inverts it, printing out the resulting error',/,
     +' matrix. The optional argument [maxcalls] specifies the',/,
     +' (approximate) maximum number of function calls after which',/,
     +' the calculation will be stopped.')
*__________________________________________________________________
*--
*--  Command IMPROVE
*--  ===============
*.
      IF(CMD3.EQ.'IMP')THEN
         WRITE(LOUT,10700)
         GO TO 99
      ENDIF
10700 FORMAT(' ***>IMPROVE  [maxcalls]',/,
     +' If a previous minimization has converged, and the current',/,
     +' values of the parameters therefore correspond to a local',/,
     +' minimum of the function, this command requests a search for',/,
     +' additional distinct local minima.',/,
     +' The optional argument [maxcalls] specifies the (approximate)',/,
     +' maximum number of function calls after which the calculation',/,
     +' will be stopped.')
*__________________________________________________________________
*--
*--  Command MIGRAD
*--  ==============
*.
      IF(CMD3.EQ.'MIG')THEN
         WRITE(LOUT,10800)
         GO TO 99
      ENDIF
10800 FORMAT(' ***>MIGrad  [maxcalls]  [tolerance]',/,
     +' Causes minimization of the function by the method of Migrad,',/,
     +' the most efficient and complete single method, recommended',/,
     +' for general functions (see also MINImize).',/,
     +' The minimization produces as a by-product the error matrix',/,
     +' of the parameters, which is usually reliable unless warning',/,
     +' messages are produced.',/,
     +' The optional argument [maxcalls] specifies the (approximate)',/,
     +' maximum number of function calls after which the calculation',/,
     +' will be stopped even if it has not yet converged.',/,
     +' The optional argument [tolerance] specifies required tolerance',
     +/,' on the function value at the minimum.',/,
     +' The default tolerance is 0.1, and the minimization will stop',/,
     +' when the estimated vertical distance to the minimum (EDM) is',/,
     +' less than 0.001*[tolerance]*UP (see [SET ERRordef]).')
*__________________________________________________________________
*--
*--  Command MINIMIZE
*--  ================
*.
      IF(COMD(1:4).EQ.'MINI')THEN
         WRITE(LOUT,10900)
         GO TO 99
      ENDIF
10900 FORMAT(' ***>MINImize  [maxcalls] [tolerance]',/,
     +' Causes minimization of the function by the method of Migrad,',/,
     +' as does the MIGrad command, but switches to the SIMplex method',
     +/,' if Migrad fails to converge. Arguments are as for MIGrad.',/,
     +' Note that command requires four characters to be unambiguous.')
*__________________________________________________________________
*--
*--  Command MINOS
*--  =============
*.
      IF(COMD(1:4).EQ.'MINO')THEN
         WRITE(LOUT,11000)
         GO TO 99
      ENDIF
11000 FORMAT(' ***>MINOs  [maxcalls]  [parno] [parno] ...',/,
     +' Causes a Minos error analysis to be performed on the parameters'
     +,/,' whose numbers [parno] are specified. If none are specified,',
     +/,' Minos errors are calculated for all variable parameters.',/,
     +' Minos errors may be expensive to calculate, but are very',/,
     +' reliable since they take account of non-linearities in the',/,
     +' problem as well as parameter correlations, and are in general',/
     +' asymmetric.',/,
     +' The optional argument [maxcalls] specifies the (approximate)',/,
     +' maximum number of function calls per parameter requested,',/,
     +' after which the calculation will stop for that parameter.')
*__________________________________________________________________
*--
*--  Command MNCONTOUR
*--  =================
*.
      IF(CMD3.EQ.'MNC')THEN
         WRITE(LOUT,11100)
         GO TO 99
      ENDIF
11100 FORMAT(' ***>MNContour  <par1> <par2> [npts]',/,
     +' Calculates one function contour of FCN with respect to',/,
     +' parameters par1 and par2, with FCN minimized always with',/,
     +' respect to all other NPAR-2 variable parameters (if any).',/,
     +' Minuit will try to find npts points on the contour (default 20)'
     +,/,' If only two parameters are variable at the time, it is not',
     +/,' necessary to specify their numbers. To calculate more than',/,
     +' one contour, it is necessary to SET ERRordef to the appropriate'
     +,/,' value and issue the MNContour command for each contour.')
*__________________________________________________________________
*--
*--  Command PARAMETER
*--  =================
*.
      IF(CMD3.EQ.'PAR')THEN
         WRITE(LOUT,11150)
         GO TO 99
      ENDIF
11150 FORMAT(' ***>PARameters',/,
     +' followed by one or more parameter definitions.',/,
     +' Parameter definitions are of the form:',/,
     +'   <number>  ''name''  <value>  <step>  [lolim] [uplim] ',/,
     +' for example:',/,
     +'  3  ''K width''  1.2   0.1' ,/,
     +' the last definition is followed by a blank line or a zero.')
*__________________________________________________________________
*--
*--  Command RELEASE
*--  ===============
*.
      IF(CMD3.EQ.'REL')THEN
         WRITE(LOUT,11200)
         GO TO 99
      ENDIF
11200 FORMAT(' ***>RELease  <parno> [parno] ... [parno]',/,
     +' If <parno> is the number of a previously variable parameter',/,
     +' which has been fixed by a command: FIX <parno>, then that',/,
     +' parameter will return to variable status.  Otherwise a warning'
     +,/,' message is printed and the command is ignored.',/,
     +' Note that this command operates only on parameters which were',/
     +' at one time variable and have been FIXed. It cannot make',/,
     +' constant parameters variable; that must be done by redefining',/
     +' the parameter with a PARameters command.')
*__________________________________________________________________
*--
*--  Command RESTORE
*--  ===============
*.
      IF(CMD3.EQ.'RES')THEN
         WRITE(LOUT,11300)
         GO TO 99
      ENDIF
11300 FORMAT(' ***>REStore  [code]',/,
     +' If no [code] is specified, this command restores all previously'
     +,/,' FIXed parameters to variable status. If [code]=1, then only',
     +/,' the last parameter FIXed is restored to variable status.',/,
     +' If code is neither zero nor one, the command is ignored.')
*__________________________________________________________________
*--
*--  Command RETURN
*--  ==============
*.
      IF(CMD3.EQ.'RET')THEN
         WRITE(LOUT,11400)
         GO TO 99
      ENDIF
11400 FORMAT(' ***>RETURN',/,
     +' Signals the end of a data block, and instructs Minuit to return'
     +,/,' to the program which called it. The RETurn command first',/,
     +' causes Minuit to CALL FCN with IFLAG=3, in order to allow FCN',/
     +,' to perform any calculations associated with the final fitted',/
     +,' parameter values, unless a CALL FCN 3 command has already been'
     +,/,' executed at the current FCN value.')
*__________________________________________________________________
*--
*--  Command SAVE
*--  ============
*.
      IF(CMD3.EQ.'SAV')THEN
         WRITE(LOUT,11500)
         GO TO 99
      ENDIF
11500 FORMAT(' ***>SAVe',/,
     +' Causes the current parameter values to be saved on a file in',/,
     +' such a format that they can be read in again as Minuit',/,
     +' parameter definitions. If the covariance matrix exists, it is',/
     +,' also output in such a format. The unit number is by default 7,'
     +,/,' or that specified by the user in his call to MINTIO or',/,
     +' MNINIT. The user is responsible for opening the file previous'
     +,/,' to issuing the [SAVe] command (except where this can be done'
     +,/,' interactively).')
*__________________________________________________________________
*--
*--  Command SCAN
*--  ============
*.
      IF(CMD3.EQ.'SCA')THEN
         WRITE(LOUT,11600)
         GO TO 99
      ENDIF
11600 FORMAT(' ***>SCAn  [parno]  [numpts] [from]  [to]',/,
     +' Scans the value of the user function by varying parameter',/,
     +' number [parno], leaving all other parameters fixed at the',/,
     +' current value. If [parno] is not specified, all variable',/,
     +' parameters are scanned in sequence.',/,
     +' The number of points [numpts] in the scan is 40 by default,',/,
     +' and cannot exceed 100. The range of the scan is by default',/,
     +' 2 standard deviations on each side of the current best value,',
     +/,' but can be specified as from [from] to [to].',/,
     +' After each scan, if a new minimum is found, the best parameter'
     +,/,' values are retained as start values for future scans or',/,
     +' minimizations. The curve resulting from each scan is plotted',/
     +,' on the output unit in order to show the approximate behaviour'
     +,/,' of the function.',/,
     +' This command is not intended for minimization, but is sometimes'
     +,/,' useful for debugging the user function or finding a',/,
     +' reasonable starting point.')
*__________________________________________________________________
*--
*--  Command SEEK
*--  ============
*.
      IF(CMD3.EQ.'SEE')THEN
         WRITE(LOUT,11700)
         GO TO 99
      ENDIF
11700 FORMAT(' ***>SEEk  [maxcalls]  [devs]',/,
     +' Causes a Monte Carlo minimization of the function, by choosing',
     +/,' random values of the variable parameters, chosen uniformly',/,
     +' over a hypercube centered at the current best value.',/,
     +' The region size is by default 3 standard deviations on each',/,
     +' side, but can be changed by specifying the value of [devs].')
*__________________________________________________________________
*--
*--  Command SET
*--  ===========
*.
      IF(CMD3.EQ.'SET')THEN
         WRITE(LOUT,11800)
         WRITE(LOUT,11801)
         WRITE(LOUT,11802)
         WRITE(LOUT,11803)
         WRITE(LOUT,11804)
         WRITE(LOUT,11805)
         WRITE(LOUT,11806)
         WRITE(LOUT,11807)
         WRITE(LOUT,11808)
         WRITE(LOUT,11809)
         WRITE(LOUT,11810)
         WRITE(LOUT,11811)
         WRITE(LOUT,11812)
         WRITE(LOUT,11813)
         WRITE(LOUT,11814)
         WRITE(LOUT,11815)
         WRITE(LOUT,11816)
         WRITE(LOUT,11817)
         GO TO 99
      ENDIF
11800 FORMAT(' ***>SET <option_name>',/,/,
     +'  SET BATch',/,
     +'    Informs Minuit that it is running in batch mode.',//,

     +'  SET EPSmachine  <accuracy>',/,
     +'    Informs Minuit that the relative floating point arithmetic',/
     +'    precision is <accuracy>. Minuit determines the nominal',/,
     +'    precision itself, but the SET EPSmachine command can be',/,
     +'    used to override Minuit own determination, when the user',/,
     +'    knows that the FCN function value is not calculated to',/,
     +'    the nominal machine accuracy. Typical values of <accuracy>',/
     +'    are between 10**-5 and 10**-14.')

11801 FORMAT(/,'  SET ERRordef  <up>',/,
     +'    Sets the value of UP (default value= 1.), defining parameter'
     +,/,'    errors. Minuit defines parameter errors as the change',/,
     +'    in parameter value required to change the function value',/,
     +'    by UP. Normally, for chisquared fits UP=1, and for negative'
     +,/,'    log likelihood, UP=0.5.')

11802 FORMAT(/,'   SET GRAdient  [force]',/,
     +'    Informs Minuit that the user function is prepared to',/,
     +'    calculate its own first derivatives and return their values'
     +,/,'    in the array GRAD when IFLAG=2 (see specs of FCN).',/,
     +'    If [force] is not specified, Minuit will calculate',/,
     +'    the FCN derivatives by finite differences at the current',/,
     +'    point and compare with the user calculation at that point,'
     +,/,'    accepting the user values only if they agree.',/,
     +'    If [force]=1, Minuit does not do its own derivative',/,
     +'    calculation, and uses the derivatives calculated in FCN.')

11803 FORMAT(/,'   SET INPut  [unitno]  [filename]',/,
     +'    Causes Minuit, in data-driven mode only, to read subsequent',
     +/,'    commands (or parameter definitions) from a different input'
     +,/,'    file. If no [unitno] is specified, reading reverts to the'
     +,/,'    previous input file, assuming that there was one.',/,
     +'    If [unitno] is specified, and that unit has not been opened,'
     +,/,'    then Minuit attempts to open the file [filename]} if a',/,
     +'    name is specified. If running in interactive mode and',/,
     +'    [filename] is not specified and [unitno] is not opened,',/,
     +'    Minuit prompts the user to enter a file name.',/,
     +'    If the word REWIND is added to the command (note:no blanks',/
     +'    between INPUT and REWIND), the file is rewound before',/,
     +'    reading. Note that this command is implemented in standard',/
     +'    Fortran 77 and the results may depend on the  system;',/,
     +'    for example, if a filename is given under VM/CMS, it must',/,
     +'    be preceeded by a slash.')

11804 FORMAT(/,'   SET INTeractive',/,
     +'    Informs Minuit that it is running interactively.')

11805 FORMAT(/,'   SET LIMits  [parno]  [lolim]  [uplim]',/,
     +'    Allows the user to change the limits on one or all',/,
     +'    parameters. If no arguments are specified, all limits are',/,
     +'    removed from all parameters. If [parno] alone is specified,',
     +/,'    limits are removed from parameter [parno].',/,
     +'    If all arguments are specified, then parameter [parno] will',
     +/,'    be bounded between [lolim] and [uplim].',/,
     +'    Limits can be specified in either order, Minuit will take',/,
     +'    the smaller as [lolim] and the larger as [uplim].',/,
     +'    However, if [lolim] is equal to [uplim], an error condition',
     +/,'    results.')

11806 FORMAT(/,'   SET LINesperpage',/,
     +'     Sets the number of lines for one page of output.',/,
     +'     Default value is 24 for interactive mode')

11807 FORMAT(/,'   SET NOGradient',/,
     +'    The inverse of SET GRAdient, instructs Minuit not to',
     +/,'    use the first derivatives calculated by the user in FCN.')

11808 FORMAT(/,'   SET NOWarnings',/,
     +'    Supresses Minuit warning messages.')

11809 FORMAT(/,'   SET OUTputfile  <unitno>',/,
     +'    Instructs Minuit to write further output to unit <unitno>.')

11810 FORMAT(/,'   SET PAGethrow  <integer>',/,
     +'    Sets the carriage control character for ``new page'' to',/,
     +'    <integer>. Thus the value 1 produces a new page, and 0',/,
     +'    produces a blank line, on some devices (see TOPofpage)')


11811 FORMAT(/,'   SET PARameter  <parno>  <value>',/,
     +'    Sets the value of parameter <parno> to <value>.',/,
     +'    The parameter in question may be variable, fixed, or',/,
     +'    constant, but must be defined.')

11812 FORMAT(/,'   SET PRIntout  <level>',/,
     +'    Sets the print level, determining how much output will be',/,
     +'    produced. Allowed values and their meanings are displayed',/,
     +'    after a SHOw PRInt command, and are currently <level>=:',/,
     +'      [-1]  no output except from SHOW commands',/,
     +'       [0]  minimum output',/,
     +'       [1]  default value, normal output',/,
     +'       [2]  additional output giving intermediate results.',/,
     +'       [3]  maximum output, showing progress of minimizations.',/
     +'    Note: See also the SET WARnings command.')

11813 FORMAT(/,'   SET RANdomgenerator  <seed>',/,
     +'    Sets the seed of the random number generator used in SEEk.',/
     +'    This can be any integer between 10000 and 900000000, for',/,
     +'    example one which was output from a SHOw RANdom command of',/
     +'    a previous run.')

11814 FORMAT(/,'   SET STRategy  <level>',/,
     +'    Sets the strategy to be used in calculating first and second'
     +,/,'    derivatives and in certain minimization methods.',/,
     +'    In general, low values of <level> mean fewer function calls',
     +/,'    and high values mean more reliable minimization.',/,
     +'    Currently allowed values are 0, 1 (default), and 2.')

11815 FORMAT(/,'   SET TITle',/,
     +'    Informs Minuit that the next input line is to be considered',
     +/,'    the (new) title for this task or sub-task.  This is for',/,
     +'    the convenience of the user in reading his output.')

11816 FORMAT(/,'   SET WARnings',/,
     +'    Instructs Minuit to output warning messages when suspicious',
     +/,'    conditions arise which may indicate unreliable results.',/
     +'    This is the default.')

11817 FORMAT(/,'    SET WIDthpage',/,
     +'    Informs Minuit of the output page width.',/,
     +'    Default values are 80 for interactive jobs')
*__________________________________________________________________
*--
*--  Command SHOW
*--  ============
*.
      IF(CMD3.EQ.'SHO')THEN
         WRITE(LOUT,11900)
         WRITE(LOUT,11901)
         WRITE(LOUT,11902)
         WRITE(LOUT,11903)
         WRITE(LOUT,11904)
         GO TO 99
      ENDIF
11900 FORMAT(' ***>SHOw  <option_name>',/,
     +'  All SET XXXX commands have a corresponding SHOw XXXX command.',
     +/,'  In addition, the SHOw commands listed starting here have no',
     +/,'  corresponding SET command for obvious reasons.')

11901 FORMAT(/,'   SHOw CORrelations',/,
     +'    Calculates and prints the parameter correlations from the',/,
     +'    error matrix.')

11902 FORMAT(/,'   SHOw COVariance',/,
     +'    Prints the (external) covariance (error) matrix.')

11903 FORMAT(/,'   SHOw EIGenvalues',/,
     +'    Calculates and prints the eigenvalues of the covariance',/,
     +'    matrix.')

11904 FORMAT(/,'   SHOw FCNvalue',/,
     +'    Prints the current value of FCN.')
*__________________________________________________________________
*--
*--  Command SIMPLEX
*--  ===============
*.
      IF(CMD3.EQ.'SIM')THEN
         WRITE(LOUT,12000)
         GO TO 99
      ENDIF
12000 FORMAT(' ***>SIMplex  [maxcalls]  [tolerance]',/,
     +' Performs a function minimization using the simplex method of',/
     +' Nelder and Mead. Minimization terminates either when the',/,
     +' function has been called (approximately) [maxcalls] times,',/,
     +' or when the estimated vertical distance to minimum (EDM) is',/,
     +' less than [tolerance].',/,
     +' The default value of [tolerance] is 0.1*UP(see SET ERRordef).')
*__________________________________________________________________
*--
*--  Command STANDARD
*--  ================
*.
      IF(CMD3.EQ.'STA')THEN
         WRITE(LOUT,12100)
         GO TO 99
      ENDIF
12100 FORMAT(' ***>STAndard',/,
     +' Causes Minuit to execute the Fortran instruction CALL STAND',/,
     +' where STAND is a subroutine supplied by the user.')
*__________________________________________________________________
*--
*--  Command STOP
*--  ============
*.
      IF(CMD3.EQ.'STO')THEN
         WRITE(LOUT,12200)
         GO TO 99
      ENDIF
12200 FORMAT(' ***>STOP',/,
     +' Same as EXIT.')
*__________________________________________________________________
*--
*--  Command TOPOFPAGE
*--  =================
*.
      IF(CMD3.EQ.'TOP')THEN
         WRITE(LOUT,12300)
         GO TO 99
      ENDIF
12300 FORMAT(' ***>TOPofpage',/,
     +' Causes Minuit to write the character specified in a',/,
     +' SET PAGethrow command (default = 1) to column 1 of the output'
     +,/,' file, which may or may not position your output medium to',
     +/,' the top of a page depending on the device and system.')
*__________________________________________________________________
*
      WRITE(LOUT,13000)
13000 FORMAT(' Unknown MINUIT command. Type HELP for list of commands.')
*
  99  RETURN
      END
