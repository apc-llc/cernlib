*
* $Id: minuit.F,v 1.1.1.1 1996/03/07 14:31:28 mclareni Exp $
*
* $Log: minuit.F,v $
* Revision 1.1.1.1  1996/03/07 14:31:28  mclareni
* Minuit
*
*
#include "minuit/pilot.h"
      SUBROUTINE MINUIT(FCN,FUTIL)
#include "minuit/d506dp.inc"
#include "minuit/d506cm.inc"
C
C  CPNAM   Parameter name (10 characters)
C  U       External (visible to user in FCN) value of parameter
C  ALIM, BLIM Lower and upper parameter limits. If both zero, no limits.
C  ERP,ERN Positive and negative MINOS errors, if calculated.
C  WERR    External parameter error (standard deviation, defined by UP)
C  GLOBCC  Global Correlation Coefficient
C  NVARL   =-1 if parameter undefined,      =0 if constant,
C          = 1 if variable without limits,  =4 if variable with limits
C   (Note that if parameter has been fixed, NVARL=1 or =4, and NIOFEX=0)
C  NIOFEX  Internal parameter number, or zero if not currently variable
C  NEXOFI  External parameter number for currently variable parameters
C  X, XT   Internal parameter values (X are sometimes saved in XT)
C  DIRIN   (Internal) step sizes for current step
C  variables with names ending in ..S are saved values for fixed params
C  VHMAT   (Internal) error matrix stored as Half MATrix, since
C                it is symmetric
C  VTHMAT  VHMAT is sometimes saved in VTHMAT, especially in MNMNOT
C
C  ISW definitions:
C      ISW(1) =0 normally, =1 means CALL LIMIT EXCEEDED
C      ISW(2) =0 means no error matrix
C             =1 means only approximate error matrix
C             =2 means full error matrix, but forced pos-def.
C             =3 means good normal full error matrix exists
C      ISW(3) =0 if Minuit is calculating the first derivatives
C             =1 if first derivatives calculated inside FCN
C      ISW(4) =-1 if most recent minimization did not converge.
C             = 0 if problem redefined since most recent minimization.
C             =+1 if most recent minimization did converge.
C      ISW(5) is the PRInt level.  See SHO PRIntlevel
C      ISW(6) = 0 for batch mode, =1 for interactive mode
C                      =-1 for originally interactive temporarily batch
C
C  LWARN is true if warning messges are to be put out (default=true)
C            SET WARN turns it on, set NOWarn turns it off
C  LREPOR is true if exceptional conditions are put out (default=false)
C            SET DEBUG turns it on, SET NODebug turns it off
C  LIMSET is true if a parameter is up against limits (for MINOS)
C  LNOLIM is true if there are no limits on any parameters (not yet used)
C  LNEWMN is true if the previous process has unexpectedly improved FCN
C  LPHEAD is true if a heading should be put out for the next parameter
C        definition, false if a parameter has just been defined
C
      EXTERNAL FCN,FUTIL
      CHARACTER*40 CWHYXT
      DATA CWHYXT/'FOR UNKNOWN REASONS                     '/
      DATA JSYSRD,JSYSWR,JSYSSA/5,6,7/
C                                 . . . . . . . . . . initialize minuit
      WRITE (JSYSWR,'(1X,75(1H*))')
      CALL MNINIT (JSYSRD,JSYSWR,JSYSSA)
C                                      . . . . initialize new data block
  100 CONTINUE
      WRITE (ISYSWR,'(1X,75(1H*))')
      NBLOCK = NBLOCK + 1
      WRITE (ISYSWR,'(26X,A,I4)')  'MINUIT DATA BLOCK NO.',NBLOCK
      WRITE (ISYSWR,'(1X,75(1H*))')
C               . . . . . . . . . . .   set parameter lists to undefined
      CALL MNCLER
C                                             . . . . . . . . read title
      CALL MNREAD(FCN,1,IFLGUT,FUTIL)
      IF (IFLGUT .EQ. 2)  GO TO 500
      IF (IFLGUT .EQ. 3)  GO TO 600
C                                        . . . . . . . . read parameters
      CALL MNREAD(FCN,2,IFLGUT,FUTIL)
      IF (IFLGUT .EQ. 2)  GO TO 500
      IF (IFLGUT .EQ. 3)  GO TO 600
      IF (IFLGUT .EQ. 4)  GO TO 700
C                              . . . . . . verify FCN not time-dependent
      WRITE (ISYSWR,'(/A,A)') ' MINUIT: FIRST CALL TO USER FUNCTION,',
     +    ' WITH IFLAG=1'
      NPARX = NPAR
      CALL MNINEX(X)
      FZERO = UNDEFI
      CALL FCN(NPARX,GIN,FZERO,U,1,FUTIL)
      FIRST = UNDEFI
      CALL FCN(NPARX,GIN,FIRST,U,4,FUTIL)
      NFCN = 2
      IF (FZERO.EQ.UNDEFI .AND. FIRST.EQ.UNDEFI)  THEN
          CWHYXT = 'BY ERROR IN USER FUNCTION.  '
          WRITE (ISYSWR,'(/A,A/)') ' USER HAS NOT CALCULATED FUNCTION',
     +    ' VALUE WHEN IFLAG=1 OR 4'
          GO TO 800
      ENDIF
      AMIN = FIRST
      IF (FIRST .EQ. UNDEFI) AMIN=FZERO
      CALL MNPRIN(1,AMIN)
      NFCN = 2
      IF (FIRST .EQ. FZERO)  GO TO 300
      FNEW = 0.0
      CALL FCN(NPARX,GIN,FNEW,U,4,FUTIL)
      IF  (FNEW .NE. AMIN) WRITE (ISYSWR,280) AMIN, FNEW
  280 FORMAT (/' MINUIT WARNING: PROBABLE ERROR IN USER FUNCTION.'/
     +         ' FOR FIXED VALUES OF PARAMETERS, FCN IS TIME-DEPENDENT'/
     +         ' F =',E22.14,' FOR FIRST CALL'/
     +         ' F =',E22.14,' FOR SECOND CALL.'/)
      NFCN = 3
  300 FVAL3 = 2.0*AMIN+1.0
C                                   . . . . . . . . . . . read commands
      CALL MNREAD(FCN,3,IFLGUT,FUTIL)
      IF (IFLGUT .EQ. 2)  GO TO 500
      IF (IFLGUT .EQ. 3)  GO TO 600
      IF (IFLGUT .EQ. 4)  GO TO 700
      CWHYXT = 'BY MINUIT COMMAND: '//CWORD
      IF (INDEX(CWORD,'STOP').GT. 0)  GO TO 800
      IF (INDEX(CWORD,'EXI') .GT. 0)  GO TO 800
      IF (INDEX(CWORD,'RET') .EQ. 0)  GO TO 100
      CWHYXT = 'AND RETURNS TO USER PROGRAM.    '
      WRITE (ISYSWR,'(A,A)')  ' ..........MINUIT TERMINATED ',CWHYXT
      RETURN
C                                           . . . . . . stop conditions
  500 CONTINUE
      CWHYXT = 'BY END-OF-DATA ON PRIMARY INPUT FILE.   '
      GO TO 800
  600 CONTINUE
      CWHYXT = 'BY UNRECOVERABLE READ ERROR ON INPUT.   '
      GO TO 800
  700 CONTINUE
      CWHYXT = ': FATAL ERROR IN PARAMETER DEFINITIONS. '
  800 WRITE (ISYSWR,'(A,A)')  ' ..........MINUIT TERMINATED ',CWHYXT
      STOP
C
C  ......................entry to set unit numbers  - - - - - - - - - -
      ENTRY MINTIO(I1,I2,I3)
      JSYSRD = I1
      JSYSWR = I2
      JSYSSA = I3
      RETURN
      END
