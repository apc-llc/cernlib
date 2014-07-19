*
* $Id: mnexcm.F,v 1.2 1996/03/15 18:02:45 james Exp $
*
* $Log: mnexcm.F,v $
* Revision 1.2  1996/03/15 18:02:45  james
*     Modified Files:
* mnderi.F eliminate possible division by zero
* mnexcm.F suppress print on STOP when print flag=-1
*          set FVAL3 to flag if FCN already called with IFLAG=3
* mninit.F set version 96.03
* mnlims.F remove arguments, not needed
* mnmigr.F VLEN -> LENV in debug print statement
* mnparm.F move call to MNRSET to after NPAR redefined, to zero all
* mnpsdf.F eliminate possible division by zero
* mnscan.F suppress printout when print flag =-1
* mnset.F  remove arguments in call to MNLIMS
* mnsimp.F fix CSTATU so status is PROGRESS only if new minimum
* mnvert.F eliminate possible division by zero
*
* Revision 1.1.1.1  1996/03/07 14:31:29  mclareni
* Minuit
*
*
#include "minuit/pilot.h"
      SUBROUTINE MNEXCM(FCN,COMAND,PLIST,LLIST,IERFLG,FUTIL)
#include "minuit/d506dp.inc"
CC        Interprets a command and takes appropriate action,
CC        either directly by skipping to the corresponding code in
CC        MNEXCM, or by setting up a call to a subroutine
CC
#include "minuit/d506cm.inc"
      EXTERNAL FCN,FUTIL
      CHARACTER*(*) COMAND
C   Cannot say DIMENSION PLIST(LLIST) since LLIST can be =0.
      DIMENSION PLIST(*)
      PARAMETER (MXPT=101)
      DIMENSION XPTU(MXPT), YPTU(MXPT)
C  alphabetical order of command names!
      CHARACTER*10 CNAME(40), CNEWAY, CHWHY*18, C26*30, CVBLNK*2
      LOGICAL LTOFIX, LFIXED, LFREED
C
      CHARACTER COMD*4
      CHARACTER CLOWER*26, CUPPER*26
      DATA CLOWER/'abcdefghijklmnopqrstuvwxyz'/
      DATA CUPPER/'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
C
C  recognized MINUIT commands:
      DATA CNAME( 1) / 'MINImize  ' /
      DATA CNAME( 2) / 'SEEk      ' /
      DATA CNAME( 3) / 'SIMplex   ' /
      DATA CNAME( 4) / 'MIGrad    ' /
      DATA CNAME( 5) / 'MINOs     ' /
      DATA CNAME( 6) / 'SET xxx   ' /
      DATA CNAME( 7) / 'SHOw xxx  ' /
      DATA CNAME( 8) / 'TOP of pag' /
      DATA CNAME( 9) / 'FIX       ' /
      DATA CNAME(10) / 'REStore   ' /
      DATA CNAME(11) / 'RELease   ' /
      DATA CNAME(12) / 'SCAn      ' /
      DATA CNAME(13) / 'CONtour   ' /
      DATA CNAME(14) / 'HESse     ' /
      DATA CNAME(15) / 'SAVe      ' /
      DATA CNAME(16) / 'IMProve   ' /
      DATA CNAME(17) / 'CALl fcn  ' /
      DATA CNAME(18) / 'STAndard  ' /
      DATA CNAME(19) / 'END       ' /
      DATA CNAME(20) / 'EXIt      ' /
      DATA CNAME(21) / 'RETurn    ' /
      DATA CNAME(22) / 'CLEar     ' /
      DATA CNAME(23) / 'HELP      ' /
      DATA CNAME(24) / 'MNContour ' /
      DATA CNAME(25) / 'STOp      ' /
      DATA CNAME(26) / 'JUMp      ' /
      DATA CNAME(27) / '          ' /
      DATA CNAME(28) / '          ' /
      DATA CNAME(29) / '          ' /
      DATA CNAME(30) / '          ' /
      DATA CNAME(31) / '          ' /
      DATA CNAME(32) / '          ' /
      DATA CNAME(33) / '          ' /
C  obsolete commands:
      DATA CNAME(34) / 'COVARIANCE' /
      DATA CNAME(35) / 'PRINTOUT  ' /
      DATA CNAME(36) / 'GRADIENT  ' /
      DATA CNAME(37) / 'MATOUT    ' /
      DATA CNAME(38) / 'ERROR DEF ' /
      DATA CNAME(39) / 'LIMITS    ' /
      DATA CNAME(40) / 'PUNCH     ' /
      DATA NNTOT/40/
C      IERFLG is now (94.5) defined the same as ICONDN in MNCOMD
CC            = 0: command executed normally
CC              1: command is blank, ignored
CC              2: command line unreadable, ignored
CC              3: unknown command, ignored
CC              4: abnormal termination (e.g., MIGRAD not converged)
CC              9: reserved
CC             10: END command
CC             11: EXIT or STOP command
CC             12: RETURN command
      LK = LEN(COMAND)
      IF (LK .GT. MAXCWD) LK=MAXCWD
      CWORD = COMAND(1:LK)
C              get upper case
      DO 16 ICOL= 1, LK
        DO 15 LET= 1, 26
        IF (CWORD(ICOL:ICOL) .EQ. CLOWER(LET:LET))
     +      CWORD(ICOL:ICOL) = CUPPER(LET:LET)
   15   CONTINUE
   16 CONTINUE
C           Copy the first MAXP arguments into COMMON (WORD7), making
C           sure that WORD7(1)=0. if LLIST=0
      DO 20 IW= 1, MAXP
      WORD7(IW) = ZERO
      IF (IW .LE. LLIST) WORD7(IW) = PLIST(IW)
   20 CONTINUE
      ICOMND = ICOMND + 1
      NFCNLC = NFCN
      IF (CWORD(1:7).NE.'SET PRI' .OR. WORD7(1).GE.0.)  THEN
        IF (ISW(5) .GE. 0) THEN
         LNOW = LLIST
         IF (LNOW .GT. 4)  LNOW=4
         WRITE (ISYSWR,25) ICOMND,CWORD(1:LK),(PLIST(I),I=1,LNOW)
   25    FORMAT (1H ,10(1H*)/' **',I5,' **',A,4G12.4)
         INONDE = 0
         IF (LLIST .GT. LNOW) THEN
           KLL = LLIST
           IF (LLIST .GT. MAXP) THEN
              INONDE = 1
              KLL = MAXP
           ENDIF
           WRITE (CVBLNK,'(I2)') LK
           C26 = '(11H **********,'//CVBLNK//'X,4G12.4)'
           WRITE (ISYSWR,C26) (PLIST(I),I=LNOW+1,KLL)
         ENDIF
         WRITE (ISYSWR, '(1H ,10(1H*))' )
         IF (INONDE .GT. 0)  WRITE (ISYSWR, '(1H ,10(1H*),A,I3,A)')
     +        '  ERROR: ABOVE CALL TO MNEXCM TRIED TO PASS MORE THAN ',
     +        MAXP,' PARAMETERS.'
        ENDIF
      ENDIF
      NFCNMX = WORD7(1)
      IF (NFCNMX .LE. 0)  NFCNMX = 200 + 100*NPAR + 5*NPAR**2
      EPSI = WORD7(2)
      IF (EPSI .LE. ZERO)  EPSI = 0.1 * UP
      LNEWMN = .FALSE.
      LPHEAD = .TRUE.
      ISW(1) = 0
      IERFLG = 0
C                look for command in list CNAME . . . . . . . . . .
      DO 80 I= 1, NNTOT
      IF (CWORD(1:3) .EQ. CNAME(I)(1:3))  GO TO 90
   80 CONTINUE
      WRITE (ISYSWR,'(11X,''UNKNOWN COMMAND IGNORED:'',A)') COMAND
      IERFLG = 3
      GO TO 5000
C                normal case: recognized MINUIT command . . . . . . .
   90 CONTINUE
      IF (CWORD(1:4) .EQ. 'MINO') I = 5
      IF (I.NE.6 .AND. I.NE.7 .AND. I.NE.8 .AND. I.NE.23)  THEN
         CFROM = CNAME(I)
         NFCNFR = NFCN
      ENDIF
C              1    2    3    4    5    6    7    8    9   10
      GO TO ( 400, 200, 300, 400, 500, 700, 700, 800, 900,1000,
     1       1100,1200,1300,1400,1500,1600,1700,1800,1900,1900,
     2       1900,2200,2300,2400,1900,2600,3300,3300,3300,3300,
     3       3300,3300,3300,3400,3500,3600,3700,3800,3900,4000) , I
C                                        . . . . . . . . . . seek
  200 CALL MNSEEK(FCN,FUTIL)
      GO TO 5000
C                                        . . . . . . . . . . simplex
  300 CALL MNSIMP(FCN,FUTIL)
      IF (ISW(4) .LT. 1)  IERFLG = 4
      GO TO 5000
C                                        . . . . . . migrad, minimize
  400 CONTINUE
      NF = NFCN
      APSI = EPSI
      CALL MNMIGR(FCN,FUTIL)
      CALL MNWERR
      IF (ISW(4) .GE. 1)         GO TO 5000
        IERFLG = 4
      IF (ISW(1) .EQ. 1)         GO TO 5000
      IF (CWORD(1:3) .EQ. 'MIG') GO TO 5000
      NFCNMX = NFCNMX + NF - NFCN
      NF = NFCN
      CALL MNSIMP(FCN,FUTIL)
      IF (ISW(1) .EQ. 1)  GO TO 5000
      NFCNMX = NFCNMX + NF - NFCN
      CALL MNMIGR(FCN,FUTIL)
         IF (ISW(4) .GE. 1)  IERFLG = 0
      CALL MNWERR
      GO TO 5000
C                                        . . . . . . . . . . minos
  500 CONTINUE
      NSUPER = NFCN + 2*(NPAR+1)*NFCNMX
C          possible loop over new minima
      EPSI = 0.1 * UP
  510 CONTINUE
      CALL MNCUVE(FCN,FUTIL)
      CALL MNMNOS(FCN,FUTIL)
      IF (.NOT. LNEWMN)  GO TO 5000
      CALL MNRSET(0)
      CALL MNMIGR(FCN,FUTIL)
      CALL MNWERR
      IF (NFCN .LT. NSUPER)  GO TO 510
      WRITE (ISYSWR,'(/'' TOO MANY FUNCTION CALLS. MINOS GIVES UP''/)')
      IERFLG = 4
      GO TO 5000
C                                        . . . . . . . . . .set, show
  700 CALL MNSET(FCN,FUTIL)
      GO TO 5000
C                                        . . . . . . . . . . top of page
  800 CONTINUE
      WRITE (ISYSWR,'(1H1)')
      GO TO 5000
C                                        . . . . . . . . . . fix
  900 LTOFIX = .TRUE.
C                                        . . (also release) ....
  901 CONTINUE
      LFREED = .FALSE.
      LFIXED = .FALSE.
      IF (LLIST .EQ. 0)  THEN
         WRITE (ISYSWR,'(A,A)') CWORD,':  NO PARAMETERS REQUESTED '
         GO TO 5000
      ENDIF
      DO 950 ILIST= 1, LLIST
      IEXT = PLIST(ILIST)
      CHWHY = ' IS UNDEFINED.'
      IF (IEXT .LE. 0)         GO TO 930
      IF (IEXT .GT. NU)        GO TO 930
      IF (NVARL(IEXT) .LT. 0)  GO TO 930
      CHWHY = ' IS CONSTANT.  '
      IF (NVARL(IEXT) .EQ. 0)  GO TO 930
      IINT = NIOFEX(IEXT)
      IF (LTOFIX) THEN
         CHWHY = ' ALREADY FIXED.'
         IF (IINT .EQ. 0)      GO TO 930
         CALL MNFIXP(IINT,IERR)
         IF (IERR .EQ. 0) THEN
            LFIXED = .TRUE.
         ELSE
            IERFLG = 4
         ENDIF
      ELSE
         CHWHY = ' ALREADY VARIABLE.'
         IF (IINT .GT. 0)      GO TO 930
         KRL = -IABS(IEXT)
         CALL MNFREE(KRL)
         LFREED = .TRUE.
      ENDIF
      GO TO 950
  930 WRITE (ISYSWR,'(A,I4,A,A)') ' PARAMETER',IEXT,CHWHY,' IGNORED.'
  950 CONTINUE
      IF (LFREED .OR. LFIXED)  CALL MNRSET(0)
      IF (LFREED)  THEN
          ISW(2) = 0
          DCOVAR = 1.
          EDM = BIGEDM
          ISW(4) = 0
      ENDIF
      CALL MNWERR
      IF (ISW(5) .GT. 1)  CALL MNPRIN(5,AMIN)
      GO TO 5000
C                                        . . . . . . . . . . restore
 1000 IT = WORD7(1)
      IF (IT.GT.1 .OR. IT.LT.0)  GO TO 1005
      LFREED = (NPFIX .GT. 0)
      CALL MNFREE(IT)
      IF (LFREED) THEN
         CALL MNRSET(0)
         ISW(2) = 0
         DCOVAR = 1.
         EDM = BIGEDM
      ENDIF
      GO TO 5000
 1005 WRITE (ISYSWR,'(A,I4)') ' IGNORED.  UNKNOWN ARGUMENT:',IT
      IERFLG = 3
      GO TO 5000
C                                        . . . . . . . . . . release
 1100 LTOFIX = .FALSE.
      GO TO 901
C                                       . . . . . . . . . . scan . . .
 1200 CONTINUE
      IEXT = WORD7(1)
      IF (IEXT .LE. 0)  GO TO 1210
      IT2 = 0
      IF (IEXT .LE. NU)  IT2 = NIOFEX(IEXT)
      IF (IT2 .LE. 0)  GO TO 1250
 1210 CALL MNSCAN(FCN,FUTIL)
      GO TO 5000
 1250 WRITE (ISYSWR,'(A,I4,A)') ' PARAMETER',IEXT,' NOT VARIABLE.'
      IERFLG = 3
      GO TO 5000
C                                        . . . . . . . . . . contour
 1300 CONTINUE
      KE1 = WORD7(1)
      KE2 = WORD7(2)
      IF (KE1 .EQ. 0)  THEN
         IF (NPAR .EQ. 2)  THEN
            KE1 = NEXOFI(1)
            KE2 = NEXOFI(2)
         ELSE
            WRITE (ISYSWR,'(A,A)') CWORD,':  NO PARAMETERS REQUESTED '
            IERFLG = 3
            GO TO 5000
         ENDIF
      ENDIF
      NFCNMX = 1000
      CALL MNCNTR(FCN,KE1,KE2,IERRF,FUTIL)
      IF (IERRF .GT. 0)  IERFLG = 3
      GO TO 5000
C                                        . . . . . . . . . . hesse
 1400 CONTINUE
      CALL MNHESS(FCN,FUTIL)
      CALL MNWERR
      IF (ISW(5) .GE. 0)  CALL MNPRIN(2, AMIN)
      IF (ISW(5) .GE. 1)  CALL MNMATU(1)
      GO TO 5000
C                                        . . . . . . . . . . save
 1500 CONTINUE
      CALL MNSAVE
      GO TO 5000
C                                        . . . . . . . . . . improve
 1600 CONTINUE
      CALL MNCUVE(FCN,FUTIL)
      CALL MNIMPR(FCN,FUTIL)
      IF (LNEWMN)  GO TO 400
      IERFLG = 4
      GO TO 5000
C                                        . . . . . . . . . . call fcn
 1700 IFLAG = WORD7(1)
      NPARX = NPAR
      F = UNDEFI
      CALL FCN(NPARX,GIN,F,U,IFLAG,FUTIL)
      NFCN = NFCN + 1
      NOWPRT = 0
      IF (F .NE. UNDEFI)  THEN
         IF (AMIN .EQ. UNDEFI)  THEN
             AMIN = F
             NOWPRT = 1
         ELSE IF (F .LT. AMIN)  THEN
             AMIN = F
             NOWPRT = 1
         ENDIF
         IF (ISW(5).GE.0 .AND. IFLAG.LE.5 .AND. NOWPRT.EQ.1)
     +          CALL MNPRIN(5,AMIN)
         IF (IFLAG .EQ. 3)  FVAL3=F
      ENDIF
      IF (IFLAG .GT. 5)  CALL MNRSET(1)
      GO TO 5000
C                                        . . . . . . . . . . standard
 1800 CALL STAND
      GO TO 5000
C                                       . . . return, stop, end, exit
 1900 IT = WORD7(1)
      IF (FVAL3 .NE. AMIN .AND. IT .EQ. 0)  THEN
        IFLAG = 3
        IF (ISW(5) .GE. 0)
     +WRITE (ISYSWR,'(/A/)') ' CALL TO USER FUNCTION WITH IFLAG = 3'
        NPARX = NPAR
        CALL FCN(NPARX,GIN,F,U,IFLAG,FUTIL)
        NFCN = NFCN + 1
        FVAL3 = F
      ENDIF
      IERFLG = 11
      IF (CWORD(1:3) .EQ. 'END')  IERFLG = 10
      IF (CWORD(1:3) .EQ. 'RET')  IERFLG = 12
      GO TO 5000
C                                        . . . . . . . . . . clear
 2200 CONTINUE
      CALL MNCLER
      IF (ISW(5) .GE. 1)  WRITE (ISYSWR,'(A)')
     + ' MINUIT MEMORY CLEARED. NO PARAMETERS NOW DEFINED.'
      GO TO 5000
C                                        . . . . . . . . . . help
 2300 CONTINUE
CCCC      IF (INDEX(CWORD,'SHO') .GT. 0)  GO TO 700
CCCC      IF (INDEX(CWORD,'SET') .GT. 0)  GO TO 700
      KCOL = 0
      DO 2310 ICOL= 5,LK
        IF (CWORD(ICOL:ICOL) .EQ. ' ') GO TO 2310
        KCOL = ICOL
        GO TO 2320
 2310 CONTINUE
 2320 CONTINUE
      IF (KCOL .EQ. 0)  THEN
         COMD = '*   '
      ELSE
         COMD = CWORD(KCOL:LK)
      ENDIF
      CALL MNHELP(COMD,ISYSWR)
      GO TO 5000
C                                       . . . . . . . . . . MNContour
 2400 CONTINUE
      EPSI = 0.05 * UP
      KE1 = WORD7(1)
      KE2 = WORD7(2)
      IF (KE1.EQ.0 .AND. NPAR.EQ.2) THEN
         KE1 = NEXOFI(1)
         KE2 = NEXOFI(2)
         ENDIF
      NPTU = WORD7(3)
      IF (NPTU .LE. 0)  NPTU=20
      IF (NPTU .GT. MXPT)  NPTU = MXPT
      NFCNMX =  100*(NPTU+5)*(NPAR+1)
      CALL MNCONT(FCN,KE1,KE2,NPTU,XPTU,YPTU,IERRF,FUTIL)
      IF (IERRF .LT. NPTU) IERFLG = 4
      IF (IERRF .EQ. -1)   IERFLG = 3
      GO TO 5000
C                                      . . . . . . . . . . jump
 2600 CONTINUE
      STEP = WORD7(1)
      IF (STEP .LE. ZERO)  STEP = 2.
      RNO = 0.
      IZERO = 0
      DO 2620 I= 1, NPAR
        CALL MNRN15(RNO,IZERO)
        RNO = 2.0*RNO - 1.0
 2620   X(I) = X(I) + RNO*STEP*WERR(I)
      CALL MNINEX(X)
      CALL MNAMIN(FCN,FUTIL)
      CALL MNRSET(0)
      GO TO 5000
C                                      . . . . . . . . . . blank line
 3300 CONTINUE
      WRITE (ISYSWR,'(10X,A)') ' BLANK COMMAND IGNORED.'
      IERFLG = 1
      GO TO 5000
C  . . . . . . . . obsolete commands     . . . . . . . . . . . . . .
C                                      . . . . . . . . . . covariance
 3400 CONTINUE
      WRITE (ISYSWR, '(A)') ' THE "COVARIANCE" COMMAND IS OSBSOLETE.',
     + ' THE COVARIANCE MATRIX IS NOW SAVED IN A DIFFERENT FORMAT',
     + ' WITH THE "SAVE" COMMAND AND READ IN WITH:"SET COVARIANCE"'
      IERFLG = 3
      GO TO 5000
C                                        . . . . . . . . . . printout
 3500 CONTINUE
      CNEWAY = 'SET PRInt '
      GO TO 3100
C                                        . . . . . . . . . . gradient
 3600 CONTINUE
      CNEWAY = 'SET GRAd  '
      GO TO 3100
C                                        . . . . . . . . . . matout
 3700 CONTINUE
      CNEWAY = 'SHOW COVar'
      GO TO 3100
C                                        . . . . . . . . . error def
 3800 CONTINUE
      CNEWAY = 'SET ERRdef'
      GO TO 3100
C                                        . . . . . . . . . . limits
 3900 CONTINUE
      CNEWAY = 'SET LIMits'
      GO TO 3100
C                                        . . . . . . . . . . punch
 4000 CONTINUE
      CNEWAY = 'SAVE      '
C                                ....... come from obsolete commands
 3100 WRITE (ISYSWR, 3101) CWORD,CNEWAY
 3101 FORMAT (' OBSOLETE COMMAND:',1X,A10,5X,'PLEASE USE:',1X,A10)
      CWORD = CNEWAY
      IF (CWORD .EQ. 'SAVE      ') GO TO 1500
      GO TO 700
C                                 . . . . . . . . . . . . . . . . . .
 5000 RETURN
      END
