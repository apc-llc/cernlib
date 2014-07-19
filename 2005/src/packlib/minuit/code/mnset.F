*
* $Id: mnset.F,v 1.2 1996/03/15 18:02:52 james Exp $
*
* $Log: mnset.F,v $
* Revision 1.2  1996/03/15 18:02:52  james
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
      SUBROUTINE MNSET(FCN,FUTIL)
#include "minuit/d506dp.inc"
CC        Called from MNEXCM
CC        Interprets the commands that start with SET and SHOW
CC
#include "minuit/d506cm.inc"
C
      EXTERNAL FCN,FUTIL
C        file characteristics for SET INPUT
      LOGICAL LNAME
      CHARACTER CFNAME*64, CMODE*16
C       'SET ' or 'SHOW',  'ON ' or 'OFF', 'SUPPRESSED' or 'REPORTED  '
      CHARACTER CKIND*4,    COPT*3,         CWARN*10
C        explanation of print level numbers -1:3  and strategies 0:2
      CHARACTER CPRLEV(-1:3)*34 ,CSTRAT(0:2)*44
C        identification of debug options
      PARAMETER (NUMDBG = 6)
      CHARACTER*40 CDBOPT(0:NUMDBG)
C        things that can be set or shown
      CHARACTER*10 CNAME(30)
      DATA CNAME( 1)/'FCN value '/
      DATA CNAME( 2)/'PARameters'/
      DATA CNAME( 3)/'LIMits    '/
      DATA CNAME( 4)/'COVariance'/
      DATA CNAME( 5)/'CORrelatio'/
      DATA CNAME( 6)/'PRInt levl'/
      DATA CNAME( 7)/'NOGradient'/
      DATA CNAME( 8)/'GRAdient  '/
      DATA CNAME( 9)/'ERRor def '/
      DATA CNAME(10)/'INPut file'/
      DATA CNAME(11)/'WIDth page'/
      DATA CNAME(12)/'LINes page'/
      DATA CNAME(13)/'NOWarnings'/
      DATA CNAME(14)/'WARnings  '/
      DATA CNAME(15)/'RANdom gen'/
      DATA CNAME(16)/'TITle     '/
      DATA CNAME(17)/'STRategy  '/
      DATA CNAME(18)/'EIGenvalue'/
      DATA CNAME(19)/'PAGe throw'/
      DATA CNAME(20)/'MINos errs'/
      DATA CNAME(21)/'EPSmachine'/
      DATA CNAME(22)/'OUTputfile'/
      DATA CNAME(23)/'BATch     '/
      DATA CNAME(24)/'INTeractiv'/
      DATA CNAME(25)/'VERsion   '/
          DATA NNAME/25/
C        options not intended for normal users
      DATA CNAME(26)/'reserve   '/
      DATA CNAME(27)/'NODebug   '/
      DATA CNAME(28)/'DEBug     '/
      DATA CNAME(29)/'SHOw      '/
      DATA CNAME(30)/'SET       '/
          DATA NNTOT/30/
C
      DATA CPRLEV(-1)/'-1: NO OUTPUT EXCEPT FROM "SHOW"  '/
      DATA CPRLEV( 0)/' 0: REDUCED OUTPUT                '/
      DATA CPRLEV( 1)/' 1: NORMAL OUTPUT                 '/
      DATA CPRLEV( 2)/' 2: EXTRA OUTPUT FOR PROBLEM CASES'/
      DATA CPRLEV( 3)/' 3: MAXIMUM OUTPUT                '/
C
      DATA CSTRAT( 0)/' 0: MINIMIZE THE NUMBER OF CALLS TO FUNCTION'/
      DATA CSTRAT( 1)/' 1: TRY TO BALANCE SPEED AGAINST RELIABILITY'/
      DATA CSTRAT( 2)/' 2: MAKE SURE MINIMUM TRUE, ERRORS CORRECT  '/
C
      DATA CDBOPT(0)/'REPORT ALL EXCEPTIONAL CONDITIONS      '/
      DATA CDBOPT(1)/'MNLINE: LINE SEARCH MINIMIZATION       '/
      DATA CDBOPT(2)/'MNDERI: FIRST DERIVATIVE CALCULATIONS  '/
      DATA CDBOPT(3)/'MNHESS: SECOND DERIVATIVE CALCULATIONS '/
      DATA CDBOPT(4)/'MNMIGR: COVARIANCE MATRIX UPDATES      '/
      DATA CDBOPT(5)/'MNHES1: FIRST DERIVATIVE UNCERTAINTIES '/
      DATA CDBOPT(6)/'MNCONT: MNCONTOUR PLOT (MNCROS SEARCH) '/
C
C
      DO 2 I= 1, NNTOT
      IF (INDEX(CWORD(4:10),CNAME(I)(1:3)) .GT. 0)  GO TO 5
    2 CONTINUE
      I = 0
    5 KNAME = I
C
C           Command could be SET xxx, SHOW xxx,  HELP SET or HELP SHOW
      IF (INDEX(CWORD(1:4),'HEL') .GT. 0)  GO TO 2000
      IF (INDEX(CWORD(1:4),'SHO') .GT. 0)  GO TO 1000
      IF (INDEX(CWORD(1:4),'SET') .EQ. 0)  GO TO 1900
C                           ---
      CKIND = 'SET '
C                                        . . . . . . . . . . set unknown
      IF (KNAME .LE. 0)  GO TO 1900
C                                        . . . . . . . . . . set known
      GO TO(3000,  20,  30,  40,3000,  60,  70,  80,  90, 100,
     +       110, 120, 130, 140, 150, 160, 170,3000, 190,3000,
     +       210, 220, 230, 240,3000,1900, 270, 280, 290, 300) , KNAME
C
C                                        . . . . . . . . . . set param
   20 CONTINUE
      IPRM = WORD7(1)
      IF (IPRM .GT. NU)  GO TO 25
      IF (IPRM .LE. 0)   GO TO 25
      IF (NVARL(IPRM) .LT. 0)  GO TO 25
      U(IPRM) = WORD7(2)
      CALL MNEXIN(X)
      ISW2 = ISW(2)
      CALL MNRSET(1)
C        Keep approximate covariance matrix, even if new param value
      ISW(2) = MIN(ISW2,1)
      CFROM = 'SET PARM'
      NFCNFR = NFCN
      CSTATU = 'NEW VALUES'
      GO TO 4000
   25 WRITE (ISYSWR,'(A/)') ' UNDEFINED PARAMETER NUMBER.  IGNORED.'
      GO TO 4000
C                                        . . . . . . . . . . set limits
   30 CALL MNLIMS
      GO TO 4000
C                                        . . . . . . . . . . set covar
   40 CONTINUE
C   this command must be handled by MNREAD, and is not Fortran-callable
      GO TO 3000
C                                        . . . . . . . . . . set print
   60 ISW(5) = WORD7(1)
      GO TO 4000
C                                        . . . . . . . . . . set nograd
   70 ISW(3) = 0
      GO TO 4000
C                                        . . . . . . . . . . set grad
   80 CALL MNGRAD(FCN,FUTIL)
      GO TO 4000
C                                        . . . . . . . . . . set errdef
   90 IF (WORD7(1) .EQ. UP)  GO TO 4000
      IF (WORD7(1) .LE. ZERO)  THEN
         IF (UP .EQ. UPDFLT)  GO TO 4000
         UP = UPDFLT
      ELSE
         UP = WORD7(1)
      ENDIF
      DO 95 I= 1, NPAR
      ERN(I) = 0.
   95 ERP(I) = 0.
      CALL MNWERR
      GO TO 4000
C                                        . . . . . . . . . . set input
C This command must be handled by MNREAD. If it gets this far,
C         it is illegal.
  100 CONTINUE
      GO TO 3000
C                                        . . . . . . . . . . set width
  110 NPAGWD = WORD7(1)
      NPAGWD = MAX(NPAGWD,50)
      GO TO 4000
C                                        . . . . . . . . . . set lines
  120 NPAGLN = WORD7(1)
      GO TO 4000
C                                        . . . . . . . . . . set nowarn
  130 LWARN = .FALSE.
      GO TO 4000
C                                        . . . . . . . . . . set warn
  140 LWARN = .TRUE.
      CALL MNWARN('W','SHO','SHO')
      GO TO 4000
C                                        . . . . . . . . . . set random
  150 JSEED = INT(WORD7(1))
      VAL = 3.
      CALL MNRN15(VAL, JSEED)
      IF (ISW(5) .GT. 0) WRITE (ISYSWR, 151) JSEED
  151 FORMAT (' MINUIT RANDOM NUMBER SEED SET TO ',I10)
      GO TO 4000
C                                        . . . . . . . . . . set title
  160 CONTINUE
C   this command must be handled by MNREAD, and is not Fortran-callable
      GO TO 3000
C                                        . . . . . . . . . set strategy
  170 ISTRAT = WORD7(1)
      ISTRAT = MAX(ISTRAT,0)
      ISTRAT = MIN(ISTRAT,2)
      IF (ISW(5) .GT. 0)  GO TO 1172
      GO TO 4000
C                                       . . . . . . . . . set page throw
  190 NEWPAG = WORD7(1)
      GO TO 1190
C                                        . . . . . . . . . . set epsmac
  210 IF (WORD7(1).GT.ZERO .AND. WORD7(1).LT.0.1) EPSMAC = WORD7(1)
      EPSMA2 = SQRT(EPSMAC)
      GO TO 1210
C                                        . . . . . . . . . . set outputfile
  220 CONTINUE
      IUNIT = WORD7(1)
      ISYSWR = IUNIT
      ISTKWR(1) = IUNIT
      IF (ISW(5) .GE. 0) GO TO 1220
      GO TO 4000
C                                        . . . . . . . . . . set batch
  230 ISW(6) = 0
      IF (ISW(5) .GE. 0)  GO TO 1100
      GO TO 4000
C                                        . . . . . . . . . . set interactive
  240 ISW(6) = 1
      IF (ISW(5) .GE. 0)  GO TO 1100
      GO TO 4000
C                                        . . . . . . . . . . set nodebug
  270 ISET = 0
      GO TO 281
C                                        . . . . . . . . . . set debug
  280 ISET = 1
  281 CONTINUE
      IDBOPT = WORD7(1)
      IF (IDBOPT .GT. NUMDBG) GO TO 288
      IF (IDBOPT .GE. 0) THEN
          IDBG(IDBOPT) = ISET
          IF (ISET .EQ. 1)  IDBG(0) = 1
      ELSE
C             SET DEBUG -1  sets all debug options
          DO 285 ID= 0, NUMDBG
  285     IDBG(ID) = ISET
      ENDIF
      LREPOR = (IDBG(0) .GE. 1)
      CALL MNWARN('D','SHO','SHO')
      GO TO 4000
  288 WRITE (ISYSWR,289) IDBOPT
  289 FORMAT (' UNKNOWN DEBUG OPTION',I6,' REQUESTED. IGNORED')
      GO TO 4000
C                                        . . . . . . . . . . set show
  290 CONTINUE
C                                        . . . . . . . . . . set set
  300 CONTINUE
      GO TO 3000
C                -----------------------------------------------------
 1000 CONTINUE
C               at this point, CWORD must be 'SHOW'
      CKIND = 'SHOW'
      IF (KNAME .LE. 0)  GO TO 1900
      GO TO (1010,1020,1030,1040,1050,1060,1070,1070,1090,1100,
     +       1110,1120,1130,1130,1150,1160,1170,1180,1190,1200,
     +       1210,1220,1100,1100,1250,1900,1270,1270,1290,1300),KNAME
C
C                                        . . . . . . . . . . show fcn
 1010 CONTINUE
      IF (AMIN .EQ. UNDEFI)  CALL MNAMIN(FCN,FUTIL)
      CALL MNPRIN (0,AMIN)
      GO TO 4000
C                                        . . . . . . . . . . show param
 1020 CONTINUE
      IF (AMIN .EQ. UNDEFI)  CALL MNAMIN(FCN,FUTIL)
      CALL MNPRIN (5,AMIN)
      GO TO 4000
C                                        . . . . . . . . . . show limits
 1030 CONTINUE
      IF (AMIN .EQ. UNDEFI)  CALL MNAMIN(FCN,FUTIL)
      CALL MNPRIN (1,AMIN)
      GO TO 4000
C                                        . . . . . . . . . . show covar
 1040 CALL MNMATU(1)
      GO TO 4000
C                                        . . . . . . . . . . show corre
 1050 CALL MNMATU(0)
      GO TO 4000
C                                        . . . . . . . . . . show print
 1060 CONTINUE
      IF (ISW(5) .LT.-1)  ISW(5) = -1
      IF (ISW(5) .GT. 3)  ISW(5) = 3
      WRITE (ISYSWR,'(A)') ' ALLOWED PRINT LEVELS ARE:'
      WRITE (ISYSWR,'(27X,A)') CPRLEV
      WRITE (ISYSWR,1061)  CPRLEV(ISW(5))
 1061 FORMAT (/' CURRENT PRINTOUT LEVEL IS ',A)
      GO TO 4000
C                                        . . . . . . . show nograd, grad
 1070 CONTINUE
      IF (ISW(3) .LE. 0) THEN
         WRITE (ISYSWR, 1081)
 1081    FORMAT(' NOGRAD IS SET.  DERIVATIVES NOT COMPUTED IN FCN.')
      ELSE
         WRITE (ISYSWR, 1082)
 1082    FORMAT('   GRAD IS SET.  USER COMPUTES DERIVATIVES IN FCN.')
      ENDIF
      GO TO 4000
C                                       . . . . . . . . . . show errdef
 1090 WRITE (ISYSWR, 1091)  UP
 1091 FORMAT (' ERRORS CORRESPOND TO FUNCTION CHANGE OF',G13.5)
      GO TO 4000
C                                       . . . . . . . . . . show input,
C                                                batch, or interactive
 1100 CONTINUE
      INQUIRE(UNIT=ISYSRD,NAMED=LNAME,NAME=CFNAME)
      CMODE = 'BATCH MODE      '
      IF (ISW(6) .EQ. 1)  CMODE = 'INTERACTIVE MODE'
      IF (.NOT. LNAME)  CFNAME='unknown'
      WRITE (ISYSWR,1002) CMODE,ISYSRD,CFNAME
 1002 FORMAT (' INPUT NOW BEING READ IN ',A,' FROM UNIT NO.',I3/
     + ' FILENAME: ',A)
      GO TO 4000
C                                       . . . . . . . . . . show width
 1110 WRITE (ISYSWR,1111) NPAGWD
 1111 FORMAT (10X,'PAGE WIDTH IS SET TO',I4,' COLUMNS')
      GO TO 4000
C                                       . . . . . . . . . . show lines
 1120 WRITE (ISYSWR,1121) NPAGLN
 1121 FORMAT (10X,'PAGE LENGTH IS SET TO',I4,' LINES')
      GO TO 4000
C                                       . . . . . . .show nowarn, warn
 1130 CONTINUE
                 CWARN = 'SUPPRESSED'
      IF (LWARN) CWARN = 'REPORTED  '
      WRITE (ISYSWR,1141) CWARN
 1141 FORMAT (' MINUIT WARNING MESSAGES ARE ',A)
      IF (.NOT. LWARN) CALL MNWARN('W','SHO','SHO')
      GO TO 4000
C                                      . . . . . . . . . . show random
 1150 VAL = 0.
      CALL MNRN15(VAL,IGRAIN)
      IKSEED = IGRAIN
      WRITE (ISYSWR, 1151)  IKSEED
 1151 FORMAT (' MINUIT RNDM SEED IS CURRENTLY=',I10/)
      VAL = 3.0
      ISEED = IKSEED
      CALL MNRN15(VAL,ISEED)
      GO TO 4000
C                                        . . . . . . . . . show title
 1160 WRITE (ISYSWR,'(A,A)') ' TITLE OF CURRENT TASK IS:',CTITL
      GO TO 4000
C                                        . . . . . . . show strategy
 1170 WRITE (ISYSWR, '(A)') ' ALLOWED STRATEGIES ARE:'
      WRITE (ISYSWR, '(20X,A)') CSTRAT
 1172 WRITE (ISYSWR, 1175) CSTRAT(ISTRAT)
 1175 FORMAT (/' NOW USING STRATEGY ',A/)
      GO TO 4000
C                                          . . . . . show eigenvalues
 1180 CONTINUE
      ISWSAV = ISW(5)
      ISW(5) = 3
      IF (ISW(2) .LT. 1)  THEN
         WRITE (ISYSWR,'(1X,A)') COVMES(0)
      ELSE
         CALL MNPSDF
      ENDIF
      ISW(5) = ISWSAV
      GO TO 4000
C                                            . . . . . show page throw
 1190 WRITE (ISYSWR,'(A,I3)') ' PAGE THROW CARRIAGE CONTROL =',NEWPAG
      IF (NEWPAG .EQ. 0)
     +    WRITE (ISYSWR,'(A)') ' NO PAGE THROWS IN MINUIT OUTPUT'
      GO TO 4000
C                                        . . . . . . show minos errors
 1200 CONTINUE
      DO 1202 II= 1, NPAR
      IF (ERP(II).GT.ZERO .OR. ERN(II).LT.ZERO)  GO TO 1204
 1202 CONTINUE
      WRITE (ISYSWR,'(A)')
     +   '       THERE ARE NO MINOS ERRORS CURRENTLY VALID.'
      GO TO 4000
 1204 CONTINUE
      CALL MNPRIN(4,AMIN)
      GO TO 4000
C                                        . . . . . . . . . show epsmac
 1210 WRITE (ISYSWR,'(A,E12.3)')
     +  ' FLOATING-POINT NUMBERS ASSUMED ACCURATE TO',EPSMAC
      GO TO 4000
C                                        . . . . . . show outputfiles
 1220 CONTINUE
      WRITE (ISYSWR,'(A,I4)') '  MINUIT PRIMARY OUTPUT TO UNIT',ISYSWR
      GO TO 4000
C                                        . . . . . . show version
 1250 CONTINUE
      WRITE (ISYSWR,'(A,A)') ' THIS IS MINUIT VERSION:',CVRSN
      GO TO 4000
C                                        . . . . . . show nodebug, debug
 1270 CONTINUE
      DO 1285 ID= 0, NUMDBG
      COPT = 'OFF'
      IF (IDBG(ID) .GE. 1)  COPT = 'ON '
 1285 WRITE (ISYSWR,1286) ID, COPT, CDBOPT(ID)
 1286 FORMAT (10X,'DEBUG OPTION',I3,' IS ',A3,' :',A)
      IF (.NOT. LREPOR) CALL MNWARN('D','SHO','SHO')
      GO TO 4000
C                                        . . . . . . . . . . show show
 1290 CKIND = 'SHOW'
      GO TO 2100
C                                        . . . . . . . . . . show set
 1300 CKIND = 'SET '
      GO TO 2100

C                -----------------------------------------------------
C                              UNKNOWN COMMAND
 1900 WRITE (ISYSWR, 1901) CWORD
 1901 FORMAT (' THE COMMAND:',A10,' IS UNKNOWN.'/)
      GO TO 2100
C                -----------------------------------------------------
C                    HELP SHOW,  HELP SET,  SHOW SET, or SHOW SHOW
 2000 CKIND = 'SET '
      IF (INDEX(CWORD(4:10),'SHO') .GT. 0)  CKIND = 'SHOW'
 2100 WRITE (ISYSWR, 2101)  CKIND,CKIND, (CNAME(KK),KK=1,NNAME)
 2101 FORMAT (' THE FORMAT OF THE ',A4,' COMMAND IS:'//
     +   1X,A4,' xxx    [numerical arguments if any]'//
     +   ' WHERE xxx MAY BE ONE OF THE FOLLOWING:'/
     +   (7X,6A12))
      GO TO 4000
C                -----------------------------------------------------
C                               ILLEGAL COMMAND
 3000 WRITE (ISYSWR,'('' ABOVE COMMAND IS ILLEGAL.   IGNORED'')')
 4000 RETURN
      END
