*
* $Id: minexam.F,v 1.1.1.1 1996/03/07 14:31:32 mclareni Exp $
*
* $Log: minexam.F,v $
* Revision 1.1.1.1  1996/03/07 14:31:32  mclareni
* Minuit
*
*
#include "minuit/pilot.h"
      PROGRAM MNEXAM
C             Minuit test case.  Fortran-callable.
C             Fit randomly-generated leptonic K0 decays to the
C       time distribution expected for interfering K1 and K2,
C       with free parameters Re(X), Im(X), DeltaM, and GammaS.
#include "minuit/d506dp.inc"
      EXTERNAL FCNK0
CC    OPEN (UNIT=6,FILE='DSDQ.OUT',STATUS='NEW',FORM='FORMATTED')
      DIMENSION NPRM(5),VSTRT(5),STP(5)
      CHARACTER*10 PNAM(5)
      DATA NPRM /   1   ,    2   ,     5    ,   10     ,  11    /
      DATA PNAM /'Re(X)', 'Im(X)', 'Delta M','T Kshort','T Klong'/
      DATA VSTRT/   0.  ,    0.  ,    .535  ,   .892   ,  518.3 /
      DATA STP  /   0.1 ,    0.1 ,     0.1  ,     0.   ,   0.   /
      DATA ZERO,ONE,THREE,FIVE / 0., 1., 3., 5. /
      CALL MNINIT(5,6,7)
      DO 11  I= 1, 5
       CALL MNPARM(NPRM(I),PNAM(I),VSTRT(I),STP(I),ZERO,ZERO,IERFLG)
       IF (IERFLG .NE. 0)  THEN
          WRITE (6,'(A,I3)')  ' UNABLE TO DEFINE PARAMETER NO.',I
          STOP
       ENDIF
   11 CONTINUE
C
      CALL MNSETI('Time Distribution of Leptonic K0 Decays')
C       Request FCN to read in (or generate random) data (IFLAG=1)
      CALL MNEXCM(FCNK0, 'CALL FCN', ONE ,1,IERFLG, 0)
C
      CALL MNEXCM(FCNK0,'FIX', FIVE ,1,IERFLG,0)
      CALL MNEXCM(FCNK0,'SET PRINT', ZERO ,1,IERFLG,0)
      CALL MNEXCM(FCNK0,'MIGRAD', ZERO ,0,IERFLG,0)
      CALL MNEXCM(FCNK0,'MINOS', ZERO ,0,IERFLG,0)
      CALL MNEXCM(FCNK0,'RELEASE', FIVE ,1,IERFLG,0)
      CALL MNEXCM(FCNK0,'MIGRAD', ZERO ,0,IERFLG,0)
      CALL MNEXCM(FCNK0,'MINOS',  ZERO ,0,IERFLG,0)
      CALL MNEXCM(FCNK0,'CALL FCN', THREE , 1,IERFLG,0)
      STOP
      END
