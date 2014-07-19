*
* $Id: prob.F,v 1.1.1.1 1996/02/15 17:48:34 mclareni Exp $
*
* $Log: prob.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:34  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
      FUNCTION PROB (CHI2,N)
C     AUTHOR  C.LETERTRE, REVISED BY B.SCHORR, 23.10.81
C
      LOGICAL MFLAG, RFLAG
#if defined(CERNLIB_NUMHIPRE)
      DATA SRTOPI /0.79788456080287/
#endif
#if defined(CERNLIB_NUMLOPRE)
      DATA SRTOPI /0.7978846/
#endif
#if defined(CERNLIB_NUME2465)
      DATA UPL /11300./
#endif
#if defined(CERNLIB_NUME293)
      DATA UPL /1300./
#endif
#if defined(CERNLIB_NUME75)
      DATA UPL /340./
#endif
#if defined(CERNLIB_NUME38)
      DATA UPL /170./
#endif
#if defined(CERNLIB_NUME999)
      DATA      ?????  UPL NOT DEFINED  ?????
#endif
      PROB=0.
      IF(N .LE. 0) GO TO 91
      IF(CHI2 .LT. 0.) GO TO 92
      IF (N .GT. 100) GO TO 30
      IF (CHI2 .GT. UPL)  RETURN
      EMYO2=EXP(-0.5*CHI2)
      SUM=1.
      TERM=1.
      M=N/2
      IF (2*M .NE. N) GO TO 1
C--                ENTRY IF N IS EVEN
      IF (M .EQ. 1) GO TO 11
      DO 10 I=2,M
      FI=I-1
      TERM=0.5*TERM*CHI2/FI
   10 SUM=SUM+TERM
   11 PROB=EMYO2*SUM
      RETURN
C--                ENTRY IF N IS ODD
    1 SRTY=SQRT (CHI2)
      VALUE=2.*(1.-FREQ (SRTY))
      IF (N .NE. 1) GO TO 2
      PROB=VALUE
      RETURN
    2 CONST=SRTOPI*SRTY*EMYO2
      IF (N .EQ. 3) GO TO 21
      K=M-1
      DO 20 I=1,K
      FI =I
      TERM=TERM*CHI2/(2.*FI+1.)
   20 SUM=SUM+TERM
   21 PROB=CONST*SUM+VALUE
      RETURN
C--                USE ASYMPTOTIC FORMULA
   30 ANU=1./FLOAT (N)
      AN9=ANU/4.5
      XP=1./3.
      Z=((CHI2*ANU)**XP - (1.-AN9)) / SQRT (AN9)
      PROB=1. - FREQ (Z)
      RETURN
C
   91 CALL KERMTR('G100.1',LGFILE,MFLAG,RFLAG)
      IF(MFLAG) THEN
         IF(LGFILE .EQ. 0) THEN
            WRITE(*,101) N
         ELSE
            WRITE(LGFILE,101) N
         ENDIF
      ENDIF
      IF(.NOT. RFLAG) CALL ABEND
      RETURN
C
   92 CALL KERMTR('G100.2',LGFILE,MFLAG,RFLAG)
      IF(MFLAG) THEN
         IF(LGFILE .EQ. 0) THEN
            WRITE(*,102) CHI2
         ELSE
            WRITE(LGFILE,102) CHI2
         ENDIF
      ENDIF
      IF(.NOT. RFLAG) CALL ABEND
      RETURN
  101 FORMAT( 7X, 'FUNCTION PROB ... N =',I6,' IS LESS THAN 1')
  102 FORMAT( 7X, 'FUNCTION PROB ... X =',E20.10,' IS LESS THAN 0.')
      END
