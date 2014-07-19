*
* $Id: hsetbf.F,v 1.1.1.1 1996/01/16 17:07:48 mclareni Exp $
*
* $Log: hsetbf.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:48  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 31/08/90  18.33.21  by  Rene Brun
*-- Author :
      SUBROUTINE HSETBF (IBASFT,MAXPOW,SELLIM)
*.==========>
*.      SET UP BASIC FUNCTIONS TABLE BY SELECTING FROM
*.      THE USER-GIVEN BASIC AND ELEMENTARY FUNCTIONS AND
*.      FROM THE STANDARD POLYNOMIALS
*.      HSELBF = USER-WRITTEN SELECTION FUNCTION
*.      IELEF  = A BASIC FUNCTION REPRESENTATION, SAME
*.               CODING AS FOR IBASFT
*..=========> ( D.Lienart )
      LOGICAL HSELBF,LSEL
#include "hbook/hcpar1.inc"
#include "hbook/hcpout.inc"
#include "hbook/hcunit.inc"
      DIMENSION IBASFT(ND,NBFMAX),MAXPOW(1),IELEF(10)
*
*  USER-GIVEN BASIC FUNCTION SELECTION
*
      DO 10 K=1,NBF
         LSEL=.TRUE.
         IELEF(1)=K*10+2
         IF (IOPT(6).EQ.1) THEN
            DO 5 I=2,ND
               IELEF(I)=K*10+2
    5       CONTINUE
            LSEL=HSELBF(IELEF)
         ENDIF
         IF (LSEL) IBASFT(1,K)=IELEF(1)
   10 CONTINUE
*
*  ELEMENTARY FUNCTION SELECTION TEST
*  STANDARD POLYNOMIALS MAY BE SELECTED BY A DEGREE LIMITING PROCEDURE
*
      CALL VZERO (IELEF,ND)
*
   15 IF (IOPT(6).EQ.0) THEN
         S=0.
         DO 20 I=1,ND
            IS=IELEF(I)/10
            IF (IS*10.EQ.IELEF(I).AND.MAXPOW(I).NE.0) S=S+
     +      FLOAT(IS)/MAXPOW(I)
   20    CONTINUE
         LSEL=S.LE.SELLIM
      ELSE
         LSEL=HSELBF(IELEF)
      ENDIF
*
*  BUILD UP IBASFT
*
      IF (LSEL) THEN
         NBF=NBF+1
         IF (NBF.GT.NBFMAX) THEN
            WRITE (LERR,100) NBFMAX
            IFLAG=5
            RETURN
         ENDIF
         DO 25 I=1,ND
            IBASFT(I,NBF)=IELEF(I)
   25    CONTINUE
      ENDIF
*
*  GENERATE ALL POSSIBLE BASIC FUNCTIONS MADE UP FROM
*  ELEMENTARY FUNCTIONS ; MAXPOW AND NEF ARE USED AS LIMITORS
*
      DO 35 I=1,ND
         NUM=IELEF(I)/10
         ITYP=IELEF(I)-NUM*10
         IF (ITYP.EQ.0) NUMMAX=MAXPOW(I)
         IF (ITYP.EQ.1) NUMMAX=NEF
         IF ((NEF.NE.0.AND.ITYP.EQ.0).OR.NUM.LT.NUMMAX) THEN
            DO 30 K=1,I-1
               IELEF(K)=0
   30       CONTINUE
            IF (NUM.LT.NUMMAX) THEN
               IELEF(I)=(NUM+1)*10+ITYP
               GOTO 15
            ELSE
               IELEF(I)=11
               GOTO 15
            ENDIF
         ENDIF
   35 CONTINUE
*
  100 FORMAT (' UNABLE TO BUILD THE BASIC FUNCTIONS TABLE ',
     +        '- MAXIMUM NUMBER OF CANDIDATE FUNCTIONS > ',I3)
      END
