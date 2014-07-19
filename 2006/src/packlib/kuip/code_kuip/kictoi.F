*
* $Id: kictoi.F,v 1.1.1.1 1996/03/08 15:32:53 mclareni Exp $
*
* $Log: kictoi.F,v $
* Revision 1.1.1.1  1996/03/08 15:32:53  mclareni
* Kuip
*
*
#include "kuip/pilot.h"
*CMZ :  1.61/01 22/06/89  17.34.30  by
*-- Author :
      SUBROUTINE KICTOI(CVAL,IVAL)
*
********************************************************************************
*
* Convert character string CVAL into integer value IVAL
*
*     Input :
*
      CHARACTER*(*) CVAL
*
*     Output :
*
      INTEGER IVAL
*
********************************************************************************
*
#include "kuip/kcques.inc"
      PARAMETER (MAXDIG=15)
      CHARACTER*(MAXDIG) CHTEMP,BLANK
*
      IQUEST(1)=0
      L=LENOCC(CVAL)
      IF (L.EQ.0) GO TO 999
      IF (L.GT.MAXDIG) THEN
        IQUEST(1)=1
        GO TO 999
      ENDIF
      DO 10 I=1,L
        IF ((CVAL(I:I).LT.'0'.OR.CVAL(I:I).GT.'9').AND.
     +       CVAL(I:I).NE.'+'.AND.CVAL(I:I).NE.'-'.AND.
     +       CVAL(I:I).NE.' ') GO TO 100
10    CONTINUE
      BLANK=' '
      IF (L.EQ.MAXDIG) THEN
        CHTEMP=CVAL
      ELSE
        CHTEMP=BLANK(:MAXDIG-L)//CVAL
      ENDIF
      READ (CHTEMP,'(I15)',ERR=100) IVAL
      GO TO 999
100   IQUEST(1)=1
999   END



