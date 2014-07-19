*
* $Id: hictoi.F,v 1.1.1.1 1996/01/16 17:07:39 mclareni Exp $
*
* $Log: hictoi.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:39  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/11 23/08/94  14.17.45  by  Rene Brun
*-- Author :
      SUBROUTINE HICTOI(CVAL,IVAL)
*.==========>
*         Convert character string CVAL into integer value IVAL
*..=========> ( P.Zanarini (KUIP routine KICTOI)
      COMMON /QUEST/ IQUEST(100)
      CHARACTER*(*) CVAL
      PARAMETER (MAXDIG=15)
      CHARACTER*(MAXDIG) CHTEMP,BLANK
*.___________________________________________
      IQUEST(1)=0
      L=LENOCC(CVAL)
      IF (L.EQ.0) GO TO 999
      IF (L.GT.MAXDIG) THEN
         IQUEST(1)=1
         GO TO 999
      ENDIF
      DO 10 I=1,L
         IF ((CVAL(I:I).LT.'0'.OR.CVAL(I:I).GT.'9').AND. CVAL(I:I)
     +   .NE.'+'.AND.CVAL(I:I).NE.'-'.AND. CVAL(I:I).NE.' ') GO TO 100
   10 CONTINUE
      BLANK=' '
      IF (L.EQ.MAXDIG) THEN
         CHTEMP=CVAL
      ELSE
         CHTEMP=BLANK(:MAXDIG-L)//CVAL
      ENDIF
      READ (CHTEMP,'(I15)',ERR=100) IVAL
      GO TO 999
  100 IQUEST(1)=1
  999 END
