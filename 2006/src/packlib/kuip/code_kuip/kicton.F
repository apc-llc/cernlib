*
* $Id: kicton.F,v 1.1.1.1 1996/03/08 15:32:53 mclareni Exp $
*
* $Log: kicton.F,v $
* Revision 1.1.1.1  1996/03/08 15:32:53  mclareni
* Kuip
*
*
#include "kuip/pilot.h"
*CMZ :  1.61/06 17/07/89  17.29.18  by
** Replaced format descriptor G15.9 by G15.7
*-- Author :
      SUBROUTINE KICTON(CVAL,IVAL,RVAL)
*
********************************************************************************
*
* Convert character string CVAL into integer value IVAL or real value RVAL
*
* IQUEST(1) is returned <> 0 if a conversion error occurred
* IQUEST(2) is returned <> 0 if the number is out of the integer range
*           (i.e. it can be read only from RVAL and not from IVAL)
*
*     Input :
*
      CHARACTER*(*) CVAL
*
*     Output :
*
      INTEGER IVAL
      REAL RVAL
*
********************************************************************************
*
#include "kuip/kcques.inc"
      PARAMETER (MAXDIG=15)
      CHARACTER*(MAXDIG) CHTEMP,BLANK
      PARAMETER (MAXINT=2147483647)
*
      IQUEST(1)=0
      IQUEST(2)=0
      IQUEST(14)=0

      L=LENOCC(CVAL)
      IF (L.EQ.0) GO TO 999

      IF (L.GT.MAXDIG) GO TO 900

      BLANK=' '
      IF (L.EQ.MAXDIG) THEN
        CHTEMP=CVAL
      ELSE
        CHTEMP=BLANK(:MAXDIG-L)//CVAL
      ENDIF

      CALL CLTOU(CHTEMP)

      I1=INDEX(CHTEMP(MAXDIG-L+1:),'E')

      IF (I1.EQ.1) I1=0
      IF (I1.GT.0) THEN
        DO 10 I=MAXDIG-L+1,MAXDIG-L+I1-1
          IF ((CHTEMP(I:I).LT.'0'.OR.CHTEMP(I:I).GT.'9').AND.
     +         CHTEMP(I:I).NE.'+'.AND.CHTEMP(I:I).NE.'-'.AND.
     +         CHTEMP(I:I).NE.' '.AND.CHTEMP(I:I).NE.'.') GO TO 900
10      CONTINUE
      ENDIF

      I2=INDEX(CHTEMP(MAXDIG-L+1:),'+')
      I3=INDEX(CHTEMP(MAXDIG-L+1:),'-')
      IF (I2.GT.1.AND.I1.EQ.0) GO TO 900
      IF (I3.GT.1.AND.I1.EQ.0) GO TO 900

      IF (L.EQ.1) THEN
        IF (CHTEMP(MAXDIG:MAXDIG).EQ.'E') GO TO 900
        IF (CHTEMP(MAXDIG:MAXDIG).EQ.'-') GO TO 900
        IF (CHTEMP(MAXDIG:MAXDIG).EQ.',') GO TO 900
        IF (CHTEMP(MAXDIG:MAXDIG).EQ.'.') GO TO 900
        IF (CHTEMP(MAXDIG:MAXDIG).EQ.'+') GO TO 900
      ENDIF

      L=MAXDIG

      I1 =INDEX(CHTEMP,'.')
      II1=0
      IF (I1.LT.L) II1=INDEX(CHTEMP(I1+1:),'.')
      I2=INDEX(CHTEMP,'E')
      II2=0
      IF (I2.LT.L) II2=INDEX(CHTEMP(I2+1:),'E')

      IF (II1+II2.NE.0) GO TO 900

      IF (I1+I2.EQ.0) THEN
        CALL KICTOI(CHTEMP,IVAL)
        IQUEST(14)=2
        RVAL=IVAL
        GO TO 999
      ENDIF

      IF (I1.EQ.L.AND.I2.EQ.0) THEN
        CALL KICTOI(CHTEMP(:I1-1),IVAL)
        IQUEST(14)=2
        RVAL=IVAL
        GO TO 999
      ENDIF

      IF (I2.GT.0.AND.I1.EQ.0) THEN
        BLANK(:I2-2)=CHTEMP(2:I2-1)
        BLANK(I2-1:I2-1)='.'
        BLANK(I2:)=CHTEMP(I2:)
        CHTEMP=BLANK
      ENDIF

      READ (CHTEMP,'(G15.7)',END=999,ERR=900) RVAL
      IQUEST(14)=1
      IF (ABS(RVAL).LT.MAXINT) THEN
        IVAL=RVAL
      ELSE
        IQUEST(2)=1
      ENDIF
      GO TO 999

900   IQUEST(1)=1
      GO TO 999

999   CONTINUE

      END



