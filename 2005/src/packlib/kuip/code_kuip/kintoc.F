*
* $Id: kintoc.F,v 1.1.1.1 1996/03/08 15:32:53 mclareni Exp $
*
* $Log: kintoc.F,v $
* Revision 1.1.1.1  1996/03/08 15:32:53  mclareni
* Kuip
*
*
#include "kuip/pilot.h"
*CMZ :  1.61/06 17/07/89  17.29.01  by
** Replaced format descriptor G15.9 by G15.7
*-- Author :
      SUBROUTINE KINTOC(IVAL,RVAL,CVAL)
*
********************************************************************************
*
* Convert integer value IVAL or real value RVAL into character string CVAL
* In input CVAL(1:1) can be 'I' or 'R' to select the type
*
* WARNING: CVAL must be dimensioned at least to 15 in the calling routine
* -----------------------------------------------------------------------
*
*     Input :
*
      INTEGER IVAL
      REAL RVAL
*
*     Input/Output :
*
      CHARACTER*(*) CVAL
*
********************************************************************************
*
#include "kuip/kcques.inc"
      PARAMETER (MAXDIG=15)
      CHARACTER*(MAXDIG) CHTEMP
*
      IQUEST(1)=0
      CHTEMP=' '
      IF (CVAL(1:1).EQ.'I') THEN
        WRITE (CHTEMP,1000) IVAL
1000    FORMAT (I15)
      ELSE IF (CVAL(1:1).EQ.'R') THEN
        IF (RVAL.EQ.0.) THEN
          CHTEMP='0'
        ELSE
          WRITE (CHTEMP,2000) RVAL
2000      FORMAT (G15.7)
        ENDIF
      ELSE
        IQUEST(1)=1
      ENDIF
      IF (LEN(CVAL).LT.LENOCC(CHTEMP)) THEN
        IQUEST(1)=1
      ELSE
        DO 10 I1=1,MAXDIG
          IF (CHTEMP(I1:I1).NE.' ') GO TO 20
10      CONTINUE
        I1=MAXDIG
20      CONTINUE
        I0=INDEX(CHTEMP(I1:MAXDIG),'.')
        IF (I0.EQ.0) THEN
          I2=MAXDIG
        ELSE
          IE=INDEX(CHTEMP(I1:MAXDIG),'E')
          IF (IE.GT.0) THEN
            I2=MAXDIG
            GO TO 50
          ENDIF
          DO 30 I2=MAXDIG,I0,-1
            IF (CHTEMP(I2:I2).NE.'0'.AND.CHTEMP(I2:I2).NE.' ') GO TO 40
30        CONTINUE
40        IF (CHTEMP(I2:I2).EQ.'.') I2=I2-1
        ENDIF
50      CVAL=' '
        CVAL=CHTEMP(I1:I2)
      ENDIF
      END



