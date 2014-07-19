*
* $Id: izctor.F,v 1.1.1.1 1996/02/14 13:11:09 mclareni Exp $
*
* $Log: izctor.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:09  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.06/05 28/02/89  18.04.24  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IZCTOR(CVAL,RVAL)
*.===========>
*.
*. Convert character string CVAL into real value RVAL
*.
*..==========>
      CHARACTER*(*) CVAL
      PARAMETER (MAXDIG=16)
      CHARACTER*(MAXDIG) CHTEMP,BLANK
*.______________________________________
*
      L=LENOCC(CVAL)
      IF (L.EQ.0)RETURN
      IF (L.GT.MAXDIG)RETURN
      BLANK=' '
      IF (L.EQ.MAXDIG) THEN
         CHTEMP=CVAL
      ELSE
         CHTEMP=BLANK(:MAXDIG-L)//CVAL
      ENDIF
      CALL CLTOU(CHTEMP)
      I1 =INDEX(CHTEMP,'.')
      II1=0
      IF (I1.LT.L) II1=INDEX(CHTEMP(I1+1:),'.')
      I2=INDEX(CHTEMP,'E')
      II2=0
      IF (I2.LT.L) II2=INDEX(CHTEMP(I2+1:),'E')
      IF (II1+II2.NE.0)RETURN
      IF (I1+I2.EQ.0) THEN
         CALL IZCTOI(CHTEMP,IVAL)
         RVAL=IVAL
         RETURN
      ENDIF
      IF (I1.EQ.L.AND.I2.EQ.0) THEN
         CALL IZCTOI(CHTEMP(:I1-1),IVAL)
         RVAL=IVAL
         RETURN
      ENDIF
      BLANK(1:15)=CHTEMP(2:16)
      CHTEMP=BLANK(1:15)//'0'
      READ (CHTEMP,'(G15.9)',END=999,ERR=999) RVAL
*
  999 END

