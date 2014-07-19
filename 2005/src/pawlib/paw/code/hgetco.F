*
* $Id: hgetco.F,v 1.1.1.1 1996/03/01 11:38:37 mclareni Exp $
*
* $Log: hgetco.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:37  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.01/00 22/02/93  12.36.02  by  Rene Brun
*-- Author :    Rene Brun   03/01/89
      SUBROUTINE HGETCO(CHW,INTW)
*
*            Convert Character string CHW into integer INTW
*            Check if really INTEGER
*
      CHARACTER*(*) CHW
      CHARACTER*8 CHWS
      CHARACTER*1 CHNUM(10)
      SAVE CHNUM
      DATA CHNUM/'1','2','3','4','5','6','7','8','9','0'/
*
      NCH=LENOCC(CHW)
      IF(NCH.GT.8)NCH=8
      IF(NCH.EQ.1.AND.CHW(1:1).EQ.'*')THEN
         INTW=0
         GO TO 99
      ENDIF
      DO 20 I=1,NCH
         DO 10 J=1,10
            IF(CHW(I:I).EQ.CHNUM(J))GO TO 20
  10     CONTINUE
         IF(I.EQ.1.AND.NCH.GT.1)THEN
            IF(CHW(I:I).EQ.'-')GO TO 20
         ENDIF
         INTW=-1000000
         GO TO 99
  20  CONTINUE
*
      CHWS=' '
      CHWS(9-NCH:8)=CHW(1:NCH)
      READ(CHWS,1000,ERR=90)INTW
 1000 FORMAT(I8)
      RETURN
  90  INTW=0
  99  END
