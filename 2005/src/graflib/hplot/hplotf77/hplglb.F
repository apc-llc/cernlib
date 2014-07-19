*
* $Id: hplglb.F,v 1.1.1.1 1996/01/19 10:50:08 mclareni Exp $
*
* $Log: hplglb.F,v $
* Revision 1.1.1.1  1996/01/19 10:50:08  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.18/19 21/10/94  11.04.51  by  O.Couet
*-- Author :    O.Couet   03/09/92
      SUBROUTINE HPLGLB(N,NLAB,CHLAB,LENLAB)
#include "hbook/hcbook.inc"
#include "hplot/hplstr.inc"
      CHARACTER*(*) CHLAB
*
      PARAMETER (LABLEN=32)
      LOGICAL FOUND
*
      FOUND=.FALSE.
      IF(N.GE.1.AND.N.LE.9)THEN
         LTMP = 0
         LN   = LQ(LHPLOT-2)
         IF(LN.EQ.0)GOTO 20
   10    IF(LN.NE.0)THEN
            IF(IQ(LN+1).EQ.N)THEN
               LTMP=LN
               FOUND=.TRUE.
            ENDIF
            LN=LQ(LN)
            GOTO 10
         ENDIF
      ENDIF
   20 IF(.NOT.FOUND)THEN
         NLAB=0
         LENLAB=1
         CHLAB(1:1)=' '
         RETURN
      ENDIF
*
      LENLAB=1
      NLAB=IQ(LTMP+2)
      DO 30 I=1,NLAB
         CHLAB(LENLAB:LENLAB)=' '
         LENLAB=LENLAB+1
         CHTEMP=' '
         CALL UHTOC(IQ(LTMP+3+(I-1)*LABLEN/4),4,CHTEMP,LABLEN)
         ILEN=LENOCC(CHTEMP)
         IF(ILEN.LE.0)THEN
            CHLAB(LENLAB:LENLAB+3)=''' '''
            LENLAB=LENLAB+3
         ELSE
            CHLAB(LENLAB:LENLAB+ILEN+2)=''''//CHTEMP(1:ILEN)//''''
            LENLAB=LENLAB+ILEN+2
         ENDIF
   30 CONTINUE
      CHLAB(LENLAB+1:LENLAB+1)=CHAR(0)
*
      END
