*
* $Id: dzdntd.F,v 1.1.1.1 1996/03/04 16:13:03 mclareni Exp $
*
* $Log: dzdntd.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:03  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDNTD(CDIR,CLUN,ICONT,IRET)
#include "zebra/rzcl.inc"
 
      CHARACTER*(*)  CDIR,CLUN
      INTEGER ICONT,IRET, LRZ, IWHOLL(4), INIFLG,IDHRZ
      SAVE INIFLG, LRZ,IDHRZ
      DATA INIFLG/0/
      IF(INIFLG.EQ.0)THEN
         INIFLG=1
         CALL UCTOH('RZ  ',IDHRZ,4,4)
      ENDIF
 
*     get next top directory (RZfile)
 
      IF(ICONT.EQ.0)THEN
         IF(LQRS.EQ.0)THEN
            ICONT=0
            IRET=-2
            GOTO 999
         ELSE
            LRZ=LQRS
         ENDIF
      ELSE
         LRZ=LQ(KQSP+LRZ)
      ENDIF
 
20    IF(LRZ.EQ.0)THEN
         ICONT=0
         IRET=-1
      ELSE
*        is it really 'RZ' (or 'RZ0')
         IF(IQ(KQSP+LRZ-4).NE.IDHRZ)THEN
            LRZ=LQ(KQSP+LRZ)
            GOTO 20
         ENDIF
         ICONT=ICONT+1
         IRET=0
         CALL ZITOH(IQ(KQSP+LRZ+1),IWHOLL,4)
         CALL UHTOC(IWHOLL,4,CDIR,16)
         WRITE(CLUN,'(I2)')IQ(KQSP+LRZ-5)
      ENDIF
999   END
***********************************************************************
