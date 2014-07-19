*
* $Id: dzdsdo.F,v 1.1.1.1 1996/03/04 16:13:01 mclareni Exp $
*
* $Log: dzdsdo.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:01  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDSDO(IST,L,LUN,CHKEYI,CHOPT)
      CHARACTER*(*) CHOPT, CHKEYI
#include "zebra/zebq.inc"
#include "zebra/mzca.inc"
#include "zebra/mzcb.inc"
#include "zebra/mzcc.inc"
#include "zebra/mzcn.inc"
#include "zebra/zunit.inc"
#include "dzdzbrinc.inc"
      INTEGER KEYV(2), LUP, LUSAVE
      CHARACTER*8 CHKEY
      SAVE LUSAVE
*---
      CALL DZDIBR
*     display documentation
      IF(CTDDOC .NE. ' ')THEN
         CALL RZCDIR(CWDSAV,'R')
         LUSAVE=IQPRNT
         IQPRNT=LUN
         IF(CHKEYI(1:1).EQ.' ')THEN
            CALL MZSDIV(IST,-7)
            KEYV(1)=IQ(KQS+L-4)
            CALL UHTOC(KEYV(1),4,CHKEY,4)
            LUP = LQ(KQS+L+1)
            IF(LUP .GT. 2)THEN
               KEYV(2)=IQ(KQS+LUP-4)
               CALL UHTOC(KEYV(2),4,CHKEY(5:8),4)
            ELSE
               CHKEY(5:8) = 'NONE'
               CALL UCTOH(CHKEY(5:8) ,KEYV(2),4,4)
            ENDIF
         ELSE
            CHKEY=CHKEYI
         ENDIF
         CALL DZDOCO(IQPRNT,CHKEY,CHOPT)
         IQPRNT=LUSAVE
         CALL RZCDIR(CWDSAV,' ')
      ELSE
         WRITE(*,*)'No documentation file open'
      ENDIF
      END
 
*********************************************************************
