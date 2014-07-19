*
* $Id: dzdwcs.F,v 1.1.1.1 1996/03/04 16:13:18 mclareni Exp $
*
* $Log: dzdwcs.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:18  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDWCS(CKEYWC, IHKEYR, NFKEY)
#include "zebra/rzcl.inc"
#include "zebra/rzk.inc"
      CHARACTER*(*) CKEYWC
      DIMENSION IHKEYR(2)
      INTEGER INKEY(2)
      CHARACTER*8 CHINKY
*
*-----------------------------------------------------------------------
*
 
      IQUEST(1)=0
      IHKEYR(1) = 0
      IF(NFKEY .LT. 1)NFKEY = 1
 
      IF(LQRS.EQ.0)GO TO 999
      IF(LQRS.EQ.0)GO TO 999
      IF(LCDIR.EQ.0)GO TO 999
      LK = IQ(KQSP+LCDIR+KLK)
      NK = IQ(KQSP+LCDIR+KNKEYS)
      NWK= IQ(KQSP+LCDIR+KNWKEY)
      IF(NWK .NE. 2)GOTO 999
      IF(NFKEY .GT. NK )GOTO 999
      NCH = LEN(CKEYWC)
      IF(NCH .GT. 8)NCH = 8
*
      N1 = NFKEY
      DO 30 I=N1,NK
         K=LK+(NWK+1)*(I-1)
         DO 10 J=1,2
               CALL ZITOH(IQ(KQSP+LCDIR+K+J),INKEY(J),1)
   10    CONTINUE
         CALL UHTOC(INKEY(1),4,CHINKY,NCH)
         DO 20 IC = 1,NCH
               IF(CKEYWC(IC:IC) .EQ. '*')GOTO 20
               IF(CKEYWC(IC:IC) .NE. CHINKY(IC:IC))GOTO 30
   20    CONTINUE
         NFKEY = I+1
         IHKEYR(1) = INKEY(1)
         IHKEYR(2) = INKEY(2)
         GOTO 999
   30 CONTINUE
*
  999 END
