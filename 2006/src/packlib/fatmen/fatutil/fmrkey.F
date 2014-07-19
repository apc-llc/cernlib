*
* $Id: fmrkey.F,v 1.1.1.1 1996/03/07 15:18:08 mclareni Exp $
*
* $Log: fmrkey.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:08  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMRKEY(KEYO,KEYN,IRC)
*
#include "fatmen/fmnkeys.inc"
#include "fatmen/fatbug.inc"
#include "zebra/rzcl.inc"
#include "zebra/rzk.inc"
      DIMENSION KEYO(LKEYFA),KEYN(LKEYFA)
*
*-----------------------------------------------------------------------
*
      IRC       = 0
      IFOUND    = 0
      IF(LQRS.EQ.0)  GOTO 40
      IF(LCDIR.EQ.0) GOTO 40
      LS = IQ(KQSP+LCDIR+KLS)
      LK = IQ(KQSP+LCDIR+KLK)
      NK = IQ(KQSP+LCDIR+KNKEYS)
      NWK= IQ(KQSP+LCDIR+KNWKEY)
*
      DO 30 I=1,NK
 
         K=LK+(NWK+1)*(I-1)
         DO 10 J=1,NWK
            IKDES=(J-1)/10
            IKBIT1=3*J-30*IKDES-2
            IF(JBYT(IQ(KQSP+LCDIR+KKDES+IKDES),IKBIT1,3).LT.3)THEN
               IF(KEYO(J).NE.IQ(KQSP+LCDIR+K+J)) GOTO 30
            ELSE
               CALL ZITOH(IQ(KQSP+LCDIR+K+J),ITEMP,1)
               IF(KEYO(J).NE.ITEMP) GOTO 30
            ENDIF
   10    CONTINUE
*
*     Found matching key vector - update
*
         IF(IDEBFA.GE.0) THEN
            PRINT 9001
9001     FORMAT(' FMRKEY. updating key vector: OLD/NEW')
            CALL FMPKEY(KEYO,LKEYFA)
            CALL FMPKEY(KEYN,LKEYFA)
         ENDIF
 
         DO 20 J=1,NWK
            IKDES=(J-1)/10
            IKBIT1=3*J-30*IKDES-2
            IF(JBYT(IQ(KQSP+LCDIR+KKDES+IKDES),IKBIT1,3).LT.3)THEN
               IQ(KQSP+LCDIR+K+J) = KEYN(J)
            ELSE
               CALL ZHTOI(KEYN(J),IQ(KQSP+LCDIR+K+J),1)
            ENDIF
   20    CONTINUE
 
   30 CONTINUE
 
   40 CONTINUE
 
      IF(IFOUND.EQ.0) IRC = 1
*
      END
 
