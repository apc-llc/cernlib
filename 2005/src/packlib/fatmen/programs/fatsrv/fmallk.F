*
* $Id: fmallk.F,v 1.1.1.1 1996/03/07 15:17:40 mclareni Exp $
*
* $Log: fmallk.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:40  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMALLK(NEWKEY,IRC)
#include "zebra/rzcl.inc"
#include "zebra/rzk.inc"
      IRC    = 0
      NKEYS  = 0
      NEWKEY = -1
      JMAX   = 0
      IF(LQRS.EQ.0)GO TO 99
      IF(LCDIR.EQ.0)GO TO 99
      LS = IQ(KQSP+LCDIR+KLS)
      LK = IQ(KQSP+LCDIR+KLK)
      NK = IQ(KQSP+LCDIR+KNKEYS)
      NWK= IQ(KQSP+LCDIR+KNWKEY)
*
*     Is this loop necessary with RZOUT option R?
*     New keys are always appended to the list,
*     so the last key should have the highest value.
*
*     keys block is nwk+1 long - word 1 is pointer to cycles block
*
*     IF(NK.GT.0) THEN
*        K=LK+(NWK+1)*(NK-1) + 1
*        NEWKEY = IQ(KQSP+LCDIR+K) + 1
*     ELSE
*        NEWKEY = 1
*     ENDIF
 
      DO 20 I=1,NK
         K=LK+(NWK+1)*(I-1) + 1
         IF(IQ(KQSP+LCDIR+K).GT.JMAX) JMAX = IQ(KQSP+LCDIR+K)
  20  CONTINUE
      NEWKEY = JMAX + 1
      RETURN
*
  99  IRC = -1
      END
