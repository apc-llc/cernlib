*
* $Id: fmgeti.F,v 1.1.1.1 1996/03/07 15:18:14 mclareni Exp $
*
* $Log: fmgeti.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:14  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMGETI(LBANK,IVECT,IOFF,IRC)
*
*     Routine to get an INTEGER from the bank at LBANK
*
#include "fatmen/fatbank.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fatusr.inc"
      IRC = 0
*
*     Check that we are not outside the bank
*
      IF(IOFF.LE.0.OR.IOFF.GT.NWDSFA) THEN
         IF(IDEBFA.GE.0) THEN
            PRINT *,'FMGETI. attempt to read outside bank boundaries'
            PRINT *,'FMGETI. IOFF = ',IOFF
            ENDIF
         IRC = 1
         RETURN
      ENDIF
 
      LADDR = LBANK+KOFUFA
      IF(LBANK.EQ.-1) LADDR = LADDBK + KOFUFA
 
      IVECT = IQ(LADDR+IOFF)
 
      END
 
