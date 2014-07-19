*
* $Id: fmputi.F,v 1.1.1.1 1996/03/07 15:18:14 mclareni Exp $
*
* $Log: fmputi.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:14  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMPUTI(LBANK,IVAL,IOFF,IRC)
*
*     Routine to put an INTEGER into the bank at LBANK
*
#include "fatmen/fatbank.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fatusr.inc"
      IRC = 0
*
*     Check that IOFF is valid
*
      IF(IOFF.LE.0.OR.IOFF.GT.NWDSFA) THEN
         IF(IDEBFA.GE.0) THEN
            PRINT *,'FMPUTI. attempt to write outside bank boundaries'
            PRINT *,'FMPUTI. IOFF = ',IOFF
         ENDIF
         IRC = 1
         RETURN
      ENDIF
 
      LADDR = LBANK+KOFUFA
      IF(LBANK.EQ.-1) LADDR = LADDBK + KOFUFA
 
      IQ(LADDR+IOFF) = IVAL
 
      END
