*
* $Id: fmchop.F,v 1.1.1.1 1996/03/07 15:18:05 mclareni Exp $
*
* $Log: fmchop.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:05  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMCHOP(ROUTIN,CHOPT,ALLOWD,IRC)
      CHARACTER*(*) ROUTIN,CHOPT,ALLOWD
#include "fatmen/fatbank.inc"
#include "fatmen/fatpara.inc"
      IRC = 0
*
*     This routine checks each character of CHOPT against ALLOWD
*     Invalid characters are flagged
*
      LCHOPT = LENOCC(CHOPT)
      LALLOW = LENOCC(ALLOWD)
      DO 10 I=1,LCHOPT
      IF(INDEX(ALLOWD(1:LALLOW),CHOPT(I:I)).EQ.0)  THEN
         IF(IDEBFA.GE.0) PRINT *,'Invalid option ',CHOPT(I:I),
     +                   ' for routine ',ROUTIN,' ignored'
         IRC = 1
         ENDIF
 
10    CONTINUE
      END
 
