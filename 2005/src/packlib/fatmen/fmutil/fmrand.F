*
* $Id: fmrand.F,v 1.1.1.1 1996/03/07 15:18:20 mclareni Exp $
*
* $Log: fmrand.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:20  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMRAND(CHRAND,IRC)
      CHARACTER*(*) CHRAND
      CHARACTER*9   CHTEMP
#include "fatmen/slate.inc"
      SAVE NENTRY
      DATA NENTRY/0/
 
      IRC = 0
 
      IF(NENTRY.EQ.0) THEN
         CALL DATIME(ID,IT)
         I = 0
         J = 0
         CALL RMARIN(IT*100+IS(6),I,J)
         NENTRY = 1
      ENDIF
 
      CALL RANMAR(X,1)
 
      WRITE(CHTEMP,'(F9.8)') X
      CHRAND = CHTEMP(2:)
 
      END
