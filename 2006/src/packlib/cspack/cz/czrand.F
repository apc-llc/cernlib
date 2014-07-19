*
* $Id: czrand.F,v 1.1.1.1 1996/03/08 15:44:24 mclareni Exp $
*
* $Log: czrand.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:24  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE CZRAND(CHRAND)
      CHARACTER*(*) CHRAND
      CHARACTER*9   CHTEMP
#include "cspack/slate.inc"
      DATA NENTRY/0/
      SAVE NENTRY
 
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
