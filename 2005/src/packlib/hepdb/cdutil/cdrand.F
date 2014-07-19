*
* $Id: cdrand.F,v 1.1.1.1 1996/02/28 16:24:43 mclareni Exp $
*
* $Log: cdrand.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:43  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDRAND(CHRAND,IRC)
      CHARACTER*(*) CHRAND
      CHARACTER*9   CHTEMP
#include "hepdb/slate.inc"
      DATA NENTRY/0/
      SAVE NENTRY
 
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
