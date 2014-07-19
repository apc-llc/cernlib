*
* $Id: mnunpt.F,v 1.1.1.1 1996/03/07 14:31:32 mclareni Exp $
*
* $Log: mnunpt.F,v $
* Revision 1.1.1.1  1996/03/07 14:31:32  mclareni
* Minuit
*
*
#include "minuit/pilot.h"
      LOGICAL FUNCTION MNUNPT(CFNAME)
C           is .TRUE. if CFNAME contains unprintable characters.
      CHARACTER CFNAME*(*)
      CHARACTER CPT*80, CP1*40,CP2*40
      PARAMETER (CP1=' ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklm')
      PARAMETER (CP2='nopqrstuvwxyz1234567890./;:[]$%*_!@#&+()')
      CPT=CP1//CP2
      MNUNPT = .FALSE.
      L = LEN(CFNAME)
      DO 100 I= 1, L
         DO 50 IC= 1, 80
         IF (CFNAME(I:I) .EQ. CPT(IC:IC))  GO TO 100
   50    CONTINUE
      MNUNPT = .TRUE.
      GO TO 150
  100 CONTINUE
  150 CONTINUE
      RETURN
      END
