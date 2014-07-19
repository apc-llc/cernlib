*
* $Id: cydiy.F,v 1.1.1.1 1996/02/15 17:47:51 mclareni Exp $
*
* $Log: cydiy.F,v $
* Revision 1.1.1.1  1996/02/15 17:47:51  mclareni
* Kernlib
*
*
#include "kernbit/pilot.h"
      SUBROUTINE  CYDIY (JULIAN, YEAR, LEAP, DIY)
C
CC    COMPUTE DIY, YEAR & LEAP FROM JULIAN
C
C     SINCE
C     JULIAN = DIY  +  (YEAR - 1) * 365  +  (YEAR - 1) / 4
C    -              -  (YEAR - 1) / 100  +  (YEAR - 1) / 400
C     WITH INTEGER DIVISION, THE INVERSION WILL NOT BE EXACT, BUT GIVE
C     A GOOD VALUE TO START FROM:
C     JULIAN <= (YEAR - 1) * (365 + 1/4 - 1/100 + 1/400)
C             = (YEAR - 1) * 146097 / 400
C     YEAR  APPROX.= JULIAN * 400 / 146097 + 1
C
C                                      LYEAR IS COPY FROM MAIN ROUTINE
      INTEGER  LYEAR (2)
      INTEGER  JULIAN, YEAR, LEAP, DIY
      INTEGER  CLEAP
      DATA LYEAR / 365, 366 /
C
C
      YEAR = (JULIAN * 400) / 146097 + 1
      DIY  = JULIAN - (YEAR - 1) * 365  -  (YEAR - 1) / 4
     +              + (YEAR - 1) / 100  -  (YEAR - 1) / 400
      IF  ( DIY .LT. 1 )  GO TO  200
C
C     ADVANCE YEAR
  100 LEAP = CLEAP ( YEAR )
      IF  ( DIY .LE. LYEAR (LEAP) )  GO TO  900
      DIY  = DIY - LYEAR (LEAP)
      YEAR = YEAR + 1
      GO TO  100
C
C     RETARD YEAR
  200 YEAR = YEAR - 1
      LEAP = CLEAP ( YEAR )
      DIY  = DIY + LYEAR (LEAP)
      IF  ( DIY .LT. 1 )  GO TO  200
C
  900 CONTINUE
      END
