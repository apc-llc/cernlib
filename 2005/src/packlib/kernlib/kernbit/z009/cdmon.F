*
* $Id: cdmon.F,v 1.1.1.1 1996/02/15 17:47:51 mclareni Exp $
*
* $Log: cdmon.F,v $
* Revision 1.1.1.1  1996/02/15 17:47:51  mclareni
* Kernlib
*
*
#include "kernbit/pilot.h"
      SUBROUTINE  CDMON (LEAP, DIY, MONTH, DAY)
C
CC    COMPUTE DAY & MONTH FROM DIY & LEAP
C
      INTEGER  LEAP, DIY, MONTH, DAY
C
C                                 DAYS IN THE MONTHS ACCUMULATED
C                                 (TABLE COPIED FROM MAIN ROUTINE)
      INTEGER  MONTAB (12,2)
      DATA  MONTAB  /
     *  000, 031, 059, 090, 120, 151, 181, 212, 243, 273, 304, 334,
     *  000, 031, 060, 091, 121, 152, 182, 213, 244, 274, 305, 335 /
C
C
      DO  10, MONTH = 12, 2, -1
         IF  ( DIY .GT. MONTAB (MONTH, LEAP) )  GO TO  20
   10 CONTINUE
C
      MONTH = 1
C
   20 DAY = DIY - MONTAB (MONTH, LEAP)
C
      END
