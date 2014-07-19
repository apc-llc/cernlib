*
* $Id: cleap.F,v 1.1.1.1 1996/02/15 17:47:51 mclareni Exp $
*
* $Log: cleap.F,v $
* Revision 1.1.1.1  1996/02/15 17:47:51  mclareni
* Kernlib
*
*
#include "kernbit/pilot.h"
      INTEGER  FUNCTION  CLEAP (YEAR)
C
CC    COMPUTE LEAP
C     I.E. SET FUNCTION VALUE TO THE CORRECT TABLE INDEX
C          = 1 FOR NORMAL, = 2 FOR LEAP YEAR
C
C     DECISION TABLE FOR LEAP YEARS
C     -----------------------+------+------+------+------
C     YEAR DIVISIBLE BY   4  | YES  | YES  | YES  | NO
C     YEAR DIVISIBLE BY 100  | YES  | YES  | NO   |--
C     YEAR DIVISIBLE BY 400  | YES  | NO   |  --  |--
C     -----------------------+------+------+------+------
C     YEAR IS LEAP YEAR      | YES  | NO   | YES  | NO
C     EXAMPLES               | 20000 | 1900 | 1980 | 1981
C     -----------------------+------+------+------+------
C
      INTEGER  YEAR
C
      IF     ( MOD (YEAR, 4  ) .NE. 0 )   THEN
                                                  CLEAP = 1
      ELSEIF ( MOD (YEAR, 100) .NE. 0 )   THEN
                                                  CLEAP = 2
      ELSEIF ( MOD (YEAR, 400) .NE. 0 )   THEN
                                                  CLEAP = 1
      ELSE
                                                  CLEAP = 2
      ENDIF
C
      END
