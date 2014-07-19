*
* $Id: cfribm16.F,v 1.1.1.1 1996/03/08 15:21:57 mclareni Exp $
*
* $Log: cfribm16.F,v $
* Revision 1.1.1.1  1996/03/08 15:21:57  mclareni
* Epio
*
*
#include "epio/pilot.h"
#if defined(CERNLIB_ND100B16)
       SUBROUTINE CFRIBM(ARRAY,NW,MODE)
C      +++++++NORD10/100 VERSION++++++++++
C      VERSION 82-05-06
C
C      CONVERT FIRST NW WORDS OF ARRAY FROM IBM TO NORD FORMAT.
C      MODE = 1  CONVERT TO 16 BIT INTEGER(DUMMY)
C             2  CONVERT TO 32 BIT INTEGER(DUMMY)
C             3  CONVERT TO 32 BIT FLOATING
C      ON RETURN,ARRAY CONTAINS CONVERTED VALUES.
       INTEGER *4 ARRAY(2)
       IF(NW.LE.0)RETURN
       IF(MODE.EQ.3)THEN
       CALL CFRIB(ARRAY,NW)
       ENDIF
C      ELSE DUMMY
       END
#endif
