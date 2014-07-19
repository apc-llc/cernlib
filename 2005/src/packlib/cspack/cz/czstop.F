*
* $Id: czstop.F,v 1.1.1.1 1996/03/08 15:44:24 mclareni Exp $
*
* $Log: czstop.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:24  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE CZSTOP(ICODE)
#include "cspack/czsock.inc"
 
      ISTOP = IABS(ICODE)
 
#if defined(CERNLIB_VAXVMS)
 
      CALL SYS$EXIT(%VAL(ISTOP))
 
#endif
#if !defined(CERNLIB_VAXVMS)
 
      GOTO (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),ISTOP
 
      IF(ISTOP.LE.0.OR.ISTOP.GT.20)THEN
         IF(LUNCZ.NE.0)WRITE(LUNCZ,*)' Error in ISTOP'
         STOP 255
      ENDIF
 
1     CONTINUE
      STOP 1
 
2     CONTINUE
      STOP 2
 
3     CONTINUE
      STOP 3
 
4     CONTINUE
      STOP 4
 
5     CONTINUE
      STOP 5
 
6     CONTINUE
      STOP 6
 
7     CONTINUE
      STOP 7
 
8     CONTINUE
      STOP 8
 
9     CONTINUE
      STOP 9
 
10    CONTINUE
      STOP 10
 
11    CONTINUE
      STOP 11
 
12    CONTINUE
      STOP 12
 
13    CONTINUE
      STOP 13
 
14    CONTINUE
      STOP 14
 
15    CONTINUE
      STOP 15
 
16    CONTINUE
      STOP 16
 
17    CONTINUE
      STOP 17
 
18    CONTINUE
      STOP 18
 
19    CONTINUE
      STOP 19
 
20    CONTINUE
      STOP 20
 
#endif
 
      END
