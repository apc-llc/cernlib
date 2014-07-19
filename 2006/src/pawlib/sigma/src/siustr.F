*
* $Id: siustr.F,v 1.1.1.1 1995/12/12 14:36:21 mclareni Exp $
*
* $Log: siustr.F,v $
* Revision 1.1.1.1  1995/12/12 14:36:21  mclareni
* Imported sources
*
*
#include "sigma/pilot.h"
*CMZ :  1.09/04 26/10/93  09.49.20  by  Carlo E. Vandoni
*-- Author :
      SUBROUTINE SIUSTR
C
C    ....................................................
C
C         PURPOSE
C            PRINT VALUE NEWLY ASSIGNED IF !PRINT IS IN USE (MAINKT(2)=1)
C
C         USAGE
C            CALL SIUSTR
C
C         COMM. BLOCKS USED
C            COM1
C            COMVAR USED: MAINKT(),KLASS
C
C         SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C            TRACE
C             PRIT
C
C...  PAW VERSION  ...  MAY 1988
C
C    ......................................................
C
#include "sigma/sigc.inc"
C
C
      CALL SITRAC (' SIUSTR  ')
C
C---  MAINKT(2) = 1 IS !PRINT  ---
      IF(MAINKT(2).EQ.0)RETURN
C
      KLASS=1
      IZ=0
      CALL SIPRIT (0,IZ)
C
 999  END
