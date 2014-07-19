*
* $Id: simstk.F,v 1.1.1.1 1995/12/12 14:36:18 mclareni Exp $
*
* $Log: simstk.F,v $
* Revision 1.1.1.1  1995/12/12 14:36:18  mclareni
* Imported sources
*
*
#include "sigma/pilot.h"
*CMZ :  1.09/04 26/10/93  09.49.19  by  Carlo E. Vandoni
*-- Author :
      SUBROUTINE SIMSTK (MADDR,MN)
C
C
C
C    ....................................................
C
C
C         PURPOSE
C            PUT MADDR AND MN IN MSTACK
C            BUT ISTAPO IS NOT INCREMENTED
C
C         USAGE
C             CALL SIMSTK(MADDR,MN)
C
C         DESCRIPTION OF PARAMETERS
C
C         COMM. BLOCKS USED
C            COM1
C            COMVAR USED: ISTAPO,MSTACK(,)
C
C         REMARKS
C            MADDR AND MN = ZERO ... IF UNUSED
C                         = 6 ... IF MISSING INDEX
C         SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C            TRACE
C            MESSI
C
C...  PAW VERSION  ...  MAY 1988
C
C    ......................................................
C
C
C
#include "sigma/sigc.inc"
C
      CALL SITRAC (' SIMSTK  ')
C
C
C...  AIMSTK USED FOR REAL CONSTANTS
C
C
      MSTACK(1,ISTAPO)=MADDR
      MSTACK(2,ISTAPO)=MN
      IF(SITRAK(20).EQ.1)PRINT*,' STACKED  ',ISTAPO
 999  END
