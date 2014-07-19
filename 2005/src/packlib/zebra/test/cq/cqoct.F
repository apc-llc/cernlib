*
* $Id: cqoct.F,v 1.1.1.1 1996/03/06 10:46:59 mclareni Exp $
*
* $Log: cqoct.F,v $
* Revision 1.1.1.1  1996/03/06 10:46:59  mclareni
* Zebra
*
*
#include "test_include/pilot.h"
      SUBROUTINE CQOCT (IDATA,JD,JE,NPRI)

C-    Copy  IDATA(JD/JE)  to  M  for printing with conversion if necc.
C-    Codes O and P : convert to pseudo-octal (1-digit integers)

#include "test_include/cqc1.inc"
C--------------    End CDE                             --------------
      DIMENSION    AM(100)
      EQUIVALENCE (AM(1),M(1))
      DIMENSION    IDATA(9),MPACK(2),NOCT(9)
#if defined(CERNLIB_QMVDS)
      SAVE         MPACK, NOCT
#endif
      DATA  MPACK  /3,10/
      DATA  NOCT   /-1,-1,0,10,-1,-1,-1,0,5/


      KD = 1
      KM = 1
      DO 18   JW=JD,JE
      I = KODEFM(KD)
      N = NOCT(I)
      IF   (N)               16, 14, 12
   12 CALL UPKBYT (IDATA(JW),1,M(101),N,MPACK)
#if defined(CERNLIB_CQFLOAT)
      CALL VFLOAT (M(101),M(101),N)
#endif
      CALL UCOPIV (M(101),M(KM),N)
      KM = KM + N
      GO TO 18

   14 CONTINUE
#if defined(CERNLIB_CQFLOAT)
      AM(KM) = IDATA(JW)
      KM = KM + 1
      GO TO 18
#endif

   16 M(KM) = IDATA(JW)
      KM = KM + 1
   18 KD = KD + 1
      NPRI = KM - 1
      RETURN
      END
*      ==================================================
#include "zebra/qcardl.inc"
