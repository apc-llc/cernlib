*
* $Id: cqcomp.F,v 1.1.1.1 1996/03/06 10:46:59 mclareni Exp $
*
* $Log: cqcomp.F,v $
* Revision 1.1.1.1  1996/03/06 10:46:59  mclareni
* Zebra
*
*
#include "test_include/pilot.h"
      SUBROUTINE CQCOMP (IFAIL,NW,IDATA,ADATA)

C-    Execute comparison for 1 check-card,  called from CQDATA

#include "test_include/cqc.inc"
#include "test_include/cqc1.inc"
C--------------    End CDE                             --------------
      DIMENSION    IDATA(9), ADATA(9), AMUST(9)
      EQUIVALENCE (AMUST(1),MUST(1))


      DO 29 I=1,NW
      IF (IDATA(I).EQ.MUST(I))     GO TO 29
      IF   (MACHFL(I))       29, 22, 21
   21 IF (IQCOPT(7).EQ.0)          GO TO 29
#if !defined(CERNLIB_TOLER1)
   22 IF (KODEFM(I).NE.2)          GO TO 41
#endif
#if defined(CERNLIB_TOLER1)
   22 IF (KODEFM(I).EQ.2)          GO TO 24
      IF (IABS(IDATA(I)-MUST(I)).EQ.1)  GO TO 29
      GO TO 41
   24 CONTINUE
#endif
      DIFF= ABS(ADATA(I)-AMUST(I))
      IF (AMUST(I).NE.0.0)         GO TO 28
      IF (DIFF.LT.1.0E-10)         GO TO 29
      GO TO 41
   28 IF (ABS(DIFF/AMUST(I)).GE..0051)  GO TO 41
   29 CONTINUE
      IFAIL= 0
      RETURN

   41 IFAIL= I
      RETURN
      END
*      ==================================================
#include "zebra/qcardl.inc"
