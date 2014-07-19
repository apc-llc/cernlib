*
* $Id: hwsgen.F,v 1.1.1.1 1996/03/08 17:02:17 mclareni Exp $
*
* $Log: hwsgen.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:17  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.48  by  Unknown
*-- Author :
CDECK  ID>, HWSGEN.
*CMZ :-        -26/04/91  14.55.45  by  Federico Carminati
*-- Author :    Bryan Webber
C------------------------------------------------------------------------
      SUBROUTINE HWSGEN(GENEX)
C     GENERATES X VALUES (IF GENEX)
C     EVALUATES STRUCTURE FUNCTIONS
C     AND ENFORCES CUTOFFS ON X
C------------------------------------------------------------------------
#include "herwig58/herwig58.inc"
      INTEGER I,J
      DOUBLE PRECISION HWBVMC,HWRUNI,X,QL
      LOGICAL GENEX
      IF (GENEX) THEN
        XX(1)=EXP(HWRUNI(0,ZERO,XLMIN))
        XX(2)=XXMIN/XX(1)
      ENDIF
      DO 10 I=1,2
        J=I
        IF (JDAHEP(1,I).NE.0) J=JDAHEP(1,I)
        X=XX(I)
        QL=(1.-X)*EMSCA
        CALL HWSFUN(X,EMSCA,IDHW(J),NSTRU,DISF(1,I),I)
      DO 10 J=1,13
        IF (QL.LT.HWBVMC(J)) DISF(J,I)=0.
   10 CONTINUE
      END
