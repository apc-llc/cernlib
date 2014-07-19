CDECK  ID>, HWSGEN.
*CMZ :-        -26/04/91  14.55.45  by  Federico Carminati
*-- Author :    Bryan Webber
C-----------------------------------------------------------------------
      SUBROUTINE HWSGEN(GENEX)
C-----------------------------------------------------------------------
C     GENERATES X VALUES (IF GENEX)
C     EVALUATES STRUCTURE FUNCTIONS AND ENFORCES CUTOFFS ON X
C-----------------------------------------------------------------------
      INCLUDE 'HERWIG59.INC'
      DOUBLE PRECISION HWBVMC,HWRUNI,X,QL
      INTEGER I,J
      LOGICAL GENEX
      EXTERNAL HWBVMC,HWRUNI
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
