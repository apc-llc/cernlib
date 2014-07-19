CDECK  ID>, HWMULT.
*CMZ :-        -26/04/91  11.11.55  by  Bryan Webber
*-- Author :    David Ward, modified by Bryan Webber
C-----------------------------------------------------------------------
      SUBROUTINE HWMULT(EPPBAR,NCHT)
C-----------------------------------------------------------------------
C     Chooses charged multiplicity NCHT at the p-pbar c.m. energy EPPBAR
C-----------------------------------------------------------------------
      DOUBLE PRECISION HWMNBI,HWRGEN,EPPBAR,E0,ALOGS,RK,EK,AVN,SUM,R,
     & CUM(100)
      INTEGER NCHT,IMAX,I,N
      EXTERNAL HWMNBI,HWRGEN
      DATA E0/0./
      IF (EPPBAR.NE.E0) THEN
         E0=EPPBAR
C---Initialize
         ALOGS=2.*LOG(EPPBAR)
         RK=.029*ALOGS-.104
         IF (ABS(RK).GT.1000.) RK=1000.
         EK=1./RK
         AVN=-9.50+9.11*EPPBAR**0.23
         IF (AVN.LT.1.) AVN=1.
         SUM=0.
         IMAX=1
         DO 10 I=1,100
         N=2*I
         CUM(I)=HWMNBI(N,AVN,EK)
         IF (CUM(I).LT.1.E-6*SUM) GOTO 11
         IMAX=I
         SUM=SUM+CUM(I)
         CUM(I)=SUM
  10     CONTINUE
  11     CONTINUE
         IF (IMAX.LE.1) THEN
            IMAX=1
            CUM(1)=1
         ELSE
            DO 12 I=1,IMAX
  12        CUM(I)=CUM(I)/SUM
         ENDIF
      ENDIF
C --- Select NCHT
      R=HWRGEN(0)
      DO 20 I=1,IMAX
      IF(R.GT.CUM(I)) GOTO 20
      NCHT=2*I
      RETURN
  20  CONTINUE
      CALL HWWARN('HWMULT',100,*999)
  999 END
