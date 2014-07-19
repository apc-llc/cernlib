*
* $Id: hwdhgf.F,v 1.1.1.1 1996/03/08 17:02:11 mclareni Exp $
*
* $Log: hwdhgf.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:11  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.46  by  Unknown
*-- Author :
CDECK  ID>, HWDHGF.
*CMZ :-        -02/05/91  11.11.45  by  Federico Carminati
*-- Author :    Mike Seymour
C-----------------------------------------------------------------------
      FUNCTION HWDHGF(X,Y)
      DOUBLE PRECISION HWDHGF
C  CALCULATE THE DOUBLE BREIT-WIGNER INTEGRAL
C  X=(EMV/EMH)**2 , Y=EMV*GAMV/EMH**2
C-----------------------------------------------------------------------
      DOUBLE PRECISION X,Y,CHANGE,X1,X2,FAC1,FAC2,TH1,TH2,TH1HI,TH1LO,
     &  TH2HI,TH2LO,X2MAX,SQFAC,PIFAC
      PARAMETER (PIFAC=3.1415926535D0)
      INTEGER NBIN,IBIN1,IBIN2
C  CHANGE IS THE POINT WHERE DIRECT INTEGRATION BEGINS TO CONVERGE
C  FASTER THAN STANDARD BREIT-WIGNER SUBSTITUTION
      DATA CHANGE,NBIN/0.425,25/
      HWDHGF=0
      IF (Y.LT.0) RETURN
      IF (X.GT.CHANGE) THEN
C---DIRECT INTEGRATION
        FAC1=0.25 / NBIN
        DO 200 IBIN1=1,NBIN
          X1=(IBIN1-0.5) * FAC1
          FAC2=( (1-SQRT(X1))**2-X1 ) / NBIN
          DO 100 IBIN2=1,NBIN
            X2=(IBIN2-0.5) * FAC2 + X1
            SQFAC=1+X1**2+X2**2-2*(X1+X2+X1*X2)
            IF (SQFAC.LT.0) GOTO 100
            HWDHGF=HWDHGF + 2.
     &        * ((1-X1-X2)**2+8*X1*X2)
     &        * SQRT(SQFAC)
     &        / ((X1-X)**2+Y**2) *Y
     &        / ((X2-X)**2+Y**2) *Y
     &        * FAC1*FAC2
 100      CONTINUE
 200    CONTINUE
      ELSE
C---INTEGRATION USING TAN THETA SUBSTITUTIONS
        TH1LO=ATAN((0-X)/Y)
        TH1HI=ATAN((1-X)/Y)
        FAC1=(TH1HI-TH1LO) / NBIN
        DO 400 IBIN1=1,NBIN
          TH1=(IBIN1-0.5) * FAC1 + TH1LO
          X1=Y*TAN(TH1) + X
          X2MAX=MIN(X1,(1-SQRT(X1))**2)
          TH2LO=ATAN((0-X)/Y)
          TH2HI=ATAN((X2MAX-X)/Y)
          FAC2=(TH2HI-TH2LO) / NBIN
          DO 300 IBIN2=1,NBIN
            TH2=(IBIN2-0.5) * FAC2 + TH2LO
            X2=Y*TAN(TH2) + X
            SQFAC=1+X1**2+X2**2-2*(X1+X2+X1*X2)
            IF (SQFAC.LT.0) GOTO 300
            HWDHGF=HWDHGF + 2.
     &        * ((1-X1-X2)**2+8*X1*X2)
     &        * SQRT(SQFAC)
     &        * FAC1 * FAC2
 300      CONTINUE
 400    CONTINUE
      ENDIF
      HWDHGF=HWDHGF/(PIFAC*PIFAC)
      END
