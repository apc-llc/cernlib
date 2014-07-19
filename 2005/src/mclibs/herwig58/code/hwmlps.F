*
* $Id: hwmlps.F,v 1.1.1.1 1996/03/08 17:02:16 mclareni Exp $
*
* $Log: hwmlps.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:16  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.48  by  Unknown
*-- Author :
CDECK  ID>, HWMLPS.
*CMZ :-        -26/04/91  14.17.04  by  Federico Carminati
*-- Author :    David Ward, modified by Bryan Webber
C------------------------------------------------------------------------
      SUBROUTINE HWMLPS(NCL,TECM)
C     GENERATES CYLINDRICAL PHASE SPACE
C     ACCORDING TO THE METHOD OF JADACH
C
C     MODIFIED 29/3/87 BY BRW FOR USE WITH HERWIG
C     RETURNS NCL=0 IF UNSUCCESSFUL
C------------------------------------------------------------------------
#include "herwig58/herwig58.inc"
      INTEGER NTRY,NCL,I,NIT,IY(NMXPAR)
      DOUBLE PRECISION HWREXT,HWRUNG,HWUSQR,TECM,ESS,ALOGS,EPS,SUMX,
     & SUMY,PT,PX,PY,PT2,SUMPT2,SUMTM,XIMIN,XIMAX,YY,SUM1,SUM2,SUM3,
     & SUM4,EX,FY,DD,DYY,ZZ,E1,TM,SLOP(11),XI(NMXPAR)
      EQUIVALENCE (XI,VPAR),(IY,ISTPAR)
C---Factors for pt slopes to fit data.  IDBR contains the type of
C   q-qbar pair produced in this cluster (0 if 1-particle cluster).
C   Corresponding slopes here:
      DATA SLOP/5.2,5.2,5.2,3.0,5.2,5.2,5.2,5.2,5.2,5.2,3.0/
C                -   d   u   s   uu  ud  dd  us  ds  ss  c
      IF (NCL.GT.NMXPAR) THEN
        CALL HWWARN('HWMLPS',1,*999)
        NCL=NMXPAR
      ENDIF
      ESS=TECM**2
      ALOGS=LOG(ESS)
      EPS=1.E-5/NCL
      NTRY=0
  11  NTRY=NTRY+1
      IF (NTRY.GT.NSTRY) THEN
        NCL=0
        RETURN
      ENDIF
      SUMX=0.
      SUMY=0.
      DO 12 I=1,NCL
C---Pt distribution of form exp(-b*Mt)
      PT=HWREXT(PPAR(5,I),SLOP(IDPAR(I)+1))
      PT=HWUSQR(PT**2-PPAR(5,I)**2)
      CALL HWRAZM(PT,PX,PY)
      PPAR(1,I)=PX
      PPAR(2,I)=PY
      SUMX=SUMX+PPAR(1,I)
  12  SUMY=SUMY+PPAR(2,I)
      SUMX=SUMX/NCL
      SUMY=SUMY/NCL
      SUMPT2=0.
      SUMTM=0.
      DO 13 I=1,NCL
      PPAR(1,I)=PPAR(1,I)-SUMX
      PPAR(2,I)=PPAR(2,I)-SUMY
      PT2=PPAR(1,I)**2+PPAR(2,I)**2
      SUMPT2=SUMPT2+PT2
C---STORE TRANSVERSE MASS IN PPAR(3,I) TEMPORARILY
      PPAR(3,I)=SQRT(PT2+PPAR(5,I)**2)
  13  SUMTM=SUMTM+PPAR(3,I)
      IF (SUMTM.GT.TECM) GO TO 11
      DO 14 I=1,NCL
C---Form of "reduced rapidity" distribution
      XI(I)=HWRUNG(0.6*ONE,ONE)
  14  CONTINUE
      CALL HWUSOR(XI,NCL,IY,1)
      XIMIN=XI(1)
      XIMAX=XI(NCL)-XI(1)
C---N.B. TARGET CLUSTER IS SECOND
      XI(1)=0.
      DO 16 I=NCL-1,2,-1
      XI(I+1)=(XI(I)-XIMIN)/XIMAX
  16  CONTINUE
      XI(2)=1.
      YY=LOG(ESS/(PPAR(3,1)*PPAR(3,2)))
      DO 18 NIT=1,10
      SUM1=0.
      SUM2=0.
      SUM3=0.
      SUM4=0.
      DO 19 I=1,NCL
      TM=PPAR(3,I)
      EX=EXP(YY*XI(I))
      SUM1=SUM1+(TM*EX)
      SUM2=SUM2+(TM/EX)
      SUM3=SUM3+(TM*EX)*XI(I)
  19  SUM4=SUM4+(TM/EX)*XI(I)
      FY=ALOGS-LOG(SUM1*SUM2)
      DD=(SUM3*SUM2-SUM1*SUM4)/(SUM1*SUM2)
      DYY=FY/DD
      IF(ABS(DYY/YY).LT.EPS) GO TO 20
  18  YY=YY+DYY
C---Y ITERATIONS EXCEEDED - TRY AGAIN
      IF (NTRY.LT.10) GO TO 11
      EPS=10.*EPS
      IF (EPS.GT.1.) CALL HWWARN('HWMLPS',100,*999)
      CALL HWWARN('HWMLPS',50,*11)
   20 YY=YY+DYY
      ZZ=LOG(TECM/SUM1)
      DO 22 I=1,NCL
      TM=PPAR(3,I)
      E1=EXP(ZZ+YY*XI(I))
      PPAR(3,I)=(0.5*TM)*((1./E1)-E1)
      PPAR(4,I)=(0.5*TM)*((1./E1)+E1)
  22  CONTINUE
 999  END
