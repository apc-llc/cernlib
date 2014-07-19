*
* $Id: hwhew0.F,v 1.1.1.1 1996/03/08 17:02:14 mclareni Exp $
*
* $Log: hwhew0.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:14  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.47  by  Unknown
*-- Author :
CDECK  ID>, HWHEW0.
*CMZ :-        -26/04/91  11.11.55  by  Bryan Webber
*-- Author :    Zoltan Kunszt, modified by Bryan Webber & Mike Seymour
C-----------------------------------------------------------------------
      SUBROUTINE HWHEW0(IP,ETOT,XM,PR,WEIGHT,CR)
      IMPLICIT NONE
      INTEGER IP,I
      DOUBLE PRECISION ETOT,XM(2),PR(5,2),WEIGHT,CR,PI,XM1,XM2,S,D1,
     & PABS,D,CX,C,E,F,SC,G,HWRGEN
      PI=3.1415926536
      XM1=XM(1)**2
      XM2=XM(2)**2
      S=ETOT*ETOT
      D1=S-XM1-XM2
      PABS=SQRT(D1*D1-4.*XM1*XM2)
      D=D1/PABS
      IF(IP.EQ.2)GOTO3
      CX=CR
      C=D-(D+CX)*((D-CR)/(D+CX))**HWRGEN(2)
      GOTO 4
3     E=((D+1.D0)/(D-1.D0))*(2D0*HWRGEN(3)-1.D0)
      C=D*((E-1.D0)/(E+1.D0))
4     F=2D0*PI*HWRGEN(4)
      SC=SQRT(1.D0-C*C)
      PR(4,1)=(S+XM1-XM2)/(2D0*ETOT)
      PR(5,1)=SQRT(PR(4,1)*PR(4,1)-XM1)
      PR(4,2)=ETOT-PR(4,1)
      PR(3,1)=PR(5,1)*C
      PR(5,2)=PR(5,1)
      PR(2,1)=PR(5,1)*SC*COS(F)
      PR(1,1)=PR(5,1)*SC*SIN(F)
      DO 7 I=1,3
7     PR(I,2)=-PR(I,1)
      G=0.
      IF(IP.EQ.1)G=(D-C)*LOG((D+CX)/(D-CR))
      IF(IP.EQ.2)G=(D*D-C*C)/D*LOG((D+1.D0)/(D-1.D0))
      WEIGHT=PI*G*PR(5,1)/ETOT*0.5D0
      RETURN
      END
