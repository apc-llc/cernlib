CDECK  ID>, HWHEW1.
*CMZ :-        -26/04/91  11.11.55  by  Bryan Webber
*-- Author :    Zoltan Kunszt, modified by Bryan Webber
C-----------------------------------------------------------------------
      SUBROUTINE HWHEW1(NPART)
C-----------------------------------------------------------------------
      IMPLICIT NONE
      DOUBLE PRECISION P(4,7),XMASS,PLAB,PRW,PCM
      INTEGER NPART,I,J,K
      COMMON/HWHEWP/ XMASS(10),PLAB(5,10),PRW(5,2),PCM(5,10)
      DO 10 I=1,NPART
      P(1,I)=PLAB(3,I)
      P(2,I)=PLAB(1,I)
      P(3,I)=PLAB(2,I)
      P(4,I)=PLAB(4,I)
  10  CONTINUE
      DO 20 J=1,4
      DO 30 K=1,(NPART-2)
  30  PCM(J,K)=P(J,K+2)
      PCM(J,NPART-1)=-P(J,1)
      PCM(J,NPART)=-P(J,2)
  20  CONTINUE
      END
