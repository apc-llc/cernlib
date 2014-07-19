*
* $Id: hwstab.F,v 1.1.1.1 1996/03/08 17:02:17 mclareni Exp $
*
* $Log: hwstab.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:17  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.48  by  Unknown
*-- Author :
CDECK  ID>, HWSTAB.
*CMZ :-        -26/04/91  11.11.56  by  Bryan Webber
*-- Author :    Adapted by Bryan Webber
C------------------------------------------------------------------------
      FUNCTION HWSTAB(F,AFUN,NN,X,MM)
      DOUBLE PRECISION HWSTAB
C     MODIFIED CERN INTERPOLATION ROUTINE DIVDIF
C     LIKE HWUTAB BUT USES FUNCTION AFUN IN PLACE OF ARRAY A
C------------------------------------------------------------------------
      INTEGER NN,MM,MMAX,N,M,MPLUS,IX,IY,MID,NPTS,IP,I,J,L,ISUB
      DOUBLE PRECISION AFUN,SUM,X,F(NN),T(20),D(20)
      EXTERNAL AFUN
      LOGICAL EXTRA
      DATA MMAX/10/
      N=NN
      M=MIN(MM,MMAX,N-1)
      MPLUS=M+1
      IX=0
      IY=N+1
      IF (AFUN(1).GT.AFUN(N)) GO TO 94
   91 MID=(IX+IY)/2
      IF (X.GE.AFUN(MID)) GO TO 92
      IY=MID
      GO TO 93
   92 IX=MID
   93 IF (IY-IX.GT.1) GO TO 91
      GO TO 97
   94 MID=(IX+IY)/2
      IF (X.LE.AFUN(MID)) GO TO 95
      IY=MID
      GO TO 96
   95 IX=MID
   96 IF (IY-IX.GT.1) GO TO 94
   97 NPTS=M+2-MOD(M,2)
      IP=0
      L=0
      GO TO 99
   98 L=-L
      IF (L.GE.0) L=L+1
   99 ISUB=IX+L
      IF ((1.LE.ISUB).AND.(ISUB.LE.N)) GO TO 100
      NPTS=MPLUS
      GO TO 101
  100 IP=IP+1
      T(IP)=AFUN(ISUB)
      D(IP)=F(ISUB)
  101 IF (IP.LT.NPTS) GO TO 98
      EXTRA=NPTS.NE.MPLUS
      DO 14 L=1,M
      IF (.NOT.EXTRA) GO TO 12
      ISUB=MPLUS-L
      D(M+2)=(D(M+2)-D(M))/(T(M+2)-T(ISUB))
   12 I=MPLUS
      DO 13 J=L,M
      ISUB=I-L
      D(I)=(D(I)-D(I-1))/(T(I)-T(ISUB))
      I=I-1
   13 CONTINUE
   14 CONTINUE
      SUM=D(MPLUS)
      IF (EXTRA) SUM=0.5*(SUM+D(M+2))
      J=M
      DO 15 L=1,M
      SUM=D(J)+(X-T(J))*SUM
      J=J-1
   15 CONTINUE
      HWSTAB=SUM
      END
