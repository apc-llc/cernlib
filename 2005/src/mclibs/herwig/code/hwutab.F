CDECK  ID>, HWUTAB.
*CMZ :-        -26/04/91  11.11.56  by  Bryan Webber
*-- Author :    Adapted by Bryan Webber
C-----------------------------------------------------------------------
      FUNCTION HWUTAB(F,A,NN,X,MM)
C-----------------------------------------------------------------------
C     MODIFIED CERN INTERPOLATION ROUTINE DIVDIF
C-----------------------------------------------------------------------
      DOUBLE PRECISION HWUTAB,SUM,X,F(NN),A(NN),T(20),D(20)
      INTEGER NN,MM,MMAX,N,M,MPLUS,IX,IY,MID,NPTS,IP,I,J,L,ISUB
      LOGICAL EXTRA
      DATA MMAX/10/
      N=NN
      M=MIN(MM,MMAX,N-1)
      MPLUS=M+1
      IX=0
      IY=N+1
      IF (A(1).GT.A(N)) GOTO 4
    1 MID=(IX+IY)/2
      IF (X.GE.A(MID)) GOTO 2
      IY=MID
      GOTO 3
    2 IX=MID
    3 IF (IY-IX.GT.1) GOTO 1
      GOTO 7
    4 MID=(IX+IY)/2
      IF (X.LE.A(MID)) GOTO 5
      IY=MID
      GOTO 6
    5 IX=MID
    6 IF (IY-IX.GT.1) GOTO 4
    7 NPTS=M+2-MOD(M,2)
      IP=0
      L=0
      GOTO 9
    8 L=-L
      IF (L.GE.0) L=L+1
    9 ISUB=IX+L
      IF ((1.LE.ISUB).AND.(ISUB.LE.N)) GOTO 10
      NPTS=MPLUS
      GOTO 11
   10 IP=IP+1
      T(IP)=A(ISUB)
      D(IP)=F(ISUB)
   11 IF (IP.LT.NPTS) GOTO 8
      EXTRA=NPTS.NE.MPLUS
      DO 14 L=1,M
      IF (.NOT.EXTRA) GOTO 12
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
      HWUTAB=SUM
      END
