CDECK  ID>, HWSTAB.
*CMZ :-        -26/04/91  11.11.56  by  Bryan Webber
*-- Author :    Adapted by Bryan Webber
C-----------------------------------------------------------------------
      FUNCTION HWSTAB(F,AFUN,NN,X,MM)
C-----------------------------------------------------------------------
C     MODIFIED CERN INTERPOLATION ROUTINE DIVDIF
C     LIKE HWUTAB BUT USES FUNCTION AFUN IN PLACE OF ARRAY A
C-----------------------------------------------------------------------
      DOUBLE PRECISION HWSTAB,AFUN,SUM,X,F(NN),T(20),D(20)
      INTEGER NN,MM,MMAX,N,M,MPLUS,IX,IY,MID,NPTS,IP,I,J,L,ISUB
      LOGICAL EXTRA
      EXTERNAL AFUN
      DATA MMAX/10/
      N=NN
      M=MIN(MM,MMAX,N-1)
      MPLUS=M+1
      IX=0
      IY=N+1
      IF (AFUN(1).GT.AFUN(N)) GOTO 94
   91 MID=(IX+IY)/2
      IF (X.GE.AFUN(MID)) GOTO 92
      IY=MID
      GOTO 93
   92 IX=MID
   93 IF (IY-IX.GT.1) GOTO 91
      GOTO 97
   94 MID=(IX+IY)/2
      IF (X.LE.AFUN(MID)) GOTO 95
      IY=MID
      GOTO 96
   95 IX=MID
   96 IF (IY-IX.GT.1) GOTO 94
   97 NPTS=M+2-MOD(M,2)
      IP=0
      L=0
      GOTO 99
   98 L=-L
      IF (L.GE.0) L=L+1
   99 ISUB=IX+L
      IF ((1.LE.ISUB).AND.(ISUB.LE.N)) GOTO 100
      NPTS=MPLUS
      GOTO 101
  100 IP=IP+1
      T(IP)=AFUN(ISUB)
      D(IP)=F(ISUB)
  101 IF (IP.LT.NPTS) GOTO 98
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
      HWSTAB=SUM
      END
