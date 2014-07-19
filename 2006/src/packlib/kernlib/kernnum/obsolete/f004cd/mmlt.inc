*
* $Id: mmlt.inc,v 1.1.1.1 1996/02/15 17:48:10 mclareni Exp $
*
* $Log: mmlt.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:10  mclareni
* Kernlib
*
*
*
* mmlt.inc
*
*CALL     BASIC
*CALL     MPY
*CALL     SCL
*CALL     SQR
*CALL     CPY
*CALL     UTL
 MAIN     MACRO     TYPE
          SETYPE    TYPE
 NAME     MICRO     1,,*"P"_MMLT*
          HEADER    "NAME",SAVEA0
 TEMP     BSS       2                   USED BY TMMPY1
 L        BSS       1
 M        BSS       1
 LY       BSS       1
 LZ       BSS       0
 IZ       BSS       1
 X11      BSS       1
 XI1      BSS       1
 Y1L      BSS       0
 XII      BSS       0
 ZII      BSS       1
 Z11      BSS       1
 Z1L      BSS       0
 T1       BSS       1
          EXIT      "NAME"
          SA2       A1+1                X2=(N)    A2=((N))
          SB7       1                                       B7=1
          SA1       X1                  X1=M      A1=(M)
          SA3       A2+B7               X3=(K)    A3=((K))
          SA4       A2+2                X4=(X)    A4=((X))
          SA5       A2+5                X5=(Y)    A5=((Y))
          SA2       X2                  X2=N      A2=(N)
          SB1       X1                                      B1=M
          SA1       A5+3                X1=(Z)    A1=((Z))
          SA3       X3                  X3=K      A3=(K)
          BX0       X4                  X0=(X)
          SA4       A4+B7               X4=(X12)  A4=((X12))
          BX7       X5                  X7=(Y)
          SA5       A5+B7               X5=(Y12)  A5=((Y12))
          SB2       X2                                      B2=N
          SA2       A4+B7               X2=(X21)  A2=((X21))
          SB3       X3                                      B3=K
          SA3       A5+B7               X3=(Y21)  A3=((Y21))
          SX6       A0                  X6=OLD A0
          LE        B1,B0,EXIT          IF M .LE. 0 GO TO EXIT
          LE        B2,B0,EXIT          IF N .LE. 0 GO TO EXIT
          LE        B3,B0,EXIT          IF K .LE. 0 GO TO EXIT
          SA6       SAVEA0              SAVEA0=OLD A0
          IX4       X4-X0               X4=(X12)-(X)=JX
          IX3       X3-X7               X3=(Y21)-(Y)=JY
          SB4       X4                                      B4=JX
          SB5       X3                                      B5=JY
          SA4       A1+B7               X4=(Z12)  A4=((Z12))
          SA3       A4+B7               X3=(Z21)  A3=((Z21))
*                                       X0=(X)
*                                       X1=(Z)              B1=M
*                                       X2=(X21)            B2=N
*                                       X3=(Z21) A3=((Z21)) B3=K
*                                       X4=(Z12)            B4=JX
*                                       X5=(Y12)            B5=JY
*                                       X7=(Y)              B7=1
*         IF (Z) = (X)  GOTO [5]
*         IF (Z) = (Y)  GOTO [6]
          IX6       X1-X0               X6=(Z)-(X)
          ZR        X6,[5]              IF (Z)=(X) GO TO [5]
          IX6       X1-X7               X6=(Z)-(Y)
          ZR        X6,[6]              IF (Z)=(Y) GO TO [6]
*         IF (Y21) = (X12)  GOTO [2]
          EQ        B4,B5,[2]           IF (X12) = (Y21) GO TO [2]
 [1]      BSS       0
          SX6       B3                  X6=K
          SA0       X1                            A0=(Z(1,1))
          SA6       L                   L=K
          IX6       X2-X0               X6=(X21)-(X)=IX
          BX2       X7                  X2=(Y(1,1))
          SB3       X6                                      B3=IX
          IX6       X5-X2               X6=(Y12)-(Y)
          IX7       X4-X1               X7=(Z12)-(Z)
          SA6       LY                  LY=(Y(1,2))-(Y(1,1))
          SA7       LZ                  LZ=(Z(1,2))-(Z(1,1))
          BX6       X5                  X6=(Y12)
          BX7       X4                  X7=(Z12)
          SA6       Y1L                 Y1L=(Y(1,2))
          SA7       Z1L                 Z1L=(Z(1,2))
          BX6       X0                  X6=(X)
          SX7       B1                  X7=M
          SA6       X11                 X11=(X(1,1))
          SA7       M                   M=M
          IX6       X3-X1               X6=(Z21)-(Z)
          BX1       X0                  X1=(X(1,1))
          SB6       X6                                      B6=IZ
*         DO   L  =  1, M
*         CALL TMMPY(M,N,X,X12,X21,Y1L,Y2L,Z1L,Z2L)
          SB7       MPY
          RJ        MPYX
          SA5       L                   X5=L      A5=L
          SA4       M                   X4=M
          SA3       Z1L                 X3=(Z(1,L)) A3=Z1L
          SA2       Y1L                 X2=(Y(1,L)) A2=Y1L
          SA1       X11                 X1=(X(1,1)) A1=X11
          SX6       X5-1                X6=L-1
          SB1       X4                                      B1=M
          SA6       A5                  L=L-1
          SA5       LZ                  X5=LZ
          SA4       LY                  X4=LY
          ZR        X6,EPILOG           IF L=0 RETURN
          SA0       X3                            A0=(Z(1,L))
          IX7       X3+X5               X7=(Z(1,L+1))
          IX6       X2+X4               X6=(Y(1,L+1))
          SA7       A3                  Z1L=(Z(1,L+1))
          SA6       A2                  Y1L=(Y(1,L+1))
***       JP        MPY
 MPY      "P"MMPY   "PLUS",B1,B2,B3,B4,B5,"NULL",A0,B6,TEMP,B7
*                                                 X1=(X), X2=(Y)
 MPYX     BSS       1                   EXIT LINE (SET ONCE ONLY)
          JP        B7
 [2]      BSS       0
*         IF (Y)   .NE. (X)    GOTO [1]
*         IF (Y12) .NE. (X21)  GOTO [1]
*         IF K     .NE. M      GOTO [1]
          IX6       X7-X0               X6=(Y)-(X)
          NZ        X6,[1]              IF (Y)   .NE. (X)   GO TO [1]
          IX6       X5-X2               X6=(Y12)-(X21)
          NZ        X6,[1]              IF (Y12) .NE. (X21) GO TO [1]
          NE        B3,B1,[1]           IF K .NE. M  GO TO [1]
          BX6       X1                  X6=(Z)
          SA0       X1                            A0=(Z(1,1))
          SA6       Z11                 Z11=(Z(1,1))
          IX6       X2-X0               X6=(X21)-(X)
          IX7       X4-X1               X7=(Z12)-(Z)
          SB3       X6                                      B3=IX
          SB6       X7                                      B6=LZ
          IX6       X3-X1               X6=(Z21)-(Z)
          SX7       B1                  X7=M
          SA6       IZ                  IZ=(Z(2,1))-(Z(1,1))
          SA7       L                   L=M
          SA7       M                   M=M
          BX1       X0                  X1=(X(1,1))
          SB7       X6+B6                                   B7=IZ+LZ
          BX6       X0                  X6=(X)
          SX7       A0+B7               X7=(Z(2,2))
          SA6       X11                 X11=(X(1,1))
          SA6       XI1                 XI1=(X(1,1))
          SA7       ZII                 ZII=(Z(2,2))
*         DO   I  =  1, M
*         CALL TVSQR(N,X(I,1),X(I,2),Z(I,I))
          SB7       [3]
          RJ        MPYX
          SA4       ZII                 X4=(Z(I,I))
          SA1       XI1                 X1=(X(I,1))
          SA0       X4                            A0=(Z(I,I))
*         JP        SQR
 SQR      "P"VSQR   B2,X1,B4,B7
          SA6       A0
 TAU      IFEQ      LENGTH,2
          SA7       A0+1
 TAU      ENDIF
 SQRX     BSS       1                   EXIT LINE (SET ONCE ONLY)
          JP        B7
*         IF  (I  =  M)   RETURN
*         CALL TMMPY(L-1,N,X(I+1,1),X(I+1,2),X(I+2,1),X(I,1),X(I,2),
*                                                 Z(I,I+1),Z(I,I+2))
 [3]      SB7       SQR
          RJ        SQRX
          SA5       L                   X5=L      A5=L
          SA4       IZ                  X4=IZ
          SA2       XI1                 X2=(X(I,1)) A2=XI1
          SA0       A0+B6                         A0=(Z(I,I+1))
          SX6       X5-1                X6=L-1
          SB7       X4                                      B7=IZ
          ZR        X6,[4]              IF L = 1
          SB1       X6                                      B1=L-1
          SX1       X2+B3               X1=(X(I+1,1))
          SA6       A5                  L=L-1
          SX7       A0+B7               X7=(Z(I+1,I+1))
          BX6       X1                  X6=(X(I+1,1))
          SA7       ZII                 ZII=(Z(I+1,I+1))
          SA6       A2                  XI1=(X(I+1,1))
          JP        MPY
*         CALL TMUTL(M,Z,Z(1,2),Z(2,1))
 [4]      SA5       M                   X5=M
          SA1       Z11                 X1=(Z(1,1))
          SB3       X4                                      B3=IZ
          SB4       B6                                      B4=LZ
          SB1       X5-1                                    B1=M-1
          NZ        B1,UTL              IF M>1 CALL TMUTL
          JP        EPILOG
 [5]      BSS       0
*         IF (Z) = (Y)  GOTO [8]
          IX6       X1-X7               X6=(Z)-(Y)
          ZR        X6,[8]
          SWAP      X0,X7               SWAP (X)   AND (Y)
          SWAP      B4,B5               SWAP JX    AND JY
          SWAP      X2,X5               SWAP (X21) AND (Y12)
          SWAP      B1,B3               SWAP M     AND K
 [6]      BSS       0
          SX6       B3                  X6=K
          SA3       A3+B7               X3=(T)
          SA6       L                   L=K
          SA7       Y1L                 Y1L=(Y(1,1))
          IX6       X2-X0               X6=(X21)-(X)
          BX2       X7                  X2=(Y(1,1))
          SB3       X6                                      B3=IX
          SX6       B1                  X6=M
          IX7       X5-X7               X7=(Y12)-(Y)
          SA0       X3                            A0=(T1)
          SA6       M                   M=M
          SA7       LY                  LY=(Y(1,2))-(Y(1,1))
          BX6       X0                  X6=(X)
          BX7       X3                  X7=(T)
          SA6       X11                 X11=(X(1,1))
          SA7       T1                  T1=(T(1))
          SB6       LENGTH                                  B6=JT
          BX1       X0                  X1=(X(1,1))
*         DO   L  =  1, K
*         CALL TMMPY(M,N,X,X(1,2),X(2,1),Y(1,L),Y(2,L),T(1),T(2))
          SB7       [7]
          RJ        CPYX
          SA5       L                   X5=L      A5=L
          SA2       Y1L                 X2=(Y(1,L)) A2=Y1L
          SA1       X11                 X1=(X(1,1))
          SX6       X5-1                X6=L-1
          ZR        X6,EPILOG           IF L=0
          SA6       A5                  L=L-1
          JP        MPY
*         CALL TVCPY(M,T(1),T(2),Y(1,L),Y(2,L))
 [7]      SB7       MPY
          RJ        MPYX
          SA5       LY                  X5=LY
          SA1       Y1L                 X1=(Y(1,L)) A1=Y1L
          SA4       M                   X4=M
          SA3       T1                  X3=(T(1))
          IX6       X1+X5               X6=(Y(1,L+1))
          SB1       X4                                      B1=M
          SA6       A1                  Y1L=(Y(1,L+1))
          SA0       X3                            A0=(T(1))
***       JP        CPY
 CPY      BSS       0
 TAU      IFNE      TYPE,CTYPE
          "P"VCPY   B1,A0,B6,X1,B5,B7
 TAU      ELSE
          DVCPY     B1,A0,B6,X1,B5,B7
 TAU      ENDIF
 CPYX     BSS       1                   EXIT LINE (SET ONCE ONLY)
          JP        B7
 [8]      BSS       0
 [9]      BSS       0
          SA3       A3+B7               X3=(T1)
          IX6       X2-X0               X6=(X21)-(X)
          SB6       LENGTH                                  B6=JT
          SB3       X6                                      B3=IX
          BX6       X0                  X6=(X)
          SX7       B1                  X7=M
          SB1       B1+1                                    B1=L+1
          SA6       Z11                 Z11=(X(1,1))
          SA6       XI1                 XI1=(X(1,1))
          SA6       XII                 XII=(X(1,1))
          SA7       L                   L=M
          SA7       M                   M=M
          BX6       X3                  X6=(T1)
          SA0       X3                            A0=(T(1))
          SA6       T1                  T1=(T(1))
*         DO   I  =  1, M
*         CALL TVSQR(N,X(I,1),X(I,2),T(1))
          SB7       [10]
          RJ        CPYX
          SA5       L                   X5=L
          SA1       XI1                 X1=(X(I,1))
          NZ        X5,SQR              IF L .NE. 0 CALL TVSQR
          JP        [13]
*         IF  I .LT. M
*         CALL TMMPY(L-1,N,X(I+1,1),X(I+1,2),X(I+2,1),X(I,1),X(I,2),
*                                                     T(2),T(3))
 [10]     SB7       [11]
          RJ        SQRX
          SA2       XI1                 X2=(X(I,1)) A2=XI1
          SB1       B1-2                                    B1=L-1
          SA0       A0+B6                         A0=(T(2))
          ZR        B1,[12]             IF L=1
          SX6       X2+B3               X6=(X(I+1,1))
          SX1       X2+B3               X1=(X(I+1,1))
          SA6       A2                  XI1=(X(I+1,1))
          JP        MPY
*         CALL TVCPY(L,T(1),T(2),X(I,I),X(I,I+1))
 [11]     SB7       SQR
          RJ        MPYX
 [12]     SA5       L                   X5=L      A5=L
          SA1       XII                 X1=(X(I,I)) A1=XII
          SA3       T1                  X3=(T(1))
          SB7       B3+B4                                   B7=IX+JX
          SX6       X5-1                X6=L-1
          SB1       X5                                      B1=L
          SA6       A5                  L=L-1
          SX7       X1+B7               X7=(X(I+1,I+1))
          SA0       X3                            A0=(T(1))
          SA7       A1                  XII=(X(I+1,I+1))
          JP        CPY
*         CALL TMUTL(M,X,X(1,2),X(2,1))
 [13]     SA5       M                   X5=M
          SA1       Z11                 X1=(X(1,1))
          SB1       X5-1                                    B1=M-1
          ZR        B1,EPILOG           IF M>1 CALL TMUTL
 UTL      TMUTL     TYPE,B1,B3,B4,B7    X1=(Z(1,1))
          EPILOG
          ENDM
