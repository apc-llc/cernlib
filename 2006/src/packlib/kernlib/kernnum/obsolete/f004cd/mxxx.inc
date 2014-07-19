*
* $Id: mxxx.inc,v 1.1.1.1 1996/02/15 17:48:10 mclareni Exp $
*
* $Log: mxxx.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:10  mclareni
* Kernlib
*
*
*
* mxxx.inc
*
*CALL     BASIC
*CALL     SCA
*CALL     MPY
 MMLA     SET       1          XY+ZZ
 MMLS     SET       2          XY-ZZ
 MNMA     SET       3         -XY+ZZ
 MNMS     SET       4         -XY-ZZ
 MAIN     MACRO     TYPE,FCN
          SETYPE    TYPE
          IFEQ      FCN,MMLA,3
 SIGNX    MICRO     1,,*+*
 SIGNZ    MICRO     1,,*+*
 VERB     MICRO     1,,*MMLA*
          IFEQ      FCN,MMLS,3
 SIGNX    MICRO     1,,*+*
 SIGNZ    MICRO     1,,*-*
 VERB     MICRO     1,,*MMLS*
          IFEQ      FCN,MNMA,3
 SIGNX    MICRO     1,,*-*
 SIGNZ    MICRO     1,,*+*
 VERB     MICRO     1,,*MNMA*
          IFEQ      FCN,MNMS,3
 SIGNX    MICRO     1,,*-*
 SIGNZ    MICRO     1,,*-*
 VERB     MICRO     1,,*MNMS*
 NAME     MICRO     1,,*"P"_"VERB"*
          HEADER    "NAME",SAVEA0
 TEMP     BSS       2         USED BY TMMPYY
 M        BSS       1
 L        BSS       1
 X11      BSS       1
 Y1L      BSS       1
 LY       BSS       1
 Z1L      BSS       1
 LZ       BSS       1
          EXIT      "NAME"         (M,N,X,X12,X21,Y,Y12,Y21,Z,Z12,Z21)
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
          LE        B1,B0,EXIT          IF M .LE. 0 RETURN
          LE        B2,B0,EXIT          IF N .LE. 0 RETURN
          LE        B3,B0,EXIT          IF K .LE. 0 RETURN
          SA6       SAVEA0              SAVEA0=OLD A0
          IX4       X4-X0               X4=(X12)-(X)=JX
          IX3       X3-X7               X3=(Y21)-(Y)=JY
          SB4       X4                                      B4=JX
          SB5       X3                                      B5=JY
          SA4       A1+B7               X4=(Z12)  A4=((Z12))
          SB6       B1-B3                                   B6=M-K
          SA3       A4+B7               X3=(Z21)  A3=((Z21))
          GE        B6,B0,MGEK          IF M.LT.K EXCHANGE ....
          SB6       B1        M AND K
          SB1       B3
          SB3       B6
          BX6       X0        (X11) AND (Y11)))
          BX0       X7
          BX7       X6
          SB6       B4        JX AND JYYY
          SB4       B5
          SB5       B6
          BX6       X2        (X21) AND (Y12)))
          BX2       X5
          BX5       X6
          BX6       X4        (Z12) AND (Z21)))
          BX4       X3
          BX3       X6
 MGEK     BSS       0         M .GE. KK
*                                       X0=(X)
*                                       X1=(Z)              B1=M
*                                       X2=(X21)            B2=N
*                                       X3=(Z21) A3=((Z21)) B3=K
*                                       X4=(Z12)            B4=JX
*                                       X5=(Y12)            B5=JY
*                                       X7=(Y)              B7=1
          SX6       B3                  X6=K
          SA0       X1                            A0=(Z(1,1))
          SA6       L                   L=K
          IX6       X2-X0               X6=(X21)-(X)=IX
          BX2       X7                  X2=(Y(1,1))
          SB3       X6                                      B3=IX
          IX6       X5-X2               X6=(Y12)-(Y)=LY
          IX7       X4-X1               X7=(Z12)-(Z)=LZ
          SA6       LY                  LY=(Y(1,2))-(Y(1,1))
          SA7       LZ                  LZ=(Z(1,2))-(Z(1,1))
          BX6       X5                  X6=(Y(1,2))
          BX7       X4                  X7=(Z(1,2))
          SA6       Y1L                 Y1L=(Y(1,2))
          SA7       Z1L                 Z1L=(Z(1,2))
          BX6       X0                  X6=(X)
          SX7       B1                  X7=M
          SA6       X11                 X11=(X(1,1))
          SA7       M                   M=M
          IX6       X3-X1               X6=(Z21)-(Z)=IZ
          BX1       X0                  X1=(X(1,1))
          SB6       X6                                      B6=IZ
 MXX      "P"MMPY   "SIGNX",B1,B2,B3,B4,B5,"SIGNZ",A0,B6,TEMP,B7
*                                    X1=(X(1,1)), X2=(Y(1,L))
          SA5       L                   X5=L      A5=L
          SA4       M                   X4=M
          SA3       Z1L                 X3=(Z(1,L+1)) A3=Z1L
          SA2       Y1L                 X2=(Y(1,L+1)) A2=Y1L
          SA1       X11                 X1=(X(1,1)) A1=X11
          SX6       X5-1                X6=L-1
          SB1       X4                                      B1=M
          SA6       A5                  L=L-1
          SA5       LZ                  X5=LZ
          SA4       LY                  X4=LY
          ZR        X6,EPILOG           IF L=0 RETURN
          SA0       X3                            A0=(Z(1,L+1))
          IX7       X3+X5               X7=(Z(1,L+2))
          IX6       X2+X4               X6=(Y(1,L+2))
          SA7       A3                  Z1L=(Z(1,L+2))
          SA6       A2                  Y1L=(Y(1,L+2))
          JP        MXX
          EPILOG
          ENDM
