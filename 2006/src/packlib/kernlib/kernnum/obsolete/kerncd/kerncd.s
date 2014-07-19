*
* $Id: kerncd.s,v 1.1.1.1 1996/02/15 17:48:14 mclareni Exp $
*
* $Log: kerncd.s,v $
* Revision 1.1.1.1  1996/02/15 17:48:14  mclareni
* Kernlib
*
*
* This directory was created from kernnum.car patch kerncd
*CD       BASIC
 RTYPE    SET       10
 DTYPE    SET       20
 CTYPE    SET       30
 PLUS     MICRO     1,,*+*
 MINUS    MICRO     1,,*-*
 NULL     MICRO     1,,*0*
 SETYPE   MACRO     TYPE
          IFEQ      TYPE,RTYPE,3
 P        MICRO     1,,*R*
 S        MICRO     0
 LENGTH   SET       1
          IFEQ      TYPE,DTYPE,3
 P        MICRO     1,,*D*
 S        MICRO     0
 LENGTH   SET       2
          IFEQ      TYPE,CTYPE,3
 P        MICRO     1,,*C*
 S        MICRO     0
 LENGTH   SET       2
 SETYPE   ENDM
 HEADER   MACRO     ID,SAVEA0
          ENTRY     ID
 TWORD    VFD       42/0H_ID,18/ID
          IFC       NE,=SAVEA0==,7
 SAVEA0   BSS       1
 SETX1    RMT
          SA1       SAVEA0              LOAD OLD A0 INTO X1
 SETX1    RMT
 SETA0    RMT
          SA0       X1                  RESTORE OLD A0 FROM X1
 SETA0    RMT
 HEADER   ENDM
 EXIT     MACRO     ID
 RESTORE  BSS       0
 SETA0    HERE
 EXIT     BSS       0
 ID       JP        *+1S17
 EXIT     ENDM
 EPILOG   MACRO
 EPILOG   BSS       0
 SETX1    HERE                          X1=OLD A0
          JP        RESTORE
 EPILOG   ENDM
 BRANCH   MACRO     LABEL,N.OP.K,J,I,M
          S_J       -N        0         J = -N
          IFC       NE,=I==,1           IF I .NE. NULL THEN
          S_I       -M        1                   I = -M
          S_J       K+J       2         J = K-N
*                             3 J
*                             4
*                             5
          OP        B0,J,LABEL          IF N.OP.K GO TO LABEL
 BRANCH   ENDM
 RESIDU   MACRO     A,X,IX,JX,B,Y,IY,JY,C,Z,IZ,JZ
          IFC       NE,*A**,1
          S_X       A+JX
          IFC       NE,*B**,1
          S_Y       B+JY
          IFC       NE,*C**,1
          S_Z       C+JZ
          IFC       NE,*A**,1
          S_IX      -IX
          IFC       NE,*B**,1
          S_IY      -IY
          IFC       NE,*C**,1
          S_IZ      -IZ
 RESIDU   ENDM
 STOREF   MACRO     TYPE,Y
          IFEQ      TYPE,RTYPE,1
          SA6       Y
          IFNE      TYPE,RTYPE,2
          SA6       Y
          SA7       Y+1
 STOREF   ENDM
 SWAP     MACRO     P,Q
          SX6       P
          S_P       Q
          S_Q       X6
 SWAP     ENDM
*CD       ADD
 RADD1R   MACRO     JX,&,JY,Z,JZ,J
          LOCAL     [1]
 [1]      BSS       0                   FOR K = 0, 1, 2, ...
          S_J       J+1       0 X5                          J=2-N+1+K
          RX1       X3_&_X5   1         X1=X(2+K) & Y(2+K)
          SA3       A3+JX     2         X3=X(3+K)
 +        SA5       A5+JY     3         X5=Y(3+K)
          SA7       Z         4 X1      Z(1+K)
          NX7       X1        5         X7=Z(2+K)
          S_Z       Z+JZ      6         Z=(Z(2+K))
*                             7 X7
*                             8
 +        MI        J,[1]     9 X3      WHILE 3+K-N<0; I.E. 4+K .LE. N
 RADD1R   ENDM
 RADD4R   MACRO     JX,&,JY,JZ,J
          LOCAL     [1]
 [1]      BSS       0                   FOR K= 0, 4, 8, ...
 +        RX0       X2_&_X4   0 X3      X0=X(4+K) & Y(4+K)
          SA6       A7+JZ     1 X5      Z(2+K)
          SA2       A3+JX     2         X2=X(6+K)
          SA4       A5+JY     3 X0      X4=Y(6+K)
 +        NX6       X0        4         X6=Z(4+K)
          RX1       X3_&_X5   5         X1=X(5+K) & Y(5+K)
          SA3       A2+JX     6 X6      X3=X(7+K)
          SA7       A6+JZ     7         Z(3+K)
 +        SA5       A4+JY     8 X1      X5=Y(7+K)
          S_J       J+4       9 X2                          J=8-N+4+K
          NX7       X1       10 X4      X7=Z(5+K)
 +        RX0       X2_&_X4  11         X0=X(6+K) & Y(6+K)
          SA6       A7+JZ    12 X7      Z(4+K)
          SA2       A3+JX    13 X3      X2=X(8+K)
          SA4       A5+JY    14 X0      X4=Y(8+K)
 +        NX6       X0       15 X5      X6=Z(6+K)
          RX1       X3_&_X5  16         X1=X(7+K) & Y(7+K)
          SA3       A2+JX    17 X6      X3=X(9+K)
          SA5       A4+JY    18         X5=Y(9+K)
 +        SA7       A6+JZ    19 X1      Z(5+K)
          NX7       X1       20 X2      X7=Z(7+K)
*                            21 X4
*                            22 X7
          MI        J,[1]    23         WHILE 12-N+K<0; I.E. 13+K .LE. N
 RADD4R   ENDM
 RVADD    MACRO     N,JX,&,JY,Z,JZ,J    X2=(X),   X4=(Y)
          LOCAL     [1],[2],[3],[4],[5],[99]
          SA3       X2                  X3=X(1)   A3=(X(1))
          SA5       X4                  X5=Y(1)   A5=(Y(1))
          BRANCH    [3],N,LE,8,J                            J=8-N
          SA2       A3+JX               X2=X(2)   A2=(X(2))
          SA4       A5+JY               X4=Y(2)   A4=(Y(2))
          RX7       X3_&_X5             X7=X(1)&Y(1)
          SA3       A2+JX               X3=X(3)   A3=(X(3))
          SA5       A4+JY               X5=Y(3)   A5=(Y(3))
          NX7       X7                  X7=Z(1)
          RX6       X2_&_X4             X6=X(2)&Y(2)
          SA2       A3+JX               X2=X(4)   A2=(X(4))
          SA4       A5+JY               X4=Y(4)   A4=(Y(4))
          NX6       X6                  X6=Z(2)
          RX1       X3_&_X5             X1=X(3)&Y(3)
          SA3       A2+JX               X3=X(5)   A3=(X(5))
          SA5       A4+JY               X5=Y(5)   A5=(Y(5))
          SA7       Z                   Z(1)=X(1)&Y(1)
          NX7       X1                  X7=Z(3)
          RADD4R    JX,&,JY,JZ,J        J=0,1,2,3 FOR N=12,11,10,9
          RX1       X2_&_X4             X1=X(8)&Y(8)
          SA6       A7+JZ               Z(6)=X(6)&Y(6)
          S_J       J-3                 -J FETCHES LEFT
          SA7       A6+JZ               Z(7)=X(7)&Y(7)
          ZR        J,[2]               IF N=9,13,17,...
 [1]      NX7       X1                  X7=Z(8+K)
          RX1       X3_&_X5             X1=X(9+K)&Y(9+K)
          SA3       A3+JX               X3=X(10+K) A3=(X(10+K))
          SA5       A5+JY               X5=Y(10+K) A5=(Y(10+K))
          S_J       J+1
          SA7       A7+JZ               Z(8+K)=X(8+K)&Y(8+K)
          MI        J,[1]               WHILE J<0
 [2]      NX7       X1                  X7=Z(N-1)
          RX1       X3_&_X5             X1=X(N)&Y(N)
          SA7       A7+JZ               Z(N-1)=X(N-1)&Y(N-1)
          NX7       X1                  X7=Z(N)
          SA7       A7+JZ               Z(N)=X(N)&Y(N)
          JP        [99]
 [3]      BSS       0                   X3=X(1) X5=Y(1)
          S_J       J-6                                     J=2-N
          RX1       X3_&_X5             X1=X(1)&Y(1)
          GT        J,B0,[5]            IF N=1
          SA3       A3+JX               X3=X(2)   A3=(X(2))
          SA5       A5+JY               X5=Y(2)   A5=(Y(2))
          NX7       X1                  X7=Z(1)
          ZR        J,[4]               IF N=2
          RADD1R    JX,&,JY,Z,JZ,J                          J=0
 [4]      RX1       X3_&_X5             X1=X(N)&Y(N)
          SA7       Z                   Z(N-1)=X(N-1)&Y(N-1)
          S_Z       Z+JZ                Z=(Z(N))
 [5]      NX7       X1                  X7=Z(N)
          SB0       0
          SA7       Z                   Z(N)=X(N)&Y(N)
 [99]     BSS       0
 RVADD    ENDM
 RMADD    MACRO     M,N,JX,&,JY,Z,IZ,JZ,J,TEMP6
*                                       X2=(X)
*                                       X3=IX
*                                       X4=(Y)
*                                       X5=IY
          LOCAL     [1],[2],[3],[4],[5],[6],[7],[99]
          LOCAL     SAVEM,SAVEX,SAVEIX,SAVEY,SAVEIY,SAVEZ
 SAVEM    EQU       TEMP6
 SAVEX    EQU       SAVEM+1
 SAVEIX   EQU       SAVEX+1
 SAVEY    EQU       SAVEIX+1
 SAVEIY   EQU       SAVEY+1
 SAVEZ    EQU       SAVEIY+1
          LE        M,N,[2]             IF M .LE. N
 [1]      SWAP      M,N                 EXCHANGE M  AND N
          SWAP      X3,JX               EXCHANGE IX AND JX
          SWAP      X5,JY               EXCHANGE IY AND JY
          SWAP      IZ,JZ               EXCHANGE IZ AND JZ
 [2]      BRANCH    [4],N,LE,8,J        J=8-N
          BX6       X3                  X6=IX
          BX7       X5                  X7=IY
          SA6       SAVEIX              SAVEIX=IX
          SX6       M                   X6=M
          SA7       SAVEIY              SAVEIY=IY
          SA6       SAVEM               SAVEM=M
          S_M       -M                                      M=-M
 [3]      IX6       X2+X3               X6=(X(2,1))
          IX7       X4+X5               X7=(Y(2,1))
          SA3       X2                  X3=X(1,1)
          SA5       X4                  X5=Y(1,1)
          SA2       A3+JX               X2=X(1,2)
          SA4       A5+JY               X4=Y(1,2)
          SA6       SAVEX               SAVEX=(X(2,1))
          SA7       SAVEY               SAVEY=(Y(2,1))
          RX7       X3_&_X5             X7=X(1,1) & Y(1,1)
          SA3       A2+JX               X3=X(1,3)
          SA5       A4+JY               X5=Y(1,3)
          RX6       X2_&_X4             X6=X(1,2) & Y(1,2)
          SA2       A3+JX               X2=X(1,4)
          SA4       A5+JY               X4=Y(1,4)
          NX7       X7                  X7=Z(1,1)
          RX1       X3_&_X5             X1=X(1,3) & Y(1,3)
          SA3       A2+JX               X3=X(1,5)
          SA5       A4+JY               X5=Y(1,5)
          NX6       X6                  X6=Z(1,2)
          SA7       Z                   Z(1,1)
          NX7       X1                  X7=Z(1,3)
          S_J       -N
          S_J       J+8                                     J=8-N
          RADD4R     JX,&,JY,JZ,J       J=3,2,1,0 FOR N=0,1,2,3 ELEM. LEFT
          RX0       X2_&_X4             X0=X(1,N-4+J) & Y(1,N-4+J)
          SA6       A7+JZ               Z(1,N-6+J)
          SA2       SAVEX               X2=(X(2,1))
          SA4       SAVEY               X4=(Y(2,1))
          NX6       X0                  X6=Z(1,N-4+J)
          RX1       X3_&_X5             X1=X(1,N-3+J) & Y(1,N-3+J)
          SX0       A3                  X0=(LAST X)
          SA7       A6+JZ               Z(1,N-5+J)
          S_Z       Z+IZ                Z=(Z(2,1))
          NX7       X1                  X7=Z(1,N-3+J)
          S_M       M+1                                     M=M+1
          SX1       A5                  J=(LAST Y)
          SA6       A7+JZ               Z(1,N-4+J)
          SA3       SAVEIX              X3=IX
          SA5       SAVEIY              X5=IY
          SA7       A6+JZ               Z(1,N-3+J)
          MI        M,[3]               WHILE M<0
          BRANCH    [99],J,EQ,3,N       N=3-J=0,1,2,3
          SA3       SAVEIX              X3=IX
          SA5       SAVEIY              X5=IY
          SX2       X0+JX               X2=(X(M,NEW 1))
          SX4       X1+JY               X4=(Y(M,NEW 1))
          SA1       SAVEM               X1=M
          S_Z       A7+JZ               Z =(Z(M,NEW 1))
          S_IZ      -IZ                 TO WORK FROM LAST ROW UPWARDS
          BX3       -X3
          BX5       -X5
          S_M       X1                  RESTORE M
          GT        M,N,[1]
 [4]      BSS       0
          BX6       X3                  X6=IX
          BX7       X5                  X7=IY
          SA6       SAVEIX              SAVEIX=IX
          SA7       SAVEIY              SAVEIY=IY
          S_M       -M                                      M=-M
 [5]      IX6       X2+X3               X6=(X(2,1))
          IX7       X4+X5               X7=(Y(2,1))
          SA3       X2                  X3=X(1,1)
          SA5       X4                  X5=Y(1,1)
          SA6       SAVEX               SAVEX=(X(2,1))
          SA7       SAVEY               SAVEY=(Y(2,1))
          S_J       -N
          SX6       Z+IZ                X6=(Z(2,1))
          S_J       J+2                 J=2-N=+1,0,-1,...; N=1,2,3,...
          S_M       M+1                                     M=M+1
          RX1       X3_&_X5             X1=X(1,1) & Y(1,1)
          SA6       SAVEZ               SAVEZ=(Z(2,1))
          GT        J,B0,[7]            IF N=1
          SA3       A3+JX               X3=X(1,2)
          SA5       A5+JY               X5=Y(1,2)
          NX7       X1                  X7=Z(1,1)
          ZR        J,[6]               IF N=2
          RADD1R    JX,&,JY,Z,JZ,J
 [6]      RX1       X3_&_X5             X1=X(1,N) & Y(1,N)
          SA7       Z                   Z(1,N-1)
          S_Z       Z+JZ                Z=(Z(1,N))
 [7]      NX7       X1                  X7=Z(1,N)
          SA1       SAVEZ               X1=(Z(2,1))
          SA2       SAVEX               X2=(X(2,1))
          SA4       SAVEY               X4=(Y(2,1))
          SA3       SAVEIX              X3=IX
          SA5       SAVEIY              X5=IY
          SA7       Z                   Z(1,N)
          S_Z       X1                  Z=(Z(2,1))
          MI        M,[5]
 [99]     BSS       0
 RMADD    ENDM
 DADD1R   MACRO     JX,&,JY,JZ,J
          LOCAL     [1]
 [1]      BSS       0                   FOR K = 0, 1, 2, ...
          FX5       X3_&_X5   0 X4      X5=U(X'' & Y'')=A'(3+K)
          DX1       X2_&_X4   1         X1=L(X' & Y')=B''(3+K)
          DX7       X6+X0     2         X7=L(E+F)=Z''(2+K)
          FX6       X6+X0     3 X5      X6=U(E+F)=Z'(2+K)
 +        FX4       X2_&_X4   4 X1      X4=U(X' & Y')=B'(3+K)
          FX0       X5+X1     5 X7      X0=U(A'+B'')=C'(3+K)
          SA7       A7+JZ     6 X6      Z''(2+K)
          SA3       A3+JX     7 X4      X3=X''(4+K)
 +        SA5       A5+JY     8 X0      X5=Y''(4+K)
          FX1       X4+X0     9         X1=U(B'+C')=D'(3+K)
          DX0       X4+X0    10         X0=L(B'+C')=D''(3+K)
          SA6       A6+JZ    11         Z'(2+K)
 +        SA2       A2+JX    12 X1      X2=X'(4+K)
          SA4       A4+JY    13 X0      X4=Y'(4+K)
          S_J       J+1      14 X3                          J=3-N+1+K
 +        NX6       X1       15 X5      X6=E(3+K)
          NX0       X0       16         X0=F(3+K)
*                            17 X6
*                            18 X0
          MI        J,[1]    19 X2      WHILE 4-N+K<0; I.E. 5+K .LE. N
 DADD1R   ENDM
 DMADD    MACRO     M,N,JX,&,JY,Z,IZ,JZ,J,TEMP4
*                                       X2=(X)
*                                       X3=IX
*                                       X4=(Y)
*                                       X5=IY
          LOCAL     [1],[2],[3],[4],[99]
          LOCAL     SAVEX,SAVEIX,SAVEY,SAVEIY
 SAVEX    EQU       TEMP4
 SAVEIX   EQU       SAVEX+1
 SAVEY    EQU       SAVEIX+1
 SAVEIY   EQU       SAVEY+1
          LE        M,N,[1]             IF M .LE. N
          SWAP      M,N                 EXCHANGE M  AND N
          SWAP      X3,JX               EXCHANGE IX AND JX
          SWAP      X5,JY               EXCHANGE IY AND JY
          SWAP      IZ,JZ               EXCHANGE IZ AND JZ
 [1]      BX6       X3                  X6=IX
          BX7       X5                  X7=IY
          S_M       -M                                      M=-M
          SA6       SAVEIX              SAVEIX=IX
          SA7       SAVEIY              SAVEIY=IY
 [2]      IX6       X2+X3               X6=(X'(2,1))
          IX7       X4+X5               X7=(Y'(2,1))
          SA3       X2+1                X3=X''(1,1)
          SA5       X4+1                X5=Y''(1,1)
          SA2       X2                  X2=X'(1,1)
          SA4       X4                  X4=Y'(1,1)
          SA6       SAVEX               SAVEX=(X'(2,1))
          SA7       SAVEY               SAVEY=(Y'(2,1))
          S_J       1                                       J=1
          FX5       X3_&_X5             X5=U(X'' & Y'')=A'(1)
          DX1       X2_&_X4             X1=L(X' & Y')=B''(1)
          FX4       X2_&_X4             X4=U(X' & Y')=B'(1)
          FX0       X5+X1               X0=U(A'+B'')=C'(1)
          FX1       X4+X0               X1=U(B'+C')=D'(1)
          DX0       X4+X0               X0=L(B'+D')=D''(1)
          NX6       X1                  X6=E(1)
          NX0       X0                  X0=F(1)
          DX7       X6+X0               X7=L(E+F)=Z''(1)
          FX6       X6+X0               X6=U(E+F)=Z'(1)
          SA7       Z+J                 Z''(1)
          SA6       Z                   Z'(1)
          EQ        J,N,[99]            IF N=1 THEN M=1
          SA3       A3+JX               X3=X''(1,2)
          SA5       A5+JY               X5=Y''(1,2)
          SA2       A2+JX               X2=X'(1,2)
          SA4       A4+JY               X4=Y'(1,2)
          S_J       -N
          S_M       M+1                                     M=M+1
          S_J       J+3                 J=3-N=+1,0,-1,...; N=2,3,4,...
          FX5       X3_&_X5             X5=U(X'' & Y'')=A'(2)
          DX1       X2_&_X4             X1=L(X' & Y')=B''(2)
          FX4       X2_&_X4             X4=U(X' & Y')=B'(2)
          FX0       X5+X1               X0=U(A'+B'')=C'(2)
          FX1       X4+X0               X1=U(B'+C')=D'(2)
          DX0       X4+X0               X0=L(B'+D')=D''(2)
          NX6       X1                  X6=E(2)
          NX0       X0                  X0=F(2)
          GT        J,B0,[4]            IF N=2
          SA3       A3+JX               X3=X''(1,3)
          SA5       A5+JY               X5=Y''(1,3)
          SA2       A2+JX               X2=X'(1,3)
          SA4       A4+JY               X4=Y'(1,3)
          ZR        J,[3]               IF N=3
          DADD1R    JX,&,JY,JZ,J        J=0
 [3]      FX5       X3_&_X5             X5=U(X'' & Y'')=A'(N)
          DX1       X2_&_X4             X1=L(X' & Y')=B''(N)
          DX7       X6+X0               X7=L(E+F)=Z''(N-1)
          FX6       X6+X0               X6=U(E+F)=Z'(N-1)
          FX4       X2_&_X4             X4=U(X' & Y')=B'(N)
          FX0       X5+X1               X0=U(A'+B'')=C'(N)
          SA7       A7+JZ               Z''(N-1)
          FX1       X4+X0               X1=U(B'+C')=D'(N)
          DX0       X4+X0               X0=L(B'+D')=D''(N)
          SA6       A6+JZ               Z'(N-1)
          NX6       X1                  X6=E(N)
          NX0       X0                  X0=F(N)
 [4]      SA2       SAVEX               X2=(X'(2,1))
          SA4       SAVEY               X4=(Y'(2,1))
          DX7       X6+X0               X7=L(E+F)=Z''(N)
          FX6       X6+X0               X6=U(E+F)=Z'(N)
          SA3       SAVEIX              X3=IX
          SA5       SAVEIY              X5=IY
          S_Z       Z+IZ                Z=(Z'(2,1))
          SA7       A7+JZ               Z''(N)
          SA6       A6+JZ               Z'(N)
          MI        M,[2]               WHILE M<0
 [99]     BSS       0
 DMADD    ENDM
*CD       CPY
 RCPY2R   MACRO     JX,JY,J
          LOCAL     [1]
 [1]      BSS       0                   FOR K = 0, 2, 4,...
 +        BX6       X4        0 X5      X6=X(2+K)
          SA4       A5+JX     1 X6      X4=X(4+K)
          BX7       X5        2         X7=X(3+K)
          SA5       A4+JX     3 X7      X5=X(5+K)
 +        SA6       A7+JY     4         Y(2+K)=X(2+K)
          S_J       J+2       5                             J=4-N+2+K
          SA7       A6+JY     6         Y(3+K)=X(3+K)
                              7
                              8 X4
 +        MI        J,[1]     9         WHILE 6-N+K=J<0; I.E. 7+K .LE. N
 RCPY2R   ENDM
 RCPY12R  MACRO     JX,JY,J
          LOCAL     [1]
 [1]      BSS       0                   FOR K = 0, 12, 24, ...
 +        BX6       X2        0 X4      X6=X(2+K)
          S_J       J+12      1 X6                          J=16-N+12+K
          BX7       X3        2 X5      X7=X(3+K)
 +        SA2       A5+JX     3 X7      X2=X(6+K)
          SA6       A7+JY     4         Y(2+K)=X(2+K)
          SA3       A2+JX     5         X3=X(7+K)
          SA7       A6+JY     6         Y(3+K)=X(3+K)
 +        BX6       X4        7         X6=X(4+K)
          BX7       X5        8 X6      X7=X(5+K)
          SA4       A3+JX     9 X7      X4=X(8+K)
          SA6       A7+JY    10 X2      Y(4+K)=X(4+K)
 +        SA5       A4+JX    11         X5=X(9+K)
          SA7       A6+JY    12 X3      Y(5+K)=X(5+K)
          BX6       X2       13         X6=X(6+K)
          BX7       X3       14 X6      X7=X(7+K)
 +        SA2       A5+JX    15 X7      X2=X(10+K)
          SA6       A7+JY    16 X4      Y(6+K)=X(6+K)
          SA3       A2+JX    17         X3=X(11+K)
          SA7       A6+JY    18 X5      Y(7+K)=X(7+K)
 +        BX6       X4       19         X6=X(8+K)
          BX7       X5       20 X6      X7=X(9+K)
          SA4       A3+JX    21 X7      X4=X(12+K)
          SA6       A7+JY    22 X2      Y(8+K)=X(8+K)
 +        SA5       A4+JX    23         X5=X(13+K)
          SA7       A6+JY    24 X3      Y(9+K)=X(9+K)
          BX6       X2       25         X6=X(10+K)
          BX7       X3       26 X6      X7=X(11+K)
 +        SA2       A5+JX    27 X7      X2=X(14+K)
          SA6       A7+JY    28 X4      Y(10+K)=X(10+K)
          SA3       A2+JX    29         X3=X(15+K)
          SA7       A6+JY    30 X5      Y(11+K)=X(11+K)
 +        BX6       X4       31         X6=X(12+K)
          BX7       X5       32 X6      X7=X(13+K)
          SA4       A3+JX    33 X7      X4=X(16+K)
          SA6       A7+JY    34 X2      Y(12+K)=X(12+K)
 +        SA5       A4+JX    35         X5=X(17+K)
          SA7       A6+JY    36 X3      Y(13+K)=X(13+K)
                             37
                             38
          MI        J,[1]    39         WHILE 28-N+K=J<0; I.E. 29+K.LE.N
 RCPY12R  ENDM
 RVCPY    MACRO     N,X,JX,Y,JY,J
          LOCAL     [1],[2],[3],[4],[5],[6],[99]
          BRANCH    [2],N,LE,16,J                             16-N
 [1]      SA5       X         5         X5=X(1)
          SA2       X+JX      6         X2=X(2)
                              7
          SA3       A2+JX     8         X3=X(3)
                              9 A3
          SB0       A3       10       AVOIDS CONFLICT AT CP 1 OF RCPY12R
 +        SA4       A3+JX    11         X4=X(4)
                             12 X5
          BX7       X5       13 X2      X7=X(1)
          SA5       A4+JX    14 X7      X5=X(5)
          SA7       Y        15 X3                A7=(Y(1))
          RCPY12R   JX,JY,J
 +        S_J       J-10     39                   J=1,0,-1,-2,...,-10
          BX6       X2       40 X4      X6=X(J+N-4)
                             41 X6
          BX7       X3       42 X5      X7=X(J+N-3)
 +        SA6       A7+JY    43 X7                A6=(Y(J+N-4))
          SB0       0        44
          SA7       A6+JY    45                   A7=(Y(J+N-3))
                             46
 +        ZR        J,[5]    47         IF 1 ELEMENT LEFT
                             48
          PL        J,[6]    49         IF NO ELEMENT LEFT
          RCPY2R    JX,JY,J
                              9                             J=1,0
          NZ        J,[6]    10 X5      IF NO ELEMENT LEFT
 +        BX7       X4       11         X7=X(N-2)
          BX4       X5       12 X7      X4=X(N-1)
          SA5       A5+JX    13 X4      X5=X(N)
          SA7       A7+JY    14                   A7=(Y(N-2))
*                            15
*                            16
 +        JP        [6]      17
 [2]      BSS       0                   J=16-N=15,14,13,...,2,1,0
          SA3       X        13         X3=X(1)
          SX6       N-2      14         X6=N-2
          NO                 15 X6
                             16
 +        MI        X6,[3]   17         IF N=1 (I.E. J=15)
          NO                 18
          SA4       X+JX     19         X4=X(2)
                             20 X3
                             21
 +        ZR        X6,[4]   22         IF N=2 (I.E.J=14)
          SA5       A4+JX    23         X5=X(3)
          BX7       X3       24         X7=X(1)
 +        S_J       J-12     25 X7               J=1,0,-1,-2,...,-11,-12
          SA7       Y        26 X4                A7=(Y(1))
          NO                 27
                             28
 +        GT        J,B0,[6] 29         IF N=3 (I.E. J=1)
                             30 X5
          ZR        J,[5]    31         IF N=4 (I.E. J=0)
          RCPY2R    JX,JY,J
          NZ        J,[6]    10 X5      IF NO ELEMENT LEFT
 +        BX7       X4       11         X7=X(N-2)
          BX4       X5       12 X7      X4=X(N-1)
          SA5       A5+JX    13 X4      X5=X(N)
          SA7       A7+JY    14                   A7=(Y(N-2))
                             15
                             16
 +        JP        [6]      17
 [3]      BX7       X3       26         X7=X(1)
          SA5       A3       27 X7                A5=(X(1))
          SA7       Y        28                   A7=(Y(1))
          NO                 29
*                            30
*                            31
          JP        [99]     32
 [4]      BX6       X3       32         X6=X(1)
          BX7       X4       33 X6      X7=X(2)
          SA6       Y        34 X7                A6=(Y(1))
          SA7       Y+JY     35                   A7=(Y(2))
 +        SA5       A4       36                   A5=(X(2))
*                            37
*                            38
          JP        [99]     39
 [5]      BX7       X4        0         X7=X(N-2)
          BX4       X5        1 X7      X4=X(N-1)
          SA5       A5+JX     2         X5=X(N)
          SA7       A7+JY     3                   A7=(Y(N-2))
 [6]      BX6       X4        0         X6=X(N-1)
          BX7       X5        1 X6      X7=X(N)
          SA6       A7+JY     2 X7                A6=(Y(N-1))
*                             3
          SA7       A6+JY     4                   A7=(Y(N))
 [99]     BSS       0
 RVCPY    ENDM
 RMCPY    MACRO     M,N,X,IX,JX,Y,IY,JY,I,J
*                                       X0=X      A0=Y
*                                       X1=I
          LOCAL     [1],[2],[3],[4],[5],[6],[7],[8],[9],[99]
          LE        M,N,[2]             IF M .LE. N
 [1]      SWAP      M,N                 EXCHANGE M AND N
          SWAP      IX,JX               EXCHANGE IX AND JX
          SWAP      IY,JY               EXCHANGE IY AND JY
 [2]      BRANCH    [4],N,LE,16,J,I,M   J=16-N, I=-M
 [3]      BSS       0              FOR K = 1, 2, ..., M  AND I = K-1-M
          SA5       X                   X5=X(K,1)
          SA2       X+JX                X2=X(K,2)
          SA3       A2+JX               X3=X(K,3)
          S_J       -N                                      J=-N
 +        S_I       I+1                                     I=K-M
          S_J       J+16                                    J=16-N
 +        BX7       X5                  X7=X(K,1)
          SA4       A3+JX               X4=X(K,4)
          SA7       Y                   Y(K,1)=X(K,1) A7=(Y(K,1))
          SA5       A4+JX               X5=X(K,5) A5=(X(K,5))
          RCPY12R   JX,JY,J
 +        S_Y       Y+IY      0         Y=(Y(K+1,1))
          BX6       X2        1         X6=X(K,N+J-14)
                              2
          BX7       X3        3         X7=X(K,N+J-13)
          SA6       A7+JY     4         Y(K,N+J-14)=X(K,N+J-14)
 +        BX6       X4        5         X6=X(K,N+J-12)
          SA7       A6+JY     6         Y(K,N+J-13)=X(K,N+J-13)
          BX7       X5        7         X7=X(K,N+J-11)
          SA6       A7+JY     8         Y(K,N+J-12)=X(K,N+J-12)
 +        S_X       X+IX      9         X=(X(K+1,1))
          SA7       A6+JY    10         Y(K,N+J-11)=X(K,N+J-11)
                             11
                             12
          MI        I,[3]    13         WHILE I<0
          BRANCH    [99],J,EQ,11,N      N=11-J
          RESIDU    A5,X,IX,JX,A7,Y,IY,JY
          GT        M,N,[1]
 [4]      BSS       0
          BRANCH    [7],N,GT,2,J,I,M    J=2-N=1,0,-1,...,-16; I=-M
          NZ        J,[6]               IF N=1
 [5]      SA4       X                   X4=X(K,1)
          SA5       X+JX                X5=X(K,2)
          S_X       X+IX                X=(X(K+1,1))
          S_I       I+1                                     I=I+1
          BX6       X4                  X6=X(K,1)
          BX7       X5                  X7=X(K,2)
          SA6       Y                   Y(K,1)=X(K,1)
          SA7       Y+JY                Y(K,2)=X(K,2)
          S_Y       Y+IY                Y=(Y(K+1,1))
          MI        I,[5]               WHILE I<M
          JP        [99]
 [6]      SA5       X                   X5=X(1,1)
          BX7       X5                  X7=X(1,1)
          SA7       Y                   Y(1,1)=X(1,1)
          JP        [99]
 [7]      BSS       0
          SA3       X                   X3=X(K,1)
          SA4       X+JX                X4=X(K,2)
          SA5       A4+JX               X5=X(K,3)
          S_J       -N                                      J=-N
          S_I       I+1                                     I=K-M
          S_J       J+4                                     J=4-N
 +        BX7       X3                  X7=X(K,1)
          S_X       X+IX                X=(X(K+1,1))
          SA7       Y                   Y(K,1)=X(K,1)
          S_Y       Y+IY                Y=(Y(K+1,1))
          ZR        J,[8]               IF N=4
          PL        J,[9]               IF N=3
          RCPY2R    JX,JY,J
          NZ        J,[9]
 [8]      BX7       X4
          BX4       X5
          SA5       A5+JX
          SA7       A7+JY
          NO
          NO
          NO
          NO
 [9]      BX6       X4
          BX7       X5
          SA6       A7+JY
          SA7       A6+JY
          MI        I,[7]
 [99]     BSS       0
 RMCPY    ENDM
 DCPY1R   MACRO     JX,JZ,J
          LOCAL     [1]
 [1]      BSS       0                   FOR K = 0, 1, 2,...
 +        BX6       X4        0 X5      X6=X'(2+K)
          SA4       A4+JX     1 X6      X4=X'(3+K)
          BX7       X5        2         X7=X"(2+K)
          SA5       A5+JX     3 X7      X5=X"(3+K)
 +        SA6       A6+JZ     4         Z'(2+K)=X'(2+K)
          S_J       J+1       5                             J=2-N+1+K
          SA7       A7+JZ     6         Z"(2+K)=X''(2+K)
                              7 X4
                              8
 +        MI        J,[1]     9         WHILE 3-N+K=J<0; I.E. 4+K .LE. N
 DCPY1R   ENDM
 DCPY6R   MACRO     JX,JZ,J
          LOCAL     [1]
 [1]      BSS       0                   FOR K = 0, 6, 12, ...
 +        BX6       X2        0 X4      X6=X'(2+K)
          S_J       J+6       1 X6                          J=8-N+6+K
          BX7       X3        2 X5      X7=X"(2+K)
 +        SA2       A4+JX     3 X7      X2=X'(4+K)
          SA6       A6+JZ     4         Z'(2+K)=X'(2+K)
          SA3       A5+JX     5         X3=X"(4+K)
          SA7       A7+JZ     6         Z"(2+K)=X''(2+K)
 +        BX6       X4        7         X6=X'(3+K)
          BX7       X5        8 X6      X7=X"(3+K)
          SA4       A2+JX     9 X7      X4=X'(5+K)
          SA6       A6+JZ    10 X2      Z(4+K)=X(4+K)
 +        SA5       A3+JX    11         X5=X"(5+K)
          SA7       A7+JZ    12 X3      Z(5+K)=X(5+K)
          BX6       X2       13         X6=X(6+K)
          BX7       X3       14 X6      X7=X(7+K)
 +        SA2       A4+JX    15 X7      X2=X(10+K)
          SA6       A6+JZ    16 X4      Z(6+K)=X(6+K)
          SA3       A5+JX    17         X3=X(11+K)
          SA7       A7+JZ    18 X5      Z(7+K)=X(7+K)
 +        BX6       X4       19         X6=X(8+K)
          BX7       X5       20 X6      X7=X(9+K)
          SA4       A2+JX    21 X7      X4=X(12+K)
          SA6       A6+JZ    22 X2      Z(8+K)=X(8+K)
 +        SA5       A3+JX    23         X5=X(13+K)
          SA7       A7+JZ    24 X3      Z(9+K)=X(9+K)
          BX6       X2       25         X6=X(10+K)
          BX7       X3       26 X6      X7=X(11+K)
 +        SA2       A4+JX    27 X7      X2=X(14+K)
          SA6       A6+JZ    28 X4      Z(10+K)=X(10+K)
          SA3       A5+JX    29         X3=X(15+K)
          SA7       A7+JZ    30 X5      Z(11+K)=X(11+K)
 +        BX6       X4       31         X6=X(12+K)
          BX7       X5       32 X6      X7=X(13+K)
          SA4       A2+JX    33 X7      X4=X(16+K)
          SA6       A6+JZ    34 X2      Z(12+K)=X(12+K)
 +        SA5       A3+JX    35         X5=X"(9+K)
          SA7       A7+JZ    36 X3      Z"(7+K)=X''(7+K)
                             37
                             38
          MI        J,[1]    39         WHILE 14-N+K=J<0; I.E. 15+K.LE.N
 DCPY6R   ENDM
 DVCPY    MACRO     N,X,JX,Y,JY,J
          LOCAL     [1],[2],[3],[4],[5],[6],[99]
          BRANCH    [4],N,LE,8,J                            J=8-N
 [1]      SA4       X         5         X4=X'(1)
          SA5       X+1       6         X5=X"(1)
                              7
          SA3       A5+JX     8         X3=X"(2)
 +        SA2       A4+JX     9         X2=X'(2)
                             10
                             11
                             12 X4
          BX6       X4       13 X5      X6=X'(1)
          SA4       A2+JX    14 X6      X4=X'(3)
          SA6       Y        15 X3                A6=(Y'(1))
 +        BX7       X5       16 X2      X7=X"(1)
          SA5       A3+JX    17 X7      X5=X"(3)
          SA7       Y+1      18                   A7=(Y"(1))
          DCPY6R    JX,JY,J   0
                              1 X6
                              2 X4
                              3 X7
                              4
                              5 X5
 +        S_J       J-5      39                   J=0,-1,-2,-3,-4,-5
          BX6       X2       40 X4      X6=X'(J+N-1)
                             41 X6
          BX7       X3       42 X5      X7=X"(J+N-1)
 +        SA6       A6+JY    43 X7                A6=(Y'(J+N-1))
          SA7       A7+JY    44                   A7=(Y"(J+N-1))
                             45
          MI        J,[5]    46         IF -J ELEMENTS LEFT
                             47
                             48
 +        JP        [6]      49
 [2]      BX7       X3        0         X7=X"(1)
          BX6       X2        1 X7      X6=X'(1)
          SA7       Y+1       2 X6                A7=(Y"(1))
 +        SA6       Y         3                   A6=(Y'(1))
          SA4       A2        4                   A4=(X'(1))
          SA5       A3        5                   A5=(X"(1))
                              6
                              7
          JP        [99]      8
 [3]      BX6       X2        0         X6=X'(1)
          BX7       X3        1         X7=X"(1)
          SA6       Y         2                   A6=(Y'(1))
          BX6       X4        3         X6=X'(2)
 +        SA7       Y+1       4                   A7=(Y"(1))
          BX7       X5        5         X7=X"(2)
          SA6       A6+JY     6                   A6=(Y'(2))
                              7
 +        SA7       A7+JY     8                   A7=(Y"(2))
                              9
                             10
          JP        [99]     11
 [4]      BSS       0                   J=8-N=7,6,5,4,3,2,1,0
          SA3       X+1       0         X3=X"(1)
          SX6       N-2       1         X6=N-2
 +        SA2       X         2 X6      X2=X'(1)
                              3
          MI        X6,[2]    4         IF N=1  (I.E. J=7)
          SA4       A2+JX     5         X4=X'(2)
                              6
                              7 X3
 +        SA5       A3+JX     8         X5=X"(2)
                              9 X2
          ZR        X6,[3]   10         IF N=2  (I.E. J=6)
                             11
          BX6       X2       12 X4      X6=X'(1)
 +        BX7       X3       13 X6      X7=X"(1)
          S_J       J-6      14 X7      J=-1,-2,...,-6
          SA6       Y        15 X6                A6=(Y'(1))
 +        SA7       Y+1      16 X5                A7=(Y"(1))
 [5]      DCPY1R    JX,JY,J
          SB0       0         9
 [6]      BX6       X4       10 X5      X6=X'(N)
          BX7       X5       11 X6      X7=X"(N)
          SA6       A6+JY    12 X7                A6=(Y'(N))
          SA7       A7+JY    13                   A7=(Y"(N))
 [99]     BSS       0
 DVCPY    ENDM
 DMCPY    MACRO     M,N,X,IX,JX,Y,IY,JY,I,J   X=X0,Y=A0,I=X1
          LOCAL     [1],[2],[3],[4],[5],[6],[99]
          LE        M,N,[2]             IF M .LE. N
 [1]      SWAP      M,N                 EXCHANGE M AND N
          SWAP      IX,JX               EXCHANGE IX AND JX
          SWAP      IY,JY               EXCHANGE IY AND JY
 [2]      BRANCH    [4],N,LE,8,J,I,M   J=8-N, I=-M
 [3]      BSS       0              FOR K = 1, 2, ..., M  AND I = K-1-M
 +        SA4       X         0         X4=X'(K,1)
          SA5       X+1       1         X5=X"(K,1)
          S_J       -N        2                             J=-N
                              3
 +        S_I       I+1       4                             I=K-M
          SA2       A4+JX     5         X2=X'(K,2)
          SA3       A5+JX     6         X3=X"(K,2)
                              7 X4
 +        BX6       X4        8 X5      X6=X'(K,1)
          BX7       X5        9 X6      X7=X"(K,1)
          SA4       A2+JX    10 X7      X4=X'(K,3)
          SA5       A3+JX    11         X5=X"(K,3)
 +        SA6       Y        12 X2      Y'(K,1)=X'(K,1)
          SA7       Y+1      13 X3      Y"(K,1)=X''(K,1)
          S_J       J+8      14                             J=8-N
          DCPY6R    JX,JY,J
 +        S_Y       Y+IY     39         Y=(Y(K+1,1))
          BX6       X2       40 X4      X6=X'(K,N+J-6)
                             41 X6
          BX7       X3       42 X5      X7=X"(K,N+J-6)
          SA6       A6+JY    43 X7      Y'(K,N+J-6)=X'(K,N+J-6)
 +        SA7       A7+JY    44         Y"(K,N+J-6)=X''(K,N+J-6)
          BX6       X4       45         X6=X'(K,N+J-5)
          BX7       X5       46 X6      X7=X"(K,N+J-5)
          S_X       X+IX     47 X7      X=(X(K+1,1))
 +        SA6       A6+JY    48 X       Y'(K,N+J-5)=X'(K,N+J-5)
          SA7       A7+JY    49         Y"(K,N+J-5)=X''(K,N+J-5)
                             50
                             51
          MI        I,[3]    52         WHILE I<0
          BRANCH    [99],J,EQ,5,N      N=5-J
          RESIDU    A4,X,IX,JX,A6,Y,IY,JY
          GT        M,N,[1]
 [4]      BSS       0
          BRANCH    [5],N,GE,2,J,I,M    J=2-N=1,0,-1,...,-6; I=-M
          SA4       X                   X4=X'(1,1)
          SA5       X+1                 X5=X"(1,1)
          BX6       X4                  X6=X'(1,1)
          BX7       X5                  X7=X"(1,1)
          SA6       Y                   Y'(1,1)=X'(1,1)
          SA7       Y+1                 Y"(1,1)=X''(1,1)
          JP        [99]
 [5]      BSS       0                   FOR K = 1,2,...,M AND I = K-1-M
          SA2       X                   X2=X'(K,1)
          SA3       X+1                 X3=X"(K,1)
          S_J       -N                                      J=-N
 +        S_I       I+1                                     I=K-M
          SA4       A2+JX               X4=X'(K,2)
          SA5       A3+JX               X5=X"(K,2)
 +        BX6       X2                  X6=X'(K,1)
          BX7       X3                  X7=X"(K,1)
          S_J       2+J                                     J=2-N
 +        SA6       Y                   Y'(K,1)=X'(K,1)
          SA7       Y+1                 Y"(K,1)=X''(K,1)
          ZR        J,[6]               IF N=2
          DCPY1R    JX,JY,J
 [6]      BX6       X4                  X6=X'(K,N)
          BX7       X5                  X7=X"(K,N)
          SA6       A6+JY               Y'(K,N)=X'(K,N)
          SA7       A7+JY               Y"(K,N)=X''(K,N)
 +        S_X       X+IX                X=(X(K+1,1))
          S_Y       Y+IY                Y=(Y(K+1,1))
          MI        I,[5]               WHILE I<0
 [99]     BSS       0
 DMCPY    ENDM
*CD       UTL
 TMUTL    MACRO     TYPE,NM1,IX,JX,J    X1=(X(1,1))
          LOCAL     [1]
          SX0       X1+JX               X0=(X(1,2))
          SX1       X1+IX               X1=(X(2,1))
 [1]      BSS       0
 TAU      IFNE      TYPE,CTYPE
          "P"VCPY   NM1,X0,JX,X1,IX,J
 TAU      ELSE
          DVCPY     NM1,X0,JX,X1,IX,J
 TAU      ENDIF
          S_J       IX+JX                                   J=IX+JX
          S_NM1     NM1-1                                   NM1=NM1-1
          SX0       X0+J                X0=(X(I+1,I+2))
          SX1       X1+J                X1=(X(I+2,I+1))
          NE        NM1,B0,[1]
 TMUTL    ENDM
*CD       DIV
 RVDIV    MACRO     N,X,JX,Y,JY,Z,JZ,IFAIL
          LOCAL     [1],[2],[3],[99]
          SA5       Y                   X5=Y(1)
          SA4       X                   X4=X(1)
          S_X       X+JX                X=(X(2))
          S_Y       Y+JY                Y=(Y(2))
          S_IFAIL   1                                       IFAIL=1
          ZR        X5,[99]             IF Y(1)=0.
          RX7       X4/X5               X7=X(1)/Y(1)
          EQ        IFAIL,N,[2]         IF N=1
 [1]      BSS       0                   FOR K = 0, 1, 2, ...
          SA5       Y                   X5=Y(K+2)
          SA4       X                   X4=X(K+2)
          S_X       X+JX                X=(X(K+3))
          S_Y       Y+JY                Y=(Y(K+3))
          S_IFAIL   IFAIL+1                            IFAIL=K+2
          ZR        X5,[3]              IF Y(K+2)=0.
          SA7       Z                   Z(K+1)=X(K+1)/Y(K+1)
          RX7       X4/X5               X7=X(K+2)/Y(K+2)
          S_Z       Z+JZ                Z=Z(K+2)
          LT        IFAIL,N,[1]
 [2]      S_IFAIL   0                                       IFAIL=0
 [3]      SA7       Z                   Z(K+2)=X(K+2)/Y(K+2)
 [99]     BSS       0
 RVDIV    ENDM
*CD       FACT
 TFACT    MACRO     TYPE,SAVEA,SAVER,N,JA,TEMP7L,A1K,AKK,I,J,K X1=(A)
          LOCAL     [1],[2],[3],[4],[99]
          LOCAL     SAVEN,IFAIL,DET,JFAIL,NXCH,FLOAT1,TEMP2
 SAVEN    EQU       TEMP7L              N
 IFAIL    EQU       SAVEN+1             IFAIL
 DET      EQU       IFAIL+1             DET
 JFAIL    EQU       DET+LENGTH          JFAIL
 NXCH     EQU       JFAIL+1             NXCH
 FLOAT1   EQU       NXCH+1              1.
 TEMP2    EQU       FLOAT1+1
*         DET    =  ONE
*         JFAIL  =  0
*         IFAIL  =  0
*         NXCH   =  0
          SX6       N                   X6=N
          MX7       0                   X7=0
          SA6       SAVEN               SAVEN=N
          SX5       1
          SA7       IFAIL               IFAIL=0
          PX6       X5
          SA7       JFAIL               JFAIL=0
          NX6       X6                  X6=1.
          SA7       NXCH                NXCH=0
          SA6       FLOAT1              FLOAT1=1.
          SA6       DET                 DET=1.
 TAU      IFEQ      LENGTH,2
          SA7       DET+1               DET''=0.
 TAU      ENDIF
          S_K       B0                  K=0
          S_AKK     X1                  AKK=(A(1,1))
          S_A1K     X1                  A1K=(A(1,1))
          SX4       N                   X4=N
 [1]      BSS       0                   A1K=(A(1,K))        K=K-1
*                                       AKK=(A(K,K))
*                                       X4=N
          SX1       AKK                 X1=(A(K,K))
          S_N       X4                  RESTORE N
          TFACT1    TYPE,[2],SAVEA,SAVER,N,K,JA,NXCH,AKK,I,J X1=(A(K,K))
          S_AKK     X1                  AKK=(A(K,K))
          "P"1DET   DET,JFAIL,FLOAT1,I,J  X1=(A(K,K)) NOT PRESERVED
          S_J       K+1                 J=K
          SA1       SAVEA               X1=(A(1,1))
          EQ        J,N,[3]             IF K=N
*                                       K=K-1
          TFACT2    SAVEN,SAVEA,N,K,A1K,AKK,JA,TEMP2,I,J
          SA4       SAVEN               X4=N
          S_J       JA+LENGTH           J=IA+JA
          S_A1K     A1K+JA              A1K=(A(1,K+1))
          S_AKK     AKK+J               AKK=(A(K+1,K+1))
          JP        [1]
*         IFAIL  =  -1
*         DET    =  ZERO
*         JFAIL  =  0
 [2]      BSS       0
          SX5       -1                  X5=IFAIL=-1
          MX6       0                   DET=ZERO
 TAU      IFEQ      LENGTH,2
          MX7       0
 TAU      ENDIF
          MX0       0                   X0=JFAIL=0
          JP        [99]
*         R(N)=NXCH
*         IF(MOD(NXCH,2) .EQ. 1)  DET = -DET
*         DE-SCALE DET
*         SET JFAIL = -1, 0, +1
*         LOAD IFAIL INTO X5
 [3]      BSS       0
          SA1       NXCH                X1=NXCH
          SA2       SAVER               X2=(R(1))
          S_J       N-1                 J=N-1
          MX0       59                  X0=777...776
          SA3       DET                 X3=DET
 TAU      IFEQ      LENGTH,2
          SA4       DET+1               X4=DET''
 TAU      ENDIF
          BX6       X1                  X6=NXCH
          BX0       -X0*X1              X0=MOD(NXCH,2)
          SA6       X2+J                R(N)=NXCH
          ZR        X0,[4]              IF MOD(NXCH,2)=1 THEN
          BX3       -X3                           DET=-DET
 TAU      IFEQ      LENGTH,2
          BX4       -X4
 TAU      ENDIF
 [4]      BSS       0                   X3,X4=DET
          "P"2DET   JFAIL,I,J           X0=JFAIL, X6,X7=DET
          SA5       IFAIL               X5=IFAIL
 [99]     BSS       0                   X0=JFAIL, X5=IFAIL, X6,X7=DET
 TFACT    ENDM
 TFACT1   MACRO     TYPE,SING,SAVEA,SAVER,N,K,JA,NXCH,IA,J,L
          LOCAL     [99]
*                                       X1=(A(K,K))
*                                       K=K-1
*         CALL VMAXA(N-K+1,A(K,K),IA,R(K),1,L,P)
*         L'  =  L + (K-1)
*         IF(P .EQ. 0.)     GOTO SING
*         IF(L' .EQ. K)     GOTO  99
*            CALL VXCH(N,A(K,1),JA,A(L,1),JA)
*            NXCH  =  NXCH + 1
*            R(NXCH)  =  K*2**12 + L'
*   99    CONTINUE
          S_IA      LENGTH                                  IA=1 OR 2
          S_K       N-K                 K=N-(K-1)=N-K+1
 TAU      IFEQ      TYPE,CTYPE
          CVMAXA    K,IA,A0,L,J         X1=(A(K,K)),  A0=L
 TAU      ELSE
          RVMAXA    K,IA,A0,L,J         X1=(A(K,K)),  A0=L
 TAU      ENDIF                         X1 IS PRESERVED
          S_K       N-K                 K=N-(N-K+1)=K-1
          SA5       SAVEA               X5=(A(1,1))
          S_L       A0-1                                    L=L-1
          ZR        X6,SING             IF P=0. GOTO SING
          ZR        L,[99]              IF L'=K GOTO 99
          SX0       X5+K                X0=(A(1,1))+K-1=(A(K,1))
 TAU      IFEQ      LENGTH,2
          SX0       X0+K
 TAU      ENDIF
          SA0       X0+L                A0=(A(K,1))+L-1=A(L',1))
 TAU      IFEQ      LENGTH,2
          SA0       A0+L
 TAU      ENDIF
 TAU      IFEQ      TYPE,RTYPE
          RVXCH     N,X0,JA,A0,JA,J
 TAU      ELSE
          DVXCH     N,X0,JA,A0,JA,J
 TAU      ENDIF
          SA4       NXCH                X4=NXCH
          SA3       SAVER               X3=(R(1))
          SX0       K+1                 X0=K
          SX7       X0+L                X7=K+L-1=L'
          LX0       12                  X0=K**12
          BX7       X7+X0               X7=(K,L)
          SX6       X4+1                X6=NXCH+1
          IX0       X3+X4               X0=(R(1))+NXCH-1=(R(NXCH))
          SA6       A4                  NXCH=NXCH+1
          SA7       X0                  R(NXCH)=(K,L')
 [99]     BSS       0
 TFACT1   ENDM
 TFACT2   MACRO     SAVEN,SAVEA,N,K,A1K,AKK,JA,TEMP2,IA,J
          LOCAL     [1]
*                                       X1=(A(1,1))
*                                       K=K-1
*         IF(K .EQ. 1)  GOTO 1
*         CALL MMNA(N-K,K-1,A(1,K+1),JA,IA,A(K,1),JA,A(K,K+1),JA)
*  1      CALL VSCL(N-K,A(K,K),A(K,K+1),JA,A(K,K+1),JA)
*         CALL MMNA(N-K,K,A(K+1,1),IA,JA,A(1,K+1),IA,A(K+1,K+1),IA)
          S_IA      LENGTH              IA=1 OR 2
          ZR        K,[1]               IF K=1
          S_J       K+1                 J=K
          SA0       AKK+JA              A0=(A(K,K+1))
          S_N       N-J                 N=N-K
          SX2       X1+K                X2=(A(1,1))+K-1=(A(K,1))
 TAU      IFEQ      LENGTH,2
          SX2       X2+K
 TAU      ENDIF
          SX1       A1K+JA              X1=(A(1,K+1))
*                                       X1=(A(1,K+1))
*                                       X2=(A(K,1))
*                                       A0=(A(K,K+1))
          "P"MMPY   "MINUS",N,K,JA,IA,JA,"PLUS",A0,JA,TEMP2,J
 [1]      BSS       0
          SA5       SAVEN               X5=N
          SA1       AKK                 X1=A(K,K)
 TAU      IFEQ      LENGTH,2
          SA2       AKK+1               X2=A''(K,K)
 TAU      ENDIF
          SA0       AKK+JA              A0=(A(K,K+1))
          S_K       K+1                 K=(K-1)+1=K
          S_J       -K                  J=-K
          S_N       X5+J                N=N-K
          "P"VSCL   N,A0,JA,A0,JA,J     X1=A(K,K) [X2=A''(K,K)]
          SA1       SAVEA               X1=(A(1,1))
          SA0       AKK+IA              A0=(A(K+1,K))
          SX2       A1K+JA              X2=(A(1,K+1))
          SA0       A0+JA               A0=(A(K+1,K+1))
          SX1       X1+K                X1=(A(1,1))+K=(A(K+1,1))
 TAU      IFEQ      LENGTH,2
          SX1       X1+K
 TAU      ENDIF
*                                       N=N-K
*                                       X1=(A(K+1,1))
*                                       X2=(A(1,K+1))
*                                       A0=(A(K+1,K+1))
          "P"MMPY   "MINUS",N,K,IA,JA,IA,"PLUS",A0,IA,TEMP2,J
 TFACT2   ENDM
*CD       DET
 R1DET    MACRO     DET,JFAIL,FLOAT1,I,J  X1=(A(K,K))
*         DET  =  AKK*DET
*         AKK  =  1./AKK
 +        SA1       X1        0         X1=AKK    A1=(AKK)
          SA5       FLOAT1    1         X5=1.
          NO                  2
 +        SA3       DET       3         X3=DET    A3=(DET)
          SA4       JFAIL     4         X4=JFAIL  A4=(JFAIL)
*                             5
*                             6
*                             7 X1
*                             8 X5
 +        RX6       X5/X1     9         X6=1./AKK
*                            10 X3
          RX3       X1*X3    11 X4      X3=AKK*DET=DET
*                            12
*                            13
*                            14
*                            15 X3
          UX3       X3,I     16         X3=MANT             I=EXP
*                            17 X3
          PX7       X3,B0    18         X7=SCALED DET
 +        SX0       -I       19 X7      X0=-EXP
          SA7       A3       20 X0      STORE SCALED DET
          IX7       X4-X0    21         X7=JFAIL+EXP=JFAIL
*                            22 X7
          SA7       A4       23         STORE NEW JFAIL
          SA6       A1       29         AKK=1./AKK
 R1DET    ENDM
 R2DET    MACRO     JFAIL,I,J   X3=DET
          LOCAL     [1],[2],[99]
*         ADD JFAIL INTO EXPONENT OF DET
*         SET JFAIL = -1, 0, OR +1
          SA5       JFAIL               X5=JFAIL
          UX3       X3,I                X3=MANT             I=EXP
          SX0       1023                X0=1023
          SX1       I                   X1=SCALED EXP
          IX1       X1+X5               X1=TRUE EXP
          IX6       X1-X0               X6=EXP-1023
          IX7       X1+X0               X7=EXP+1023
          PL        X6,[1]              IF EXP .GE. 1023 OVERFLOW
          PL        X7,[2]              IF EXP .GE. -1023 IN RANGE
          SX0       -1                  JFAIL=-1
          MX6       0                   DET=ZERO
          JP        [99]
 [1]      SX0       1                   JFAIL=+1
          MX6       0                   DET=ZERO
          JP        [99]
 [2]      BSS       0
          S_I       X1                  I=EXPONENT
          MX0       0                   JFAIL=0
          PX6       X3,I                X6=TRUE DET
 [99]     BSS       0                   X0=JFAIL, X6=DET
 R2DET    ENDM
 D1DET    MACRO     DET,JFAIL,FLOAT1,I,J  X1=(A(K,K))
          SA5       X1                  X5=AKK'   A5=(AKK')
          SA4       FLOAT1              X4=1.
          SA3       A5+1                X3=AKK''  A3=(AKK'')
          SA2       DET                 X2=DET'   A2=(DET')
          SA1       DET+1               X1=DET''  A1=(DET'')
          FX0       X4/X5               X0=1./AKK'
          FX7       X2*X3               X7=DET'*AKK''
          FX1       X1*X5               X1=DET''*AKK'
          DX6       X2*X5               X6=L(DET'*AKK')=B''
          FX2       X2*X5               X2=DET'*AKK'=B'
          FX7       X1+X7               X7=DET''*AKK'+DET'*AKK''=C'
          FX1       X6+X7               X1=B''+C'
          FX6       X1+X2               X6=B''+C'+B'=DET'
          DX7       X1+X2               X7=L(B''+C'+B')=DET''
          FX2       X0*X5               X2=(1/AKK')*AKK'=R'
          DX1       X0*X5               X1=L(1/AKK')*AKK'=R''
          UX7       X7,J                X7=MANT''           J=EXP''
          UX6       X6,I                X6=MANT'            I=EXP'
          S_J       J-I                 REDUCE EXPONENTS BY I
          PX6       X6,B0
          PX7       X7,J
          SA6       A2                  STORE SCALED DET
          SA7       A1
          FX7       X4-X2               X7=1-R'=S'
          DX4       X4-X2               X4=L(1-R')=S''
          FX2       X0*X3               X2=(1/AKK')*AKK''
          NX3       X7                  X3=S'
          FX7       X3+X4               X7=S'+S''=T'
          FX4       X7-X1               X4=T'-R''
          FX7       X4-X2               X7=T'-R''-(1/AKK')*AKK''=U'
          FX3       X7/X5               X3=U'/AKK'
          SA4       JFAIL               X4=JFAIL
          SX2       -I                  X2=-EXP'
          IX7       X4-X2               X7=JFAIL+EXP'
          SA7       A4                  JFAIL=JFAIL+EXP'
          FX7       X3+X0
          DX5       X3+X0
          NX0       X7
          DX7       X0+X5
          FX6       X0+X5
          SA7       A3
          SA6       A5
 D1DET    ENDM
 D2DET    MACRO     JFAIL,I,J    X3,X4=DET
          LOCAL     [1],[2],[99]
*         ADD JFAIL INTO EXPONENT OF DET
*         SET JFAIL = -1, 0, OR +1
          SA5       JFAIL               X5=JFAIL
          UX3       X3,I                X3=MANT             I=EXP
          UX4       X4,J                X4=MANT''           J=EXP''
          SX0       1023                X0=1023
          SX1       I                   X1=SCALED EXP
          SX2       J                   X2=SCALED EXP''
          IX1       X1+X5               X1=TRUE EXP
          IX2       X2+X5               X2=TRUE EXP''
          IX6       X1-X0               X6=EXP-1023
          IX7       X2+X0               X7=EXP''+1023
          PL        X6,[1]              IF EXP .GE. 1023 OVERFLOW
          PL        X7,[2]              IF EXP .GE. -1023 IN RANGE
          SX0       -1                  JFAIL=-1
          MX6       0                   DET=ZERO
          MX7       0
          JP        [99]
 [1]      SX0       1                   JFAIL=+1
          MX6       0                   DET=ZERO
          MX7       0
          JP        [99]
 [2]      S_I       X1                  I=EXPONENT
          S_J       X2                  J=EXPONENT''
          MX0       0                   JFAIL=0
          PX6       X3,I                X6=TRUE DET
          PX7       X4,J                X7=TRUE DET''
 [99]     BSS       0                   X0=JFAIL, X6,X7=DET
 D2DET    ENDM
 C1DET    MACRO     DET,JFAIL,FLOAT1,I,J  X1=(A(K,K))
          LOCAL     [1],[2],[3]
          SA2       X1+1      0         X2=AKK''  A2=(AKK'')
          SA3       DET       1         X3=DET'   A3=(DET')
          SA1       X1        2         X1=AKK'   A1=(AKK')
          SA4       DET+1     3         X4=DET''  A4=(DET'')
          NO                  4
*                             5
*                             6
*                             7 X2
 +        RX6       X2*X2     8 X3      X6=AKK''**2
          SA5       FLOAT1    9 X1      X5=1.
          RX0       X1*X1    10 X4      X0=AKK'**2
*                            11
*                            12 X6
 +        RX7       X1*X4    13         X7=AKK'*DET''
*                            14 X0
          RX0       X0+X6    15         X0=(AKK'**2+AKK''**2)
          RX4       X2*X4    16 X5      X4=AKK''*DET''
*                            17 X7
          RX6       X1*X3    18 X0      X6=AKK'*DET'
 +        NX0       X0       19
          RX3       X2*X3    20 X4      X3=AKK''*DET'
*                            21 X0
          RX0       X5/X0    22 X6      X0=1./(...)
          RX6       X6-X4    23         X6=AKK'*DET'-AKK''*DET''=DET'
 +        SA5       JFAIL    24 X3      X5=JFAIL    A5=(JFAIL)
          RX7       X7+X3    25         X7=AKK'*DET''+AKK''*DET'=DET''
*                            26 X6
          NX6       X6       27
*                            28 X7
*                            29 X6
 +        NX7       X7       30
*                            31 X5
          UX3       X6,I     32 X7      X3=MANT'            I=EXP'
          UX4       X7,J     33 X3      X4=MANT''           J=EXP''
          SX7       -I       34 X4      X7=-EXP'
 +        NO                 35 X7
*                            36
          GT        I,J,[1]  37
          SX7       -J       38         X7=-MAX(EXP',EXP'')
*                            39 X7
*                            40
 [1]      IX6       X5-X7    41 X0      X6=JFAIL+MAX(EXP',EXP'')
          RX2       X2*X0    42 X6      X2=AKK''/(...)
          SA6       A5       43         JFAIL=JFAIL+MAX(EXP',EXP'')
          RX6       X1*X0    44         X6=AKK'/(...)
 +        S_I       X7+I     45         I=EXP'-MAX(EXP',EXP'').LE.0
          S_J       X7+J     46 X2      J=EXP''-MAX(EXP',EXP'').LE.0
          NO                 47
          BX7       -X2      48 X6      X7=-AKK''/(...)=(1./AKK)''
 +        SA6       A1       49 X7      AKK'=(1./AKK)'
          SA7       A2       50         AKK''=(1./AKK)''
          MX6       0        51         IF I<-1023 THEN DET'=0.
          MX7       0        52 X6      IF J<-1023 THEN DET''=0.
 +        SX0       I+1023   53 X7
          MI        X0,[2]
          PX6       X3,I                X6=DET'
 [2]      SX0       J+1023
          MI        X0,[3]
          PX7       X4,J                X7=DET''
 [3]      SA6       A3                  STORE SCALED DET'
          SA7       A4                  STORE SCALED DET''
 C1DET    ENDM
 C2DET    MACRO     JFAIL,I,J    X3,X4=DET
          LOCAL     [1],[2],[3],[99]
*         ADD JFAIL INTO EXPONENT OF DET
*         SET JFAIL = -1, 0, OR +1
          SA5       JFAIL               X5=JFAIL
          UX3       X3,I                X3=MANT             I=EXP
          UX4       X4,J                X4=MANT''           J=EXP''
          SX0       1023                X0=1023
          SX1       I                   X1=SCALED EXP
          SX2       J                   X2=SCALED EXP''
          IX1       X1+X5               X1=TRUE EXP
          IX2       X2+X5               X2=TRUE EXP''
          IX6       X1-X0               X6=EXP-1023
          IX7       X2-X0               X7=EXP''-1023
          PL        X6,[1]              IF EXP .GE. 1023 OVERFLOW
          PL        X7,[1]              IF EXP'' .GE. 1023 OVERFLOW
          IX6       X1+X0               X6=EXP+1023
          IX7       X2+X0               X7=EXP''+1023
          PL        X6,[2]              IF EXP .GE. -1023 IN RANGE
          BX1       -X0                           ELSE EXP=-1023
          PL        X7,[3]              IF EXP'' .GE. -1023 IN RANGE
          SX0       -1                  JFAIL=-1
          MX6       0                   DET=ZERO
          MX7       0
          JP        [99]
 [1]      SX0       1                   JFAIL=+1
          MX6       0                   DET=ZERO
          MX7       0
          JP        [99]
 [2]      PL        X7,[3]
          BX2       -X0                           EXP''=-1023
 [3]      S_I       X1                  I=EXPONENT
          S_J       X2                  J=EXPONENT''
          MX0       0                   JFAIL=0
          PX6       X3,I                X6=TRUE DET
          PX7       X4,J                X7=TRUE DET''
 [99]     BSS       0                   X0=JFAIL, X6,X7=DET
 C2DET    ENDM
*CD       FEQN
 TFEQN    MACRO     TYPE,SAVEA,N,JA,K,B,TEMP1,I,J,L  X1=(A), X3=(R)
          LOCAL     [1]
          S_J       N-1                 J=N-1
          S_I       1                   I=1=JR
          SA5       X3+J                X5=R(N)
          SX7       LENGTH              X7=IA
          SA0       X3                  A0=(R(1))
          ZR        X5,[1]              IF NO TRANSPOSITIONS
          S_L       X5                  L=R(N)= NO. OF TRANSPOS.
          SUBST     TYPE,K,B,JA,L,A0,I,TEMP1,J    X7=IA=LENGTH
          SA1       SAVEA               X1=(A)
 [1]      BSS       0                   X1=(A)
          S_L       LENGTH              L=IA
          SA0       X1                  A0=(A)
          "P"FEQN   N,A0,L,JA,K,B,I,J
 TFEQN    ENDM
 RFEQN    MACRO     N,A,IA,JA,K,B,I,J
          LOCAL     [1],[2],[3],[4],[99]
          SA4       A                   X4=A(1,1) A4=(A(1,1))
          SA5       B                   X5=B(1,1) A5=(B(1,1))
 [1]      BSS       0                   X4=A(1,1) A4=(A(1,1))
*                                       X5=B(1,L) A5=(B(1,L))
          RX6       X4*X5               X6=A(1,1)*B(1,L)
          S_I       1                                       I=1
          SA6       A5                            A6=(B(1,L))
          EQ        I,N,[4]             IF N=1
 [2]      S_A       A+IA                          A=(A(I+1,1))
          SX5       A6+IA               X5=(B(I+1,1))
          RVMPY     "MINUS",I,A,JA,B,IA,"PLUS",X5,J
*                                                 A4=(A(I+1,I))
*                                                 A5=(B(I,L))
          SA4       A4+JA               X4=A(I+1,I+1) A4=(A(I+1,I+1))
          S_I       I+1                                     I=I+1
          RX6       X6*X4               X6=B(I+1,L)
          SA6       A5+IA                         A6=(B(I+1,L))
          LT        I,N,[2]             IF I<N
          S_IA      -IA                                     IA=-1
          S_JA      -JA                                     JA=-IDIM
          S_B       A6                                      B=(B(N,L))
          S_I       1                                       I=1
          S_A       A4                                      A=(A(N,N))
 [3]      S_A       A+IA                                    A=(A(N-I,N))
          SX5       A6+IA               X5=(B(N-1,L))
          RVMPY     "MINUS",I,A,JA,B,IA,"PLUS",X5,J
          S_I       I+1                                     I=I+1
          SA6       A5+IA                         A6=(B(N-I,L))
          LT        I,N,[3]             IF I<N
*                                                 A4=(A(1,2))
          S_A       A4+JA                                   A=(A(1,1))
          S_IA      -IA                                     IA=1
          S_JA      -JA                                     JA=IDIM
          S_B       A6                                      B=(B(1,L))
 [4]      S_K       K-1                                     K=K-1
          S_B       B+JA                                    B=(B(1,L+1))
          ZR        K,[99]              IF L<K
          SA4       A                   X4=A(1,1) A4=(A(1,1))
          SA5       B                   X5=B(1,L+1) A5=(B(1,L+1))
          JP        [1]
 [99]     BSS       0
 RFEQN    ENDM
 DFEQN    MACRO     N,A,IA,JA,K,B,I,J
          LOCAL     [1],[2],[3],[4],[99]
          SA2       A                   X2=A'(1,1) A2=(A(1,1))
          SA4       B                   X4=B'(1,1) A4=(B(1,1))
          SA3       A2+1                X3=A''(1,1)A3=(A''(1,1))
          SA5       A4+1                X5=B''(1,1)A5=(B''(1,1))
 [1]      BSS       0                             A2=(A(1,1))
*                                                 A4=(B(1,L))
          FX0       X3*X4               X0=A''*B'
          DX1       X2*X4               X1=L(A'*B')
          FX7       X2*X5               X7=A'*B''
          FX1       X1+X0               X1=L(A'*B')+A''*B'
          FX6       X2*X4               X6=A'*B'
          FX1       X1+X7               X1=L(A'*B')+A''*B'+A'*B''
          DX7       X1+X6
          FX6       X1+X6
          S_I       1                                       I=1
          SA7       A4+1
          SA6       A4                            A6=(B(1,L))
          EQ        I,N,[4]             IF N=1
 [2]      S_A       A+IA                          A=(A(I+1,1))
          SX5       A6+IA               X5=(B(I+1,1))
          DVMPY     "MINUS",I,A,JA,B,IA,"PLUS",X5,J
*                                                 A2=(A(I+1,I))
*                                                 A4=(B(I,L))
          SA2       A2+JA               X2=A(I+1,I+1)
          SA3       A3+JA               X3=A''(I+1,I+1)
          S_I       I+1                                     I=I+1
          FX0       X3*X6
          DX1       X2*X6
          FX7       X2*X7
          FX1       X1+X0
          FX6       X2*X6
          FX1       X1+X7
          DX7       X1+X6
          FX6       X1+X6
          SA7       A5+IA
          SA6       A4+IA                         A6=(B(I+1,L))
          LT        I,N,[2]             IF I<N
          S_IA      -IA                                     IA=-1
          S_JA      -JA                                     JA=-IDIM
          S_B       A6                                      B=(B(N,L))
          S_I       1                                       I=1
          S_A       A2                                      A=(A(N,N))
 [3]      S_A       A+IA                                    A=(A(N-I,N))
          SX5       A6+IA               X5=(B(N-1,L))
          DVMPY     "MINUS",I,A,JA,B,IA,"PLUS",X5,J
          S_I       I+1                                     I=I+1
          SA6       A4+IA                         A6=(B(N-I,L))
          SA7       A5+IA
          LT        I,N,[3]             IF I<N
*                                                 A2=(A(1,2))
          S_A       A2+JA                                   A=(A(1,1))
          S_IA      -IA                                     IA=1
          S_JA      -JA                                     JA=IDIM
          S_B       A6                                      B=(B(1,L))
 [4]      S_K       K-1                                     K=K-1
          S_B       B+JA                                    B=(B(1,L+1))
          ZR        K,[99]              IF L<K
          SA2       A                   X2=A'(1,1)
          SA4       B                   X4=B'(1,L+1)
          SA3       A2+1                X3=A''(1,1)
          SA5       A4+1                X5=B''(1,L+1)
          JP        [1]
 [99]     BSS       0
 DFEQN    ENDM
CFEQN    MACRO     N,A,IA,JA,K,B,I,J
          LOCAL     [1],[2],[3],[4],[99]
          SA3       B                   X3=B'(1,1) A3=(B(1,1))
          SA1       A                   X1=A'(1,1) A1=(A(1,1))
          SA4       A3+1                X4=B''(1,1)A4=(B''(1,1))
          SA2       A1+1                X2=A''(1,1)A2=(A''(1,1))
          SA5       1                   X5=VOID    A5=1
 [1]      BSS       0                             A1=(A(1,1))
*                                                 A3=(B(1,L))
          RX6       X1*X3               X6=A'*B'
          RX7       X1*X4               X7=A'*B''
          RX4       X2*X4               X4=A''*B''
          RX2       X2*X3               XX2=A''*B'
          RX6       X6-X4               X6=A'*B'-A''*B''
          RX7       X7+X2               X7=A'*B''+A''*B'
          S_I       1                                       I=1
          NX6       X6
          NX7       X7
          SA6       A3                            A6=(B(1,L))
          SA7       A3+1
          EQ        I,N,[4]             IF N=1
 [2]      S_A       A+IA                          A=(A(I+1,1))
          SX5       A6+IA               X5=(B(I+1,1))
          CVMPY     "MINUS",I,A,JA,B,IA,"PLUS",X5,J
*                                                 A1=(A(I+1,I))
*                                                 A3=(B(I,L))
          SA1       A1+JA               X1=A(I+1,I+1)
          SA2       A2+JA               X2=A''(I+1,I+1)
          S_I       I+1                                     I=I+1
          RX3       X6*X1               X3=S'*A'
          RX0       X7*X2               X0=S''*A''
          RX4       X7*X1               X4=S''*A'
          RX5       X6*X2               X5=S'*A''
          RX6       X3-X0               X6=S'*A'-S''*A''
          RX7       X4+X5               X7=S''*A'+S'*A''
          NX6       X6
          NX7       X7
          SA6       A3+IA                         A6=(B(I+1,L))
          SA7       A4+IA
          LT        I,N,[2]             IF I<N
          S_IA      -IA                                     IA=-1
          S_JA      -JA                                     JA=-IDIM
          S_B       A6                                      B=(B(N,L))
          S_I       1                                       I=1
          S_A       A1                                      A=(A(N,N))
 [3]      S_A       A+IA                                    A=(A(N-I,N))
          SX5       A6+IA               X5=(B(N-1,L))
          CVMPY     "MINUS",I,A,JA,B,IA,"PLUS",X5,J
          S_I       I+1                                     I=I+1
          SA6       A3+IA                         A6=(B(N-I,L))
          SA7       A4+IA
          LT        I,N,[3]             IF I<N
*                                                 A1=(A(1,2))
          S_A       A1+JA                                   A=(A(1,1))
          S_IA      -IA                                     IA=1
          S_JA      -JA                                     JA=IDIM
          S_B       A6                                      B=(B(1,L))
 [4]      S_K       K-1                                     K=K-1
          S_B       B+JA                                    B=(B(1,L+1))
          ZR        K,[99]              IF L<K
          SA3       B                   X3=B(1,L+1)
          SA1       A                   X1=A(1,1)
          SA4       A3+1                X4=B''(1,L+1)
          SA2       A1+1                X2=A''(1,1)
          JP        [1]
 [99]     BSS       0
CFEQN    ENDM
*CD       SUBST
 SUBST    MACRO     TYPE,N,X,JX,L,R,JR,TEMP1,J    X7=IX
          LOCAL     [1],FLOATIX
 FLOATIX  EQU       TEMP1
          PX7       X7,B0               X7=FLOAT(IX)
          SA7       FLOATIX             FLOATIX=FLOAT(IX)
          BX5       X7                  X5=FLOAT(IX)
 [1]      SA3       R                   X3=(...J,K)
          MX6       48                  X6=11...10000
          BX1       -X6*X3              X1=K
          LX3       48                  X3=(K...J)
          SX1       X1-1                X1=K-1
          BX0       -X6*X3              X0=J
          PX1       X1,B0               X1=FLOAT(K-1)
          SX0       X0-1                X0=J-1
          DX1       X1*X5               X1=(K-1)*IX
          PX0       X0,B0               X0=FLOAT(J-1)
          DX0       X0*X5               X0=(J-1)*IX
          SX1       X1+X                X1=(X(K,1))
          SX0       X0+X                X0=(X(J,1))
 TAU      IFEQ      TYPE,CTYPE
          DVXCH     N,X0,JX,X1,JX,J
 TAU      ELSE
          "P"VXCH   N,X0,JX,X1,JX,J
 TAU      ENDIF
          SA5       FLOATIX             X5=FLOAT(IX)
          S_L       L-1                                     L=L-1
          S_R       R+JR
          NZ        L,[1]               IF L>0
 SUBST    ENDM
*CD       FINV
 TFINV    MACRO     TYPE,SAVEA,SAVER,N,JA,TEMP4,IA,I,J,K,L X1=(A) X3=(R)
          LOCAL     [99]
          S_IA      LENGTH              IA=LENGTH
          TFINV1    SAVEA,N,IA,JA,TEMP4,I,J,K,L   X1=(A)
          TFINV2    N,IA,JA,TEMP4,I,J,K,L         X1=(A)
          TFINV3    N,IA,JA,TEMP4,I,J,K,L         X1=(A)
          SA3       SAVER               X3=(R(1))
          SA1       SAVEA               X1=(A(1,1))
          SX5       X3+N                X5=(R(N))+1
          SA5       X5-1                X5=R(N)=L
          S_I       X1                  I=(A)
          SX7       JA                  X7=JA
          S_JA      -1                  JA=-1=JR
          ZR        X5,[99]             IF NO TRANSPOSITIONS
          S_K       X5-1                K=L-1
          S_L       X5                  L=L
          S_K       X3+K                K=(R(L))
          SUBST     TYPE,N,I,IA,L,K,JA,TEMP4,J    X7=JA
 [99]     BSS       0
 TFINV    ENDM
 TFINV1   MACRO     SAVEA,N,IA,JA,TEMP3,AI1,AII,J,K   X1=(A)
          LOCAL     [1],SAVEK,TEMP2
 SAVEK    EQU       TEMP3
 TEMP2    EQU       TEMP3+1
*         DO 10     K  =  1, N-1
*            I  =  K + 1
*            CALL UMPY(K,A11,A21,A22,A(I,1),A(I,2),A(I,1),A(I,2))
*            CALL VSCL(K,-A(I,I),A(I,1),A(I,2),A(I,1),A(I,2))
* 10         CONTINUE
          S_AI1     X1+IA               AI1=(A21)=(A(I,1))
          S_K       1                   K=1
          S_AII     AI1+JA              AII=(A22)=(A(I,I))
 [1]      BSS       0
          SX7       K                   X7=K
          SX2       AI1                 X2=(A(I,1))
          SA7       SAVEK               SAVEK=K
          SA0       AI1                 A0=(A(I,1))
          "P"UMPY   "PLUS",K,JA,IA,JA,"NULL",A0,JA,TEMP2,J
          SA1       AII                 X1=A(I,I)
 TAU      IFEQ      LENGTH,2
          SA2       AII+1
 TAU      ENDIF
          SA3       SAVEK               X3=K
          BX1       -X1
 TAU      IFEQ      LENGTH,2
          BX2       -X2
 TAU      ENDIF
          S_K       X3
          "P"VSCL   K,AI1,JA,AI1,JA,J   X1=-A(I,I), [X2=-A''(I,I)]
          S_AII     AII+IA              AII=(A(I+1,I))
          S_AI1     AI1+IA              AI1=(A(I+1,1))
          S_AII     AII+JA              AII=(A(I+1,I+1))
          S_K       K+1                 K=K+1
          SA1       SAVEA               X1=(A11)
          LT        K,N,[1]             IF K<N
 TFINV1   ENDM
 TFINV2   MACRO     N,IA,JA,TEMP4,A1I,AII,J,K    X1=(A)
          LOCAL     [1],[99],SAVEA12,SAVEI,TEMP2
 SAVEA12  EQU       TEMP4
 SAVEI    EQU       TEMP4+1
 TEMP2    EQU       TEMP4+2
*         A12  =  -A12
*         IF(N .EQ. 2)  GOTO 30
*         DO 20     I  =  3,N
*            K  =  I - 2
*            CALL UMNS(K,A12,A13,A23,A(2,I),A(3,I),A(1,I),A(2,I))
*            A(I-1,I)  =  -A(I-1,I)
* 20         CONTINUE
* 30      CONTINUE
          SX0       N-2                 X0=N-2
          SA4       X1+JA               X4=A12    A4=(A(1,2))
 TAU      IFEQ      LENGTH,2
          SA5       A4+1
 TAU      ENDIF
          BX6       -X4                 X6=-A12
 TAU      IFEQ      LENGTH,2
          BX7       -X5
 TAU      ENDIF
          SA6       A4                  A12=-A12
 TAU      IFEQ      LENGTH,2
          SA7       A5
 TAU      ENDIF
          ZR        X0,[99]             IF (N=2) X1=(A)
          SX1       X1+JA               X1=(A12)
          S_J       3                   J=I
          S_A1I     X1+JA               A1I=(A13)
          BX7       X1                  X7=(A12)
          S_AII     A1I+IA              AII=(A23)
          SA7       SAVEA12             SAVEA12=(A12)
          S_AII     AII+IA              AII=(A33)
 [1]      BSS       0                   X1=(A12)  J=I
          SX6       J                   X6=I
          SX2       A1I+IA              X2=(A(2,I))
          SA6       SAVEI               SAVEI=I
          S_K       J-2                 K=I-2
          SA0       A1I                 A0=(A(1,I))
          "P"UMPY   "MINUS",K,IA,JA,IA,"MINUS",A0,IA,TEMP2,J
          SA4       AII-IA              X4=A(I-1,I)
          SA3       SAVEI               X3=I
 TAU      IFEQ      LENGTH,2
          SA5       A4+1
 TAU      ENDIF
          S_AII     AII+JA              AII=(A(I,I+1))
          S_A1I     A1I+JA              A1I=(A(1,I+1))
          S_AII     AII+IA              AII=(A(I+1,I+1))
          BX6       -X4                 X6=-A(I-1,I)
 TAU      IFEQ      LENGTH,2
          BX7       -X5
 TAU      ENDIF
          SA6       A4                  A(I-1,I)=-A(I-1,I)
 TAU      IFEQ      LENGTH,2
          SA7       A5
 TAU      ENDIF
          S_J       X3+1                J=I+1
          SA1       SAVEA12             X1=(A(1,2))
          LE        J,N,[1]             IF I .LE. N
          S_J       -JA                 J=-JA
          SX1       X1+J                X1=(A(1,1))
 [99]     BSS       0                   X1=(A)
 TFINV2   ENDM
 TFINV3   MACRO     N,IA,JA,TEMP4,AI1,AII,I,J    X1=(A)
          LOCAL     [1],SAVEN,SAVEI,TEMP2
 SAVEN    EQU       TEMP4
 SAVEI    EQU       TEMP4+1
 TEMP2    EQU       TEMP4+2
*         DO 40     I  =  1, N-1
*            CALL MMPA(I,N-I,A(I+1,1),A(I+2,1),A(I+1,2),
*                                A(I,I+1),A(I,I+2),A(I,1),A(I,2))
*            CALL UMPY(N-I,A(I+1,I+1),A(I+2,I+1),A(I+2,I+2),
*                      A(I,I+1),A(I,I+2),A(I,I+1),A(I,I+2))
*  40        CONTINUE
          SX7       N                   X7=N
          SX3       1                   X3=1=I
          SA7       SAVEN               SAVEN=N
          S_N       N-1                 N=N-1=N-I
          S_AI1     X1                  AI1=(A11)
          S_AII     X1                  AII=(A11)
 [1]      BSS       0
          SX7       X3+1                X7=I+1
          S_I       X3                  I=I
          SA7       SAVEI               SAVEI=I+1
          SX1       AI1+IA              X1=(A(I+1,1))
          SX2       AII+JA              X2=(A(I,I+1))
          SA0       AI1                 A0=(A(I,1))
          "P"MMPY   "PLUS",I,N,JA,IA,JA,"PLUS",A0,JA,TEMP2,J
          S_I       N                   I=N-I
          SX2       AII+JA              X2=(A(I,I+1))
          SA0       AII+JA              A0=(A(I,I+1))
          SX1       X2+IA               X1=(A(I+1,I+1))
          S_AI1     AI1+IA              AI1=(A(I+1,1))
          S_AII     X1                  AII=(A(I+1,I+1))
          "P"UMPY   "PLUS",I,JA,IA,JA,"NULL",A0,JA,TEMP2,J
          S_N       N-1                 N=N-I-1
          SA3       SAVEI               X3=I+1
          NZ        N,[1]               IF NEXT N-I>0
          SA5       SAVEN               X5=N
          S_N       X5                  RESTORE N
 TFINV3   ENDM
*CD       TMPRNT
 TMPRNT   MACRO     HVALUE,N,IDIM,K
          LOCAL     ARGLIST,NVALUE,IVALUE,KVALUE
          EXT       TMPRNT
          SX6       N                   X6=N
          SX7       IDIM                X7=IDIM
          SA6       NVALUE              NVALUE=N
          SX6       K                   X6=K
          SA7       IVALUE              IVALUE=IDIM
          SA6       KVALUE              KVALUE=K
          SA1       ARGLIST
 +        RJ        TMPRNT              CALL TMPRNT(H,N,IDIM,K)
 -        VFD       12/0,18/TWORD
          JP        EPILOG
 ARGLIST  BSS       0                   TMPRNT ARGUMENT LIST
          VFD       30/4,30/HVALUE
          VFD       60/NVALUE
          VFD       60/IVALUE
          VFD       60/KVALUE
          BSSZ      1
 NVALUE   BSS       1
 IVALUE   BSS       1
 KVALUE   BSS       1
 TMPRNT   ENDM
*CD       LSQ
 RLLSQ    MACRO     JX,JY,J             J=2-N
          LOCAL     [1]
 [1]      BSS       0                   FOR K = 0, 1, 2,...
 +        NX6       X6        0 X5      X6=SX(1+K)
          RX2       X4*X4     1         X2=X(2+K)**2
          NX7       X7        2 X6      X7=SY(1+K)
          RX3       X4*X5     3         X3=X(2+K)*Y(2+K)
 +        NX0       X0        4 X7      X0=SXX(1+K)
          RX6       X6+X4     5 X2      X6=SX(1+K)+X(2+K)
          SA4       A4+JX     6 X0      X4=X(3+K)
          NX1       X1        7 X3      X1=SXY(1+K)
 +        RX7       X7+X5     8 X6      X7=SY(1+K)+Y(2+K)
          SA5       A5+JY     9 X1      X5=Y(3+K)
          S_J       J+1      10                             J=2-N+K+1
 +        RX0       X0+X2    11 X7      X0=SXX(1+K)+X(2+K)**2
          RX1       X1+X3    12         X1=SXY(1+K)+X(2+K)*Y(2+K)
*                            13 X4
*                            14 X0
          MI        J,[1]    15 X1      WHILE 3-N+K<0; I.E. 4+K .LE. N
 RLLSQ    ENDM                          X6=SX(N-1) ETC      J=0
 RVLLSQ   MACRO     FAIL,N,X,JX,Y,JY,J       N>1
          LOCAL     [1]
          SA4       X                   X4=X(1)   A4=(X(1))
          SA5       Y                   X5=Y(1)   A5=(Y(1))
          S_J       2
          S_J       J-N                                     J=2-N
          RX0       X4*X4               X0=X(1)**2=SXX(1)
          BX6       X4                  X6=X(1)=SX(1)
          RX1       X4*X5               X1=X(1)*Y(1)=SXY(1)
          SA4       A4+JX               X4=X(2)   A4=(X(2))
          BX7       X5                  X7=Y(1)=SY(1)
          SA5       A5+JY               X5=Y(2)   A5=(Y(2))
          PL        J,[1]               IF N=2
          RLLSQ     JX,JY,J
 [1]      NX6       X6                  X6=SX(N-1)
          RX2       X4*X4               X2=X(N)**2
          NX7       X7                  X7=SY(N-1)
          RX3       X4*X5               X3=X(N)*Y(N)
          NX0       X0                  X0=SXX(N-1)
          RX6       X6+X4               X6=SX(N-1)+X(N)
          NX1       X1                  X1=SXY(N-1)
          RX7       X7+X5               X7=SY(N-1)+Y(N)
          RX0       X0+X2               X0=SXX(N-1)+X(N)**2
          RX1       X1+X3               X1=SXY(N-1)+X(N)*Y(N)
          NX6       X6                  X6=SX(N)
          NX7       X7                  X7=SY(N)
          NX0       X0                  X0=SXX(N)
          NX1       X1                  X1=SXY(N)
          SX4       N                   X4=N
          SX5       -1                  X5=-1
          PX4       X4
          PX5       X5
          NX4       X4                  X4=FLOAT(N)
          RX2       X6*X6               X2=SX**2
          NX5       X5                  X5=-1.
          RX3       X4*X0               X3=FLOAT(N)*SXX
          RX4       X4*X1               X4=FLOAT(N)*SXY
          RX1       X1*X6               X1=SXY*SX
          RX2       X2-X3               X2=-N*SXX+SX**2=-DET
          RX3       X7*X6               X3=SY*SX
          RX6       X7*X0               X6=SY*SXX
          NX2       X2
          PL        X2,FAIL             IF DET .LE. 0.
          RX5       X5/X2               X5=-1./(-DET)
          RX3       X4-X3               X3=N*SXY-SY*SX=DET2
          RX6       X6-X1               X6=SY*SXX-SXY*SX=DET1
          NX7       X3                  X7=DET2
          NX6       X6                  X6=DET1
          RX6       X6*X5               X6=DET1/DET=C1
          RX7       X7*X5               X7=DET2/DET=C2
 RVLLSQ   ENDM                          X6=C1, X7=C2
 RLSQXY   MACRO     J,ONE               X1=X      J=2-M
          LOCAL     [1]
 [1]      BSS       0                   FOR K = 0, 1, 2,...
 +        NX6       X6        0 X4      X6=S(1+K)
          RX2       X2*X1     1         X2=X**(1+K)
          NX7       X7        2 X6      X7=T(1+K)
          RX3       X3*X1     3 X5      X3=Y*X**(1+K)
 +        SA6       A4-ONE    4 X7      B(1+K,1)=S(1+K)
          S_J       J+1       5 X2                          J=2-M+K+1
          RX6       X4+X2     6         X6=S(2+K)+X**(1+K)
 +        SA4       A4+ONE    7 X3      X4=S(3+K) A4=(B(3+K,1))
          SA7       A5-ONE    8         C(1+K)=T(1+K)
          RX7       X5+X3     9 X6      X7=T(2+K)+Y*X**(1+K)
          SA5       A5+ONE   10         X5=T(3+K) A5=(C(3+K))
*                            11
*                            12 X7
 +        MI        J,[1]    13         WHILE 3-M+K<0; I.E. 4+K .LE. M
 RLSQXY   ENDM                                              J=0
 RLSQX    MACRO     J,ONE
          LOCAL     [1]
 [1]      BSS       0                   FOR K = 0, 1, 2,...
 +        SA6       A6+IDIMPAR  X4      B(M,2+K)=B(M,2+K)+X**(M+K)
          RX6       X4+X2     1         X6=B(M,3+K)+X**(M+1+K)
          S_J       J+ONE     2                             J=J+1
          SA4       A4+IDIMPAR          X4=B(M,4+K)
          RX2       X2*X1     4 X6      X2=X**(M+2+K)
          NX6       X6        5
*                             6
*                             7 X6
          MI        J,[1]     8 X2      WHILE J<0
*                             9
 RLSQX    ENDM
 RLSQM    MACRO     M,B,IB,JB,I,J,K       B=(B(1,1)), I=(B(M,M))
          LOCAL     [1],[2]
          SX0       B+IB                X0=(B(2,1))
          S_I       I-JB                          I=(B(M,M-1))
          SA3       X0                  X3=B(2,1)
          S_K       JB-IB               K=STEP LENGTH GOING NORTH-EAST
          S_M       M-1                                     M=M-1
          S_J       1                   J=COUNT OF PASSAGES
 [1]      BSS       0                   X0=(B(J+1,1)) I=(B(M,M-J))
          SX5       X0+K                X5=(B(J,2))=LOC OF FIRST COPY
          BX6       X3                  X6=B(J+1,1)
          SA4       I                   X4=B(M,M-J)
          RVSET2    J,X5,K,B            NORTH-WEST PASSAGE (J STEPS)
          EQ        J,M,[2]             IF J=M-1 THEN B IS FILLED
          SX5       I+K                 X5=(B(M-1,M-J+1)) LOC FIRST CPY
          BX6       X4                  X6=B(M,M-J)
          RVSET2    J,X5,K,B            SOUTH-EAST PASSAGE (J STEPS)
          SX0       X0+IB               X0=(B(J+2,1))
          S_I       I-JB                          I=(B(M,M-J-1))
          SA3       X0                  X3=B(J+2,1)
          S_J       J+1
          JP        [1]                 MAKE NEXT PAIR OF PASSAGES
 [2]      S_M       M+1                 RESTORE REGISTER M TO VALUE M
 RLSQM    ENDM
 RLSQP    MACRO     N,M,B,C,I,J,ONE     X1=(X), X3=(Y)
          LOCAL     [1],[2],[3],[4],[5],[6],..
          MX6       0                   X6=0.
          S_J       ONE-M                                   J=1-M
          MX7       0                   X7=0.
          SA6       B                   B(1,1)=0.
          SA7       C                   C(1)=0.
 [1]      S_J       J+1                 J=2-M,3-M,...,0
          SA6       A6+ONE              B(I,1)=0 FOR I=2,3,..,M
          SA7       A7+ONE              C(I)=0
          MI        J,[1]               WHILE J<0 OR I<M
          S_J       ONE-M                                   J=1-M
 [2]      S_J       J+ONE               J=2-M,3-M,...,0
          SA6       A6+IDIMPAR          B(M,I)=0 FOR I=2,3,...,M
          MI        J,[2]               WHILE J<0 OR I<M
*                                       X1=(X), X2=(Y)
          SA1       X1                  X1=X(1)   A1=(X(1))
          SA3       X3                  X3=Y(1)   A3=(Y(1))
 [3]      BSS       0                   X1=X      A1=(X(I))
*                                       X3=Y      A3=(Y(I))
          SA4       B                   X4=S(1)   A4=(B(1,1))
          SA5       C                   X5=T(1)   A5=(C(1))
          SX2       ONE                 X2=1
          S_J       ONE-M
          PX2       X2
          S_J       J+ONE                                   J=2-M
          NX2       X2                  X2=1.
          RX6       X4+X2               X6=S(1)+1.
          SA4       A4+ONE              X4=S(2)   A4=(B(2,1))
          RX7       X5+X3               X7=T(1)+Y
          SA5       A5+ONE              X5=T(2)   A5=(C(2))
          RLSQXY    J,ONE
          NX6       X6                  X6=S(M-1)
          RX2       X2*X1               X2=X**(M-1)
          NX7       X7                  X7=T(M-1)
          RX3       X3*X1               X3=Y*X**(M-1)
          SA6       A6+ONE              B(M-1,1)=S(M-1) A6=(B(M-1,1))
          RX6       X4+X2               X6=S(M)+X**(M-1)
          S_J       3                                       J=3
          SA4       A4+IDIMPAR          X4=B(M,2)
          SA7       A7+ONE              C(M-1)=T(M-1)   A7=(C(M-1))
          RX7       X5+X3               X7=T(M)+Y*X**(M-1)
          NX6       X6                  X6=S(M)
          RX2       X2*X1               X2=X**M
          NX7       X7                  X7=T(M)
          SA6       A6+ONE              B(M,1)=S(M)     A6=(B(M,1))
          S_J       J-M                                     J=3-M
          RX6       X4+X2               X6=B(M,2)+X**M
          SA4       A4+IDIMPAR          X4=B(M,3)
          SA7       A7+ONE              C(M)=T(M)       A7=(C(M))
          RX2       X2*X1               X2=X**(M+1)
          NX6       X6
          PL        J,[4]               IF M=3
          RLSQX     J,ONE
 [4]      SA6       A6+IDIMPAR          B(M,M-1)=B(M,M-1)+X**(2M-3)
          RX6       X4+X2               X6=B(M,M)+X**(2M-2)
          SA1       A1+ONE              X1=X(I+1) A1=(X(I+1))
          SA3       A3+ONE              X3=Y(I+1) A3=(Y(I+1))
          S_N       N-1                 DECREMENT N
          NX6       X6
          SA6       A6+IDIMPAR          B(M,M)=B(M,M)+X**(2M-2)
          NZ        N,[3]               WHILE N>0
          S_N       IDIMPAR                                 N=IDIM
          S_I       A6                            I=(B(M,M))
          RLSQM     M,B,ONE,N,I,J,C
 RLSQP    ENDM
*CD       MAXA
 RMAXA2R  MACRO     N,JX,L,K,J
          LOCAL     [1]
 [1]      BSS       0                   FOR K = 0, 2, 4, ...
*                                       (COMMENTS ASSUME THAT N IS EVEN)
 +        AX2       X4,K      0         X2=SIGN BITS OF X(K+1)
          AX3       X5,K      1 X2      X3=SIGN BITS OF X(K+2)
          BX2       X2-X4     2 X3      X2=ABS(X(K+1))
          BX3       X3-X5     3 X2      X3=ABS(X(K+2))
 +        SA4       A5+JX     4 X3      X4=X(K+3) A4=(X(K+3))
          IX0       X6-X2     5         X0=P(K)-ABS(X(K+1)) SEE NOTE
          S_J       J+1       6 X0                          J=K+1
*                             7
 +        PL        X0,*+1    8         IF P(K) < ABS(X(K+1))
          BX6       X2        9                   X6=ABS(X(K+1))=P(K+1)
          S_L       J        10 X6                L=K+1
 +        IX0       X6-X3    11 X4      X0=P(K+1)-ABS(X(K+2)) SEE NOTE
          SA5       A4+JX    12 X0      X5=X(K+4) A5=(X(K+4))
          S_J       J+1      13                             J=K+2
*                            14
 +        PL        X0,*+1   15         IF P(K+1) < ABS(X(K+2))
          BX6       X3       16                   X6=ABS(X(K+2))=P(K+2)
          S_L       J        17 X6                L=K+2
*                            18
*                            19 X5
 +        NE        J,N,[1]  20         IF J .NE. N (N = N-2)
 RMAXA2R  ENDM
 RVMAXA   MACRO     N,JX,L,K,J          X1=(X), X6=P
          LOCAL     [1],[2],[3],[99]
          MX5       59                  X5=777...776
          SX6       N                   X6=N
          S_L       1                                       L=1
          BX6       -X5*X6              X6=MOD(N,2)
          NZ        X6,[1]              IF N IS ODD
          SA4       X1                  X4=X(1)   A4=(X(1))
          S_K       59                          SHIFT COUNT K=59
          S_J       B0                                      J=0
          JP        [2]
 [1]      SA5       X1                  X5=X(1)   A5=(X(1))
          S_K       59                          SHIFT COUNT K=59
          S_J       1                                       J=1
          AX3       X5,K                X3=SIGN BITS OF X(1)
          BX6       X3-X5               X6=ABS(X(1))=P(1)
          EQ        N,J,[99]            IF N=1
          SA4       A5+JX               X4=X(2)   A4=(X(2))
 [2]      BSS       0                   X4=X(J+1)           J=MOD(N,2)
          SA5       A4+JX               X5=X(J+2) A5=(X(J+2))
          S_N       N-2                                     N=N-2
          EQ        J,N,[3]             IF N = 2 OR 3
          RMAXA2R   N,JX,L,K,J                              J=N-2
 [3]      AX2       X4,K                X2=SIGN BITS OF X(N-1)
          AX3       X5,K                X3=SIGN BITS OF X(N)
          BX2       X2-X4               X2=ABS(X(N-1))
          BX3       X3-X5               X3=ABS(X(N))
          IX0       X6-X2               X0=P(N-2)-ABS(X(N-1))
          S_J       J+1                                     J=N-1
 +        PL        X0,*+1              IF P(N-2) < ABS(X(N-1))
          BX6       X2                            X6=ABS(X(N-1))=P(N-1)
          S_L       J                             L=N-1
 +        IX0       X6-X3               X0=P(N-1)-ABS(X(N))
          S_N       N+2                             RESTORE N
 +        PL        X0,*+1              IF P(N-1) < ABS(X(N))
          BX6       X3                            X6=ABS(X(N))=P(N-1)
          S_L       N                             L=N
 [99]     BSS       0
 RVMAXA   ENDM
 CMAXA1R  MACRO     N,JX,L,K,J
          LOCAL     [1]
 [1]      BSS       0                   FOR K = 0, 1, 2, ...
 +        AX2       X4,K      0         X2=SIGN BITS OF X'(K+2)
          AX3       X5,K      1 X2      X3=SIGN BITS OF X''(K+2)
          BX2       X2-X4     2 X3      X2=ABS(X'(K+2))
          BX3       X3-X5     3 X2      X3=ABS(X''(K+2))
 +        SA4       A4+JX     4 X3      X4=X'(K+3)
          IX0       X6-X2     5         X0=P(K+1)-ABS(X'(K+2)) SEE NOTE
          S_J       J+1       6 X0                          J=1+K+1
*                             7
 +        PL        X0,*+1    8         IF P(K+1) < ABS(X'(K+2))
          BX6       X2        9                   X6=ABS(X'(K+2))
          S_L       J        10 X6                L=K+2
 +        IX0       X6-X3    11 X4      X0=P-ABS(X''(K+2)) SEE NOTE
          SA5       A5+JX    12 X0      X5=X''(K+3)
*                            13
          PL        X0,*+2   14         IF P < ABS(X''(K+2))
 +        BX6       X3       15                   X6=ABS(X''(K+2))=P(K+2)
          S_L       J        16 X6                L=K+2
          SB0       0        17
*                            18
*                            19 X5
 +        NE        J,N,[1]  20         IF J .NE. N (N = N-1)
 CMAXA1R  ENDM
 CVMAXA   MACRO     N,JX,L,K,J          X1=(X), X6=P
          LOCAL     [1],[99]
          SA4       X1                  X4=X'(1)  A4=(X'(1))
          SA5       X1+1                X5=X''(1) A5=(X''(1))
          S_K       59                          SHIFT COUNT K=59
          S_J       1                                       J=1
          AX2       X4,K                X2=SIGN BITS OF X'(1)
          AX3       X5,K                X3=SIGN BITS OF X''(1)
          BX6       X2-X4               X6=ABS(X'(1))
          BX3       X3-X5               X3=ABS(X''(1))
          S_L       J                                       L=1
          IX0       X6-X3               X0=ABS(X'(1))-ABS(X''(1))
 +        PL        X0,*+1              IF ABS(X'(1)) < ABS(X''(1))
          BX6       X3                            P(1)=ABS(X''(1))
          NO
 +        EQ        N,J,[99]            IF N=1
          SA4       A4+JX               X4=X'(2)  A4=(X'(2))
          SA5       A5+JX               X5=X''(2) A5=(X''(2))
          S_N       N-1                                     N=N-1
          EQ        N,J,[1]             IF N=2
          CMAXA1R   N,JX,L,K,J                              J=N-1
 [1]      AX2       X4,K                X2=SIGN BITS OF X'(N)
          AX3       X5,K                X3=SIGN BITS OF X''(N)
          BX2       X2-X4               X2=ABS(X'(N))
          BX3       X3-X5               X3=ABS(X''(N))
          IX0       X6-X2               X0=P(N-1)-ABS(X'(N))
          S_N       J+1                           RESTORE   N
 +        PL        X0,*+1              IF P(N-1) < ABS(X'(N))
          BX6       X2                            X6=ABS(X'(N))
          S_L       N
 +        IX0       X6-X3               X0=P-ABS(X''(N))
          PL        X0,[99]             IF P < ABS(X''(N))
          BX6       X3                            X6=ABS(X''(N))=P(N)
          S_L       N                             L=N
 [99]     BSS       0
 CVMAXA   ENDM
*CD       MPY
 RMPY2R   MACRO     &,JX,JY,J,ONE
          LOCAL     [1]
 [1]      BSS       0                   FOR K = 0, 1, 2, ...
 +        RX1       X4*X3     0         X1=X(1,2+K)*Y(2+K)
          NX6       X6        1 X5                X6=Z(1,1+K)
          RX2       X5*X3     2         X2=X(2,2+K)*Y(2+K)
          SA4       A4+JX     3 X6                X4=X(1,2+K)
 +        SA3       A3+JY     4 X1                X3=Y(2+K)
          NX7       X7        5                   X7=Z(2,1+K)
          RX6       X6_&_X1   6 X2      X6=Z(1,1+K) & X(1,2+K)*Y(2+K)
          SA5       A5+JX     7 X7                X5=X(2,2+K)
 +        S_J       J+ONE     8                             J=2-N+1+K
          RX7       X7_&_X2   9 X6      X7=Z(2,1+K) & X(2,2+K)*Y(2+K)
*                            10 X4
*                            11 X3
          MI        J,[1]    12 X7      WHILE 3-N+K=J<0; I.E. 4+K .LE. N
 RMPY2R   ENDM
 RMPY6R   MACRO     &,JX,JY,J
          LOCAL     [1]
 [1]      BSS       0                   FOR K = 0, 2, 4, ...
 +        NX6       X6        0                   X6=Z(1,K)
          RX1       X3*X2     1 X4      X1=X(1,1+K)*Y(1+K)
          NX7       X7        2 X6                X7=Z(2,K)
          RX4       X4*X2     3 X5      X4=X(2,1+K)*Y(1+K)
 +        NX0       X0        4 X7                X0=Z(3,K)
          RX5       X5*X2     5 X1      X5=X(3,1+K)*Y(1+K)
          SA3       A3+JX     6 X0                X3=X(1,2+K)
          RX6       X6_&_X1   7 X4      X6=Z(1,K) & X(1,1+K)*Y(1+K)
 +        SA2       A2+JY     8                   X2=Y(2+K)
          RX7       X7_&_X4   9 X5      X7=Z(2,K) & X(2,1+K)*Y(1+K)
          SA4       A4+JX    10 X6                X4=X(2,2+K)
          RX0       X0_&_X5  11         X0=Z(3,K) & X(3,1+K)*Y(1+K)
 +        SA5       A5+JX    12 X7                X5=X(3,2+K)
          S_J       J+2      13 X3                          J=2-N+K+2
          NX6       X6       14 X0      X6=Z(1,1+K)
*                            15 X2
 +        NX7       X7       16 X6      X7=Z(2,1+K)
          RX1       X3*X2    17 X4                X1=X(1,2+K)*Y(2+K)
          NX0       X0       18 X7      X0=Z(3,1+K)
          RX4       X4*X2    19 X5                X4=X(2,2+K)*Y(2+K)
 +        SA3       A3+JX    20 X0      X3=X(1,3+K)
          RX5       X5*X2    21 X1                X5=X(3,2+K)*Y(2+K)
          SA2       A2+JY    22         X2=Y(3+K)
          RX6       X6_&_X1  23 X4           X6=Z(1,1+K)&X(1,2+K)*Y(2+K)
*                            24
 +        RX7       X7_&_X4  25 X5           X7=Z(2,1+K)&X(2,2+K)*Y(2+K)
          SA4       A4+JX    26 X6      X4=X(2,3+K)
          RX0       X0_&_X5  27 X3           X0=Z(3,1+K)&X(3,2+K)*Y(2+K)
          SA5       A5+JX    28 X7      X5=X(3,3+K)
*                            29 X2
*                            30 X0
 +        MI        J,[1]    31         WHILE 4-N+K=J<0; I.E.5+K.LE.N
 RMPY6R   ENDM
 RMPY3H   MACRO     &,JX,JY
          RX1       X3*X2     1 X4      X1=X(1,1+K)*Y(1+K)
          NX7       X7        2 X6                X7=Z(2,K)
          RX4       X4*X2     3 X5      X4=X(2,1+K)*Y(1+K)
          NX0       X0        4 X7                X0=Z(3,K)
          RX5       X5*X2     5 X1      X5=X(3,1+K)*Y(1+K)
          SA3       A3+JX     6 X0                X3=X(1,2+K)
          RX6       X6_&_X1   7 X4      X6=Z(1,K) & X(1,1+K)*Y(1+K)
          SA2       A2+JY     8                   X2=Y(2+K)
          RX7       X7_&_X4   9 X5      X7=Z(2,K) & X(2,1+K)*Y(1+K)
          SA4       A4+JX    10 X6                X4=X(2,2+K)
          RX0       X0_&_X5  11         X0=Z(3,K) & X(3,1+K)*Y(1+K)
          SA5       A5+JX    12 X7                X5=X(3,2+K)
 RMPY3H   ENDM
 RMPY6T   MACRO     N,&,Z,IZ,SAVEX,SAVEY,J
 +        NX6       X6       32                   X6=Z(1,N-1)
          RX3       X3*X2    33 X4      X3=X(1,N)*Y(N)
          NX7       X7       34 X6                X7=Z(2,N-1)
          RX4       X4*X2    35         X4=X(2,N)*Y(N)
 +        NX0       X0       36 X7                X0=Z(3,N-1)
          RX5       X5*X2    37 X3      X5=X(3,N)*Y(N)
*                            38 X0
          RX6       X6_&_X3  39 X4      X6=Z(1,N-1) & X(1,N)*Y(N)
          RX7       X7_&_X4  40         X7=Z(2,N-1) & X(2,N)*Y(N)
 +        SA1       SAVEX    41 X5      X1=(X(4,1))
          RX0       X0_&_X5  42 X6      X0=Z(3,N-1) & X(3,N)*Y(N)
          S_J       J-N      43 X7                          J=J-N
 +        NX7       X7       44         X7=Z(2,N)
          NX6       X6       45 X0      X6=Z(1,N)
*                            46 X7
          SA7       Z+IZ     47 X6      Z(2)=Z(2,N)
          NX7       X0       48 X1      X7=Z(3,N)
 +        SA6       Z        49         Z(1)=Z(1,N)
          SA2       SAVEY    50 X7      X2=(Y(1))
          SA7       A7+IZ    51         Z(3)=Z(3,N)
*                            52
 +        S_Z       A7+IZ    53         Z=(Z(4))
 RMPY6T   ENDM
 RVMPY    MACRO     &,N,X,JX,Y,JY,#,Z,J
          LOCAL     [1],[2],[3],[4],[5],[6],[7],[99]
 [1]      BSS       0
 LAMBDA   IFC       NE,*#*0*
          SA1       Z                   X1=Z
 LAMBDA   ENDIF
          S_J       3                                       J=3
          SA2       X                   X2=X(1)
          S_J       J-N                                     J=3-N
          SA3       Y                   X3=Y(1)
          PL        J,[3]               IF N < 4
          SA4       A2+JX               X4=X(2)
          SA5       A3+JY               X5=Y(2)
 LAMBDA   IFC       EQ,*#*+*
          BX6       X1                  X6=Z
 LAMBDA   ENDIF
 LAMBDA   IFC       EQ,*#*-*
          BX6       -X1                 X6=-Z
 LAMBDA   ENDIF
 LAMBDA   IFC       EQ,*#*0*
          NO
          MX6       0                   X6=0.
 LAMBDA   ENDIF
          MX7       0                   X7=0.
 [2]      BSS       0                   FOR K = 0, 2, 4, ...
 +        S_J       J+2       0 X3                          J=3-N+K+2
          RX0       X2*X3     1 X4      X0=X(1+K)*Y(1+K)
          NX6       X6        2 X5
 +        RX1       X4*X5     3         X1=X(2+K)*Y(2+K)
          NX7       X7        4 X6
          SA2       A4+JX     5 X0      X2=X(3+K)
          RX6       X6_&_X0   6 X7      X6=X6 & X(1+K)*Y(1+K)
 +        SA3       A5+JY     7 X1      X3=Y(3+K)
          SA4       A2+JX     8         X4=X(4+K)
          SA5       A3+JY     9 X6      X5=Y(4+K)
          RX7       X7_&_X1  10         X7=X7 & X(2+K)*Y(2+K)
                             11
                             12 X2
 +        MI        J,[2]    13 X7      WHILE 5-N+K=J<0; I.E. 6+K.LE.N
          ZR        J,[6]    14 X3      IF N IS ODD AND ONE TERM IS LEFT
 +        RX0       X2*X3    15 X4      X0=X(N-1)*Y(N-1)
          NX6       X6       16 X5
          RX1       X4*X5    17         X1=X(N)*Y(N)
          NX7       X7       18 X6
                             19 X0
                             20 X7
 +        JP        [7]      21 X1
 [3]      SX6       J-1                 X6=2-N
 LAMBDA   IFC       EQ,*#*-*
          BX1       -X1                 X1=-Z
 LAMBDA   ENDIF
          ZR        J,[5]               IF N = 3
          ZR        X6,[4]              IF N = 2
 LAMBDA   IFC       NE,*#*0*
          RX0       X2*X3               X0=X(1)*Y(1)
          RX6       X1_&_X0             X6= # Z & X(1)*Y(1)
          NX6       X6
 LAMBDA   ELSE
          RX6       X2*X3               X6=X(1)*Y(1)
 SIGMA    IFC       EQ,*&*-*
          BX6       -X6                 X6= & X(1)*Y(1)
 SIGMA    ENDIF
 LAMBDA   ENDIF
          SA4       A2                            A4=(X(N))
          SA5       A3                            A5=(Y(N))
          JP        [99]
 [4]      SA4       A2+JX               X4=X(2)
          SA5       A3+JY               X5=Y(2)
 LAMBDA   IFC       NE,*#*0*
          RX0       X2*X3               X0=X(1)*Y(1)
          RX6       X1_&_X0             X6= # Z & X(1)*Y(1)
          RX7       X4*X5               X7=X(2)*Y(2)
          NX6       X6
          RX6       X6_&_X7             X6= # Z & (X(1)*Y(1)+X(2)*Y(2))
 LAMBDA   ELSE
          RX6       X2*X3               X6=X(1)*Y(1)
          RX7       X4*X5               X7=X(2)*Y(2)
          RX6       X6+X7               X6=X(1)*Y(1)+X(2)*Y(2)
 SIGMA    IFC       EQ,*&*-*
          BX6       -X6                 X6= & (X(1)*Y(1)+X(2)*Y(2))
 SIGMA    ENDIF
 LAMBDA   ENDIF
          NX6       X6                  X6= #Z & (X(1)*Y(1)+X(2)*Y(2))
          JP        [99]
 [5]      SA4       A2+JX               X4=X(2)
          SA5       A3+JY               X5=Y(2)
 LAMBDA   IFC       NE,*#*0*
          RX7       X2*X3               X7=X(1)*Y(1)
          SA2       A4+JX               X2=X(3)
          SA3       A5+JY               X3=Y(3)
 SIGMA    IFC       EQ,*&*-*
          BX7       -X7                 X7= & X(1)*Y(1)
 SIGMA    ENDIF
          RX4       X4*X5               X4=X(2)*Y(2)
          RX0       X2*X3               X0=X(3)*Y(3)
          RX7       X7_&_X4             X7= & (X(1)*Y(1)+X(2)*Y(2))
          RX6       X1_&_X0             X6= #Z & X(3)*Y(3)
          NX7       X7
          NX6       X6
          RX6       X6+X7            X6= # Z & (X(1)*Y(1)+...+X(3)*Y(3))
 LAMBDA   ELSE
          RX6       X2*X3               X6=X(1)*Y(1)
          SA2       A4+JX               X2=X(3)
          SA3       A5+JY               X3=Y(3)
          RX7       X4*X5               X7=X(2)*Y(2)
          RX0       X2*X3               X0=X(3)*Y(3)
          RX6       X6+X7               X6=X(1)*Y(1)+X(2)*Y(2)
          NX6       X6
          RX6       X6+X0               X6=X(1)*Y(1)+...+X(3)*Y(3)
 SIGMA    IFC       EQ,*&*-*
          BX6       -X6                 X6= & (X(1)*Y(1)+...+X(3)*Y(3))
 SIGMA    ENDIF
 LAMBDA   ENDIF
          SA4       A2                            A4=(X(N))
          SA5       A3                            A5=(Y(N))
          NX6       X6                  X6= # Z & (X(1)*Y(1)+...+X(3)*Y(3))
          JP        [99]
 [6]      RX0       X2*X3     0         X0=X(N-2)*Y(N-2)
          SA2       A4+JX     1         X2=X(N)
          SA3       A5+JY     2         X3=Y(N)
          RX1       X4*X5     3         X1=X(N-1)*Y(N-1)
 +        NX6       X6        4 X0
          SA4       A2        5                   A4=(X(N))
          SA5       A3        6 X6                A5=(Y(N))
          RX6       X6_&_X0   7 X1      X6=X6 & X(N-2)*Y(N-2)
 +        NO                  8 X2
          NX7       X7        9 X4
          RX0       X2*X3    10 X6      X0=X(N)*Y(N)
          NX6       X6       11 X7
 [7]      RX7       X7_&_X1
          RX6       X6_&_X0
          NX7       X7
          NX6       X6
          RX6       X6+X7            X6= # Z & (X(1)*Y(1)+...+X(N)*Y(N))
          NO
          NO
          NX6       X6
 [99]     BSS       0
 RVMPY    ENDM
 RMMPY2   MACRO     &,N,IX,JX,JY,#,Z,IZ,J,ONE     X1=(X), X2=(Y)
 LAMBDA   IFC       EQ,*#*0*            # IS 0
          SA4       X1                  X4=X(1,1) A4=(X(1,1))
          SA3       X2                  X3=Y(1)   A3=(Y(1))
          SA5       A4+IX               X5=X(2,1) A5=(X(2,1))
          S_J       -N
          S_J       J+2                                     J=2-N
          RX6       X4*X3               X6=X(1,1)*Y(1)
          SA4       A4+JX               X4=X(1,2) A4=(X(1,2))
          RX7       X5*X3               X7=X(2,1)*Y(1)
          SA3       A3+JY               X3=Y(2)   A3=(Y(2))
          SA5       A5+JX               X5=X(2,2) A5=(X(2,2))
 SIGMA    IFC       NE,*&*+*            & IS -
          BX6       -X6                 X6=Z(1,1)
          BX7       -X7                 X7=Z(2,1)
 SIGMA    ENDIF
 LAMBDA   ELSE                          # IS + OR -
          SA4       X1                  X4=X(1,1) A4=(X(1,1))
          SA3       X2                  X3=Y(1)   A3=(Y(1))
          SA1       Z                   X1=Z(1)
          SA2       Z+IZ                X2=Z(2)
          S_J       1
          S_J       J-N                                     J=1-N
          SA5       A4+IX               X5=X(2,1) A5=(X(2,1))
 LAMBDA2  IFC       EQ,*#*+*            # IS +
          BX6       X1                  X6=Z(1)
          BX7       X2                  X7=Z(2)
 LAMBDA2  ELSE                          # IS -
          BX6       -X1                 X6=-Z(1)
          BX7       -X2                 X7=-Z(2)
 LAMBDA2  ENDIF
 LAMBDA   ENDIF
          RMPY2R    &,JX,JY,J,ONE                           J=0
          RX1       X4*X3               X1=X(1,N)*Y(N)
          NX6       X6                  X6=Z(1,N-1)
          RX2       X5*X3               X2=X(2,N)*Y(N)
          NX7       X7                  X7=Z(2,N-1)
          RX6       X6_&_X1             X6=Z(1)
          RX7       X7_&_X2             X7=Z(2)
          NX6       X6
          NX7       X7
          SA6       Z                   Z(1)
          SA7       Z+IZ                Z(2)
 RMMPY2   ENDM
 RM2MPYR  MACRO     IX,IZ,I
          LOCAL     [1]
 [1]      BSS       0                   FOR K = 0, 2, 4, ...
 +        RX2       X2*X0     0         X2=X(4+K,1)*Y(1)
          S_I       I+2       1                             I=6-M+2+K
          RX3       X3*X1     2 X4      X3=X(4+K,2)*Y(2)
 +        NX6       X6        3 X5      X6=Z(2+K)
          RX4       X4*X0     4 X2      X4=X(5+K,1)*Y(1)
          NX7       X7        5 X6      X7=Z(3+K)
          RX5       X5*X1     6 X3      X5=X(5+K,2)*Y(2)
 +        SA6       A7+IZ     7 X7      Z(2+K)
          RX6       X2+X3     8 X4      X6=X(4+K,1)*Y(1)+X(4+K,2)*Y(2)
          SA2       A4+IX     9         X2=X(6+K,1)
          SA3       A5+IX    10 X5      X3=X(6+K,2)
 +        SA7       A6+IZ    11 X6      Z(3+K)
          RX7       X4+X5    12         X7=X(5+K,1)*Y(1)+X(5+K,2)*Y(2)
          SA4       A2+IX    13         X4=X(7+K,1)
          SA5       A3+IX    14         X5=X(7+K,2)
*                            15 X7
*                            16 X2
 +        MI        I,[1]    17 X3      WHILE 8-M+K<0; I.E. 9+K.LE.M
 RM2MPYR  ENDM
 RM2MPY   MACRO     &,M,IX,JX,JY,Z,IZ,I X1=(X(1,1)) X2=(Y(1))
          LOCAL     [1],[2],[3],[4],[5],[99]
          SA2       X2                  X2=Y(1)   A2=(Y(1))
          SA4       X1                  X4=X(1,1) A4=(X(1,1))
          SA1       A2+JY               X1=Y(2)   A1=(Y(2))
          SA5       A4+JX               X5=X(1,2) A5=(X(1,2))
 SIGMA    IFC       EQ,*&*+*            & IS +
          BX0       X2
 SIGMA    ELSE                          & IS -
          BX0       -X2                 X0=&Y(1)
          BX1       -X1                 X1=&Y(2)
 SIGMA    ENDIF
          BRANCH    [1],M,LE,6,I        I=6-M; IF M<7 GO TO [1]
          RX4       X4*X0               X4=X(1,1)*Y(1)
          SA2       A4+IX               X2=X(2,1) A2=(X(2,1))
          RX5       X5*X1               X5=X(1,2)*Y(2)
          SA3       A5+IX               X3=X(2,2) A3=(X(2,2))
          RX7       X4+X5               X7=X(1,1)*Y(1)+X(1,2)*Y(2)
          SA4       A2+IX               X4=X(3,1) A4=(X(3,1))
          SA5       A3+IX               X5=X(3,2) A5=(X(3,2))
          RX2       X2*X0               X2=X(2,1)*Y(1)
          RX3       X3*X1               X3=X(2,2)*Y(2)
          RX4       X4*X0               X4=X(3,1)*Y(1)
          NX7       X7                  X7=Z(1)
          RX5       X5*X1               X5=X(3,2)*Y(2)
          RX6       X2+X3               X6=X(2,1)*Y(1)+X(2,2)*Y(2)
          SA2       A4+IX               X2=X(4,1) A2=(X(4,1))
          SA3       A5+IX               X3=X(4,2) A3=(X(4,2))
          SA7       Z                   Z(1)
          RX7       X4+X5               X7=X(3,1)*Y(1)+X(3,2)*Y(2)
          SA4       A2+IX               X4=X(5,1) A4=(X(5,1))
          SA5       A3+IX               X5=X(5,2) A5=(X(5,2))
          RM2MPYR   IX,IZ,I             I=+1 FOR M=7,9,11,...
*                                       I=0  FOR M=8,10,12,...
          RX2       X2*X0               X2=X(M-2+I,1)*Y(1)
          RX3       X3*X1               X3=X(M-2+I,2)*Y(2)
          NX6       X6                  X6=Z(M-4+I)
          RX4       X4*X0               X4=X(M-1+I,1)*Y(1)
          NX7       X7                  X7=Z(M-3+I)
          RX5       X5*X1               X5=X(M-1+I,2)*Y(2)
          SA6       A7+IZ               Z(M-4+I)
          RX6       X2+X3               X6=Z(M-2+I)
          NZ        I,[5]               IF M IS ODD
          SA2       A4+IX               X2=X(M,1)
          SA3       A5+IX               X3=X(M,2)
          SA7       A6+IZ               Z(M-3)
          RX7       X4+X5               X7=Z(M-1)
          RX2       X2*X0               X2=X(M,1)*Y(1)
          RX3       X3*X1               X3=X(M,2)*Y(2)
          NX6       X6                  X6=Z(M-2)
          NX7       X7                  X7=Z(M-1)
          SA6       A7+IZ               Z(M-2)
          RX6       X2+X3               X6=Z(M)
          SA7       A6+IZ               Z(M-1)
          NX6       X6                  X6=Z(M)
          SA6       A7+IZ               Z(M)
          JP        [99]
 [1]      BSS       0                   M=1,2,3,...;  I=5,4,3,...
          RX2       X4*X0               X2=X(1,1)*Y(1)
          S_I       I-4                 M=1,2,3,...;  I=+1,0,-1,...
          RX3       X5*X1               X3=X(1,2)*Y(2)
          GT        I,B0,[4]            IF M=1; I.E. I=+1
          SA4       A4+IX               X4=X(2,1) A4=(X(2,1))
          SA5       A5+IX               X5=X(2,2) A5=(X(2,2))
          RX7       X2+X3               X7=X(1,1)*Y(1)+X(1,2)*Y(2)
          ZR        I,[3]               IF M=2; I.E. I=0
 [2]      BSS       0                   FOR K = 0, 1, 2, ...
 +        RX2       X4*X0     0 X5      X2=X(2+K,1)*Y(1)
          NX7       X7        1         X7=Z(1+K)
          RX3       X5*X1     2         X3=X(2+K,2)*Y(2)
          SA4       A4+IX     3 X7      X4=X(3+K,1)
 +        SA7       Z         4 X2      Z(1+K)
          SA5       A5+IX     5         X5=X(3+K,2)
          S_I       I+1       6 X3                          I=2-M+1+K
 +        S_Z       Z+IZ      7         Z=(Z(2+K))
          RX7       X2+X3     8         X7=X(2+K,1)*Y(1)+X(2+K,2)*Y(2)
*                             9
*                            10 X4
          MI        I,[2]    11 X7      WHILE 3-M+K<0; I.E. 4+K.LE.M
 [3]      RX2       X4*X0               X2=X(M,1)*Y(1)
          NX7       X7                  X7=Z(M-1)
          RX3       X5*X1               X3=X(M,2)*Y(2)
          SA7       Z                   Z(M-1)
          S_Z       Z+IZ                Z=(Z(M))
 [4]      RX7       X2+X3               X7=Z(M)
          NX7       X7                  X7=Z(M)
          SA7       Z                   Z(M)
          JP        [99]
 [5]      SA7       A6+IZ               Z(M-2)
          RX7       X4+X5               X7=X(M,1)*Y(1)+X(M,2)*Y(2)
          NX6       X6                  X6=Z(M-1)
          NX7       X7                  X7=Z(M)
          SA6       A7+IZ               Z(M-1)
          SB0       0
          SA7       A6+IZ               Z(M)
 [99]     BSS       0
 RM2MPY   ENDM
 RM2MPAR  MACRO     &,IX,#,IZ,I         X0=&Y(1) X1=&Y(2)
          LOCAL     [1]
 [1]      BSS       0                   FOR K = 0, 2, 4, ...
          RX6       X6_&_X7   0 X4      X6=Z(1+K)
          RX4       X4*X0     1 X3      X4=X(2+K,1)*Y(1)
*                             2
 LAMBDA   IFC       EQ,*#*-*            # IS -
          BX3       -X3       3 X6
 LAMBDA   ENDIF
          RX7       X5*X1     3 X6      X7=X(2+K,2)*Y(2)
          NX6       X6        4
          SA5       A5+IX     5 X4      X5=X(3+K,2) A5=(X(3+K,2))
          RX2       X3_&_X4   6 X6      X2=#Z(2+K)&X(2+K,1)*Y(1)
          SA6       A3-IZ     7 X7      Z(1+K)=#Z(1+K)&(...
          SA4       A4+IX     8         X4=X(3+K,1) A4=(X(3+K,1))
          SA3       A3+IZ     9 X2      X3=Z(3+K)   A3=(Z(3+K))
          S_I       I+1      10                             I=2-M+1+K
          NX6       X2       11
*                            12 X5
*                            13 X6
          MI        I,[1]    14         WHILE 3-M+K<0; I.E. 4+K .LE. M
 RM2MPAR  ENDM
 RM2MPA   MACRO     &,M,IX,JX,JY,#,Z,IZ,I
          LOCAL     [1],[2].[99]
          SA2       X2                  X2=Y(1)   A2=(Y(1))
          SA4       X1                  X4=X(1,1) A4=(X(1,1))
          SA1       A2+JY               X1=Y(2)   A1=(Y(2))
          SA3       Z                   X3=Z(1)   A3=(Z(1))
          SA5       A4+JX               X5=X(1,2) A5=(X(1,2))
          S_I       2
          BX0       X2                  X0=Y(1)
          S_I       I-M                                     I=2-M
          RX4       X4*X0               X4=X(1,1)*Y(1)
 LAMBDA   IFC       EQ,*#*-*            # IS -
          BX3       -X3                 X3=#Z(1)
 LAMBDA   ENDIF
          RX7       X5*X1               X7=X(1,2)*Y(2)
          LE        I,B0,[1]            IF M = 2,3,....
          RX2       X3_&_X4             X2=#Z(1)&X(1,1)*Y(1)
          NX6       X2                  X6=...
          RX6       X6_&_X7           X6=#Z(1)&(X(1,1)*Y(1)+X(1,2)*Y(2))
          NX6       X6
          SA6       A3                  Z(1)=#Z(1)&...
          JP        [99]
 [1]      SA5       A5+IX               X5=X(2,2) A5=(X(2,2))
          RX2       X3_&_X4             X2=#Z(1)&X(1,1)*Y(1)
          SA4       A4+IX               X4=X(2,1) A4=(X(2,1))
          SA3       A3+IZ               X3=Z(2)   A3=(Z(2))
          NX6       X2                  X6=#Z(1)&X(1,1)*Y(1)
          ZR        I,[2]               IF M=2
          RM2MPAR   &,IX,#,IZ,I                             I=0
 [2]      RX6       X6_&_X7             X7=#Z(M-1)&(X(M-1,1)*Y(1)+...)
          RX4       X4*X0               X4=X(M,1)*Y(1)
 LAMBDA   IFC       EQ,*#*-*
          BX3       -X3                 X3=#Z(M)
 LAMBDA   ENDIF
          RX7       X5*X1               X7=X(M,2)*Y(2)
          NX6       X6
          RX2       X3_&_X4             X2=#Z(M)&X(M,1)*Y(1)
          SA6       A3-IZ               Z(M-1)=#Z(M-1) (...)
          NX6       X2                  X6=X2
          RX6       X6_&_X7             X6=#Z(M)&(X(M,1)*Y(1)+...)
          NX6       X6
          SA6       A6+IZ               Z(M)=#Z(M)&(...)
 [99]     BSS       0
 RM2MPA   ENDM
 RMMPY6H  MACRO     &,IX,#,Z,IZ,SAVEX,I
 LAMBDA   IFC       EQ,*#*0*            # IS 0
*                                       X1=(X(1,1))
*                                       X2=(Y(1))
 +        SA3       X1        0         X3=X(1,1) A3=(X(1,1))
          SA4       X1+IX     1         X4=X(2,1) A4=(X(2,1))
          SA2       X2        2         X2=Y(1)   A2=(Y(1))
          SA5       A4+IX     3         X5=X(3,1) A5=(X(3,1))
 +        MX6       0         4         X6=0.
          SX7       A5+IX     5 X6      X7=(X(4,1))
*                             6 X7
          SA7       SAVEX     7 X3      SAVEX=(X(4,1))
 +        S_I       I+3       8 X4                          I=I+3
*                             9 X2
          MX7       0        10 X5      X7=0.
          MX0       0        11 X7      X0=0.
*                            12 X0
 LAMBDA   ELSE                          # IS + OR -
*                                       X1=(X(1,1))
*                                       X2=(Y(1))
 +        SA3       Z         0         X3=Z(1)   A3=(Z(1))
          SA4       Z+IZ      1         X4=Z(2)   A4=(Z(2))
          SX6       X1+IX     2         X6=(X(2,1))
          SX7       IX+IX     3 X6      X7=2*IX
          SA5       A4+IZ     4 X7      X5=Z(3)   A5=(Z(3))
          IX7       X6+X7     5         X7=(X(4,1))
*                             6 X7
          SA7       SAVEX     7 X3      SAVEX=(X(4,1))
 LAMBDA2  IFC       EQ,*#*+*
          BX6       X3        8 X4
          BX7       X4        9 X6
 LAMBDA2  ELSE
          BX6       -X3       8 X4      X6=#Z(1)
          BX7       -X4       9 X6      X7=#Z(2)
 LAMBDA2  ENDIF
          SA2       X2       10 X7      X2=Y(1)   A2=(Y(1))
          SA3       X1       11 X5      X3=X(1,1) A3=(X(1,1))
 LAMBDA2  IFC       EQ,*#*+*
          BX0       X5
 LAMBDA2  ELSE
          BX0       -X5                 X0=#Z(3)
 LAMBDA2  ENDIF
          SA4       X1+IX               X4=X(2,1) A4=(X(2,1))
          S_I       I+3                                     I=I+3
          SA5       A4+IX               X5=X(3,1) A5=(X(3,1))
 LAMBDA   ENDIF
 RMMPY6H  ENDM
 RMMPY    MACRO     &,I,N,IX,JX,JY,#,Z,IZ,TEMP,J   X1=(X), X2=(Y)
          LOCAL     [1],[2],[3],[4],[5],[6],[99],SAVEX,SAVEY
 SAVEX    EQU       TEMP
 SAVEY    EQU       TEMP+1
          SX5       N                   X5=N
          S_J       -N
          BX7       X2                  X7=(Y)
          S_J       J+2                 J=2-N=+1,0,-1,...; N=1,2,3,...
          SA7       SAVEY               SAVEY=(Y)
          ZR        J,[5]               IF N=2 GO TO [5]
          PL        J,[4]               IF N=1 GO TO [4]
          S_I       -I
          LX5       59                  SIGN(X5)=MOD(N,2)
          S_I       I+2                 I=2-M=+1,0,-1,...; M=1,2,3,...
          ZR        I,[2]               IF N>2 AND M=2 GO TO [2]
          PL        I,[3]               IF N>2 AND M=1 GO TO [3]
          MI        X5,[6]              IF M>2 AND N=3,5,7,... GO TO [6]
 [1]      BSS       0                   N=4,6,8,...; J=2-N=-2,-4,-6,...
          RMMPY6H   &,IX,#,Z,IZ,SAVEX,I   I=I+3=2,1,0,...; M=3,4,5,...
          RMPY3H    &,JX,JY
          RMPY6R    &,JX,JY,J           J=0
          S_J       2                   J=2
          RMPY6T    N,&,Z,IZ,SAVEX,SAVEY,J     J=J-N=2-N
          MI        I,[1]               WHILE I<0 PROCESS NEXT 3 ROWS
          S_I       I-1                 I=+1,0,-1;  M=0,1,2
          ZR        I,[3]               IF M=1 PROCESS LAST ROW
          PL        I,[99]              IF M=0 ALL DONE
 [2]      S_I       1                   I=1
          RMMPY2    &,N,IX,JX,JY,#,Z,IZ,J,I
          JP        [99]
 [3]      BX0       X1                  X0=(X)
          BX3       X2                  X3=(Y)
          RVMPY     &,N,X0,JX,X3,JY,#,Z,J
          SA6       Z                   Z(1)
          JP        [99]
 [4]      BSS       0
 LAMBDA   IFC       EQ,*#*0*            # IS 0
          S_N       X1                  N=(X) SORRY
          SA1       X2                  X1=Y(1)
 SIGMA    IFC       EQ,*&*-*
          BX1       -X1                 X1=&Y(1)
 SIGMA    ENDIF
          RVSCL     I,N,IX,Z,IZ,J
          S_N       1                   RESTORE N
 LAMBDA   ELSE                          # IS + OR -
          BX4       X1                  X4=(X)
          SA1       X2                  X1=Y      A1=(Y)
          SX2       Z                   X2=(Z)
 SIGMA    IFC       EQ,*&*-*
          BX1       -X1                 X1=&Y
 SIGMA    ENDIF
          RVSCA     I,IX,#,IZ,Z,IZ,J    Z(J)=&X(J,1)*Y(1) # Z(J)
 LAMBDA   ENDIF
          JP        [99]
 [5]      BSS       0
 LAMBDA   IFC       EQ,*#*0*            # IS 0
          RM2MPY    &,I,IX,JX,JY,Z,IZ,J
 LAMBDA   ELSE                # IS + OR -
          RM2MPA    &,I,IX,JX,JY,#,Z,IZ,J
 LAMBDA   ENDIF
          JP        [99]
 [6]      BSS       0                   N=3,5,7...; J=2-N=-1,-3,-5,...
          RMMPY6H   &,IX,#,Z,IZ,SAVEX,I  I=I+3=2,1,0,...; M=3,4,5,...
          RMPY6R    &,JX,JY,J           J=+1
          S_J       2                   J=2
          RMPY6T    N,&,Z,IZ,SAVEX,SAVEY,J    J=J-N=2-N
          MI        I,[6]               WHILE I<0 PROCESS NEXT 3 ROWS
          S_I       I-1                 I=+1,0,-1;  M=0,1,2
          ZR        I,[3]               IF M=1 PROCESS LAST ROW
          MI        I,[2]               IF M=2 PROCESS LAST 2 ROWS
 [99]     BSS       0
 RMMPY    ENDM
 RMBIL6T  MACRO     N,IX,SXYZ,SAVEY,SAVEZ,J     A0=(X)
          S_J       2                                       J=2
 +        NX6       X6                  X6=SYZ(1,N-1)
          RX3       X3*X2               X3=Y(1,N)*Z(N)
          NX7       X7                  X7=SYZ(2,N-1)
          RX4       X4*X2               X4=Y(2,N)*Z(N)
 +        NX0       X0                  X0=SYZ(3,N-1)
          RX5       X5*X2               X5=Y(3,N)*Z(N)
          SA1       SAVEY               X1=(Y(4,1))
 +        RX6       X6+X3               X6=SYZ(1,N-1)+Y(1,N)*Z(N)
          RX7       X7+X4               X7=SYZ(2,N-1)+Y(2,N)*Z(N)
          SA4       A0+IX               X4=X(2)   A4=(X(2))
          SA3       A0                  X3=X(1)   A3=(X(1))
 +        RX0       X0+X5               X0=SYZ(3,N-1)+Y(3,N)*Z(N)
          SA5       A4+IX               X5=X(3)   A5=(X(3))
          NX6       X6                  X6=SYZ(1,N)
          S_J       J-N                                     J=2-N
 +        SA2       SXYZ                X2=SXYZ(0)
          NX7       X7                  X7=SYZ(2,N)
          SA0       A5+IX                         A0=(X(4))
 +        NX0       X0                  X0=SYZ(3,N)
          RX7       X4*X7               X7=X(2)*SYZ(2,N)
          RX6       X3*X6               X6=X(1)*SYZ(1,N)
          RX0       X5*X0               X0=X(3)*SYZ(3,N)
 +        RX7       X7+X2               X7=X(2)*SYZ(2,N)+SXYZ(0)
          RX6       X6+X0               X6=X(1)*SYZ(1,N)+X(3)*SYZ(3,N)
          NX7       X7
          NX6       X6
 +        RX6       X6+X7               X6=SXYZ(3)
          SA2       SAVEZ               X2=(Z(1))
          NX6       X6
          SA6       SXYZ                SXYZ=SXYZ(3)
 RMBIL6T  ENDM
 RMBIL    MACRO     N,IX,IY,JY,JZ,TEMP3,I,J  A0=(X), X1=(Y), X2=(Z)
          LOCAL     [1],[2],[3],[4],[5],[6],[99]
          LOCAL     SXYZ,SAVEY,SAVEZ
 SXYZ     EQU       TEMP3
 SAVEY    EQU       TEMP3+1
 SAVEZ    EQU       TEMP3+2
          SX5       N                   X5=N
          S_I       -N                                      I=-N
          BX7       X2                  X7=(Z)
          MX6       0                   X6=0.
          S_J       I+2                                     J=2-N
          SA7       SAVEZ               SAVEZ=(Z(1))
          SA6       SXYZ                SXYZ=SXYZ(0)=0.
          ZR        J,[5]               IF N=2 GOTO [5]
          PL        J,[4]               IF N=1 GOTO [4]
          LX5       59                  SIGN(X5)=MOD(N,2)
          S_I       J                                       I=2-N
          MI        X5,[6]              IF N=3,5,7,... GOTO [6]
 [1]      BSS       0                   N=4,6,8,...; J=2-N
*                                       X1=(Y(I,1))
*                                       X2=(Z(1))
          RMMPY6H   "PLUS",IY,"NULL",B0,B0,SAVEY,I
          RMPY3H    "PLUS",JY,JZ
          RMPY6R    "PLUS",JY,JZ,J                          J=0
          RMBIL6T   N,IX,SXYZ,SAVEY,SAVEZ,J  X6=SXYZ
          MI        I,[1]               WHILE I<0 PROCESS NEXT 3 ROWS
          S_I       I-1                 I=+1,0,-1; N=0,1,2
          ZR        I,[3]               IF N=1 PROCESS LAST ROW
          PL        I,[99]              IF N=0 ALL DONE
 [2]      SA4       X1                  X4=Y(N-1,1) A4=(Y(N-1,1))
          SA3       X2                  X3=Z(1)     A3=(Z(1))
          SA5       A4+IY               X5=Y(N,1)   A5=(Y(N,1))
          S_J       -N
          S_I       1                                       I=1
          S_J       J+2                                     J=2-N
          RX6       X4*X3               X6=Y(N-1,1)*Z(1)
          SA4       A4+JY               X4=Y(N-1,2) A4=(Y(N-1,2))
          RX7       X5*X3               X7=Y(N,1)*Z(1)
          SA3       A3+JZ               X3=Z(2)     A3=(Z(2))
          SA5       A5+JY               X5=Y(N,2)   A5=(Y(N,2))
          RMPY2R    "PLUS",JY,JZ,J,I                        J=0
          RX1       X4*X3               X1=Y(N-1,N)*Z(N)
          NX6       X6                  X6=SYZ(N-1,N-1)
          RX2       X5*X3               X2=Y(N,N)*Z(N)
          NX7       X7                  X7=SYZ(N,N-1)
          SA4       A0                  X4=X(N-1) A4=(X(N-1))
          SA5       A0+IX               X5=X(N)   A5=(X(N))
          RX6       X6+X1               X6=SYZ(N-1,N-1)+Y(N-1,N)*Z(N)
          RX7       X7+X2               X7=SYZ(N,N-1)+Y(N,N)*Z(N)
          SA3       SXYZ                X3=SXYZ(N-2)
          NX6       X6                  X6=SYZ(N-1,N)
          NX7       X7                  X7=SYZ(N,N)
          RX6       X4*X6               X6=X(N-1)*SYZ(N-1,N)
          RX7       X5*X7               X7=X(N)*SYZ(N,N)
          RX6       X3+X6               X6=SXYZ(N-2)+X(N-1)*SYZ(N-1,N)
          NX6       X6                  X6=SXYZ(N-1)
          RX6       X6+X7               X6=SXYZ(N-1)+X(N)*SYZ(N,N)
          NX6       X6                  X6=SXYZ(N)=RMBIL
          JP        [99]
 [3]      BX0       X1                  X0=(Y(N,1))
          BX3       X2                  X3=(Z(1))
          RVMPY     "PLUS",N,X0,JY,X3,JZ,"NULL",B0,J
          SA1       A0                  X1=X(N)   A1=(X(N))
          SA2       SXYZ                X2=SXYZ(N-1)
          RX6       X1*X6               X6=X(N)*SYZ(N,N)
          RX6       X2+X6               X6=SXYZ(N-1)+X(N)*SYZ(N,N)
          NX6       X6                  X6=SXYZ(N)=RMBIL
          JP        [99]
 [4]      SA3       A0                  X3=X(1)
          SA4       X1                  X4=Y(1,1)
          SA5       X2                  X5=Z(1)
          RX6       X3*X4               X6=X(1)*Y(1,1)
          RX6       X6*X5               X6=SXYZ(1)=RMBIL
          JP        [99]
 [5]      SA4       X1                  X4=Y(1,1) A4=(Y(1,1))
          SA3       X2                  X3=Z(1)   A3=(Z(1))
          SA5       A4+IY               X5=Y(2,1) A5=(Y(2,1))
          RX6       X4*X3               X6=Y(1,1)*Z(1)
          SA4       A4+JY               X4=Y(1,2) A4=(Y(1,2))
          RX7       X5*X3               X7=Y(2,1)*Z(1)
          SA3       A3+JZ               X3=Z(2)   A3=(Z(2))
          SA5       A5+JY               X5=Y(2,2) A5=(Y(2,2))
          SA1       A0                  X1=X(1)   A1=(X(1))
          SA2       A0+IX               X2=X(2)   A2=(X(2))
          RX4       X4*X3               X4=Y(1,2)*Z(2)
          RX5       X5*X3               X5=Y(2,2)*Z(2)
          RX6       X6+X4               X6=Y(1,1)*Z(1)+Y(1,2)*Z(2)
          RX7       X7+X5               X7=Y(2,1)*Z(1)+Y(2,2)*Z(2)
          NX6       X6                  X6=SYZ(1)
          NX7       X7                  X7=SYZ(2)
          RX6       X1*X6               X6=X(1)*SYZ(1)
          RX7       X2*X7               X7=X(2)*SYZ(2)
          RX6       X6+X7               X6=X(1)*SYZ(1)+X(2)*SYZ(2)
          NX6       X6                  X6=SXYZ(2)=RMBIL
          JP        [99]
 [6]      BSS       0                   N=3,5,7,...; J=2-N
*                                       X1=(Y(I,1))
*                                       X2=(Z(1))
          RMMPY6H   "PLUS",IY,"NULL",B0,B0,SAVEY,I
          RMPY6R    "PLUS",JY,JZ,J                   J=+1
          RMBIL6T   N,IX,SXYZ,SAVEY,SAVEZ,J   X6=SXYZ
          MI        I,[6]               WHILE I<0 PROCESS NEXT 3 ROWS
          S_I       I-1                 I=+1,0,-1; N=0,1,2
          ZR        I,[3]               IF N=1 PROCESS LAST ROW
          MI        I,[2]               IF N=2 PROCESS LAST 2 ROWS
 [99]     BSS       0                   X6=RMBIL
 RMBIL    ENDM
 RUMPY6H  MACRO     N,&,IX,JX,JY,#,Z,IZ,SAVEX,SAVEY,J
 LAMBDA   IFC       EQ,*#*0*            # IS 0
*                                       X1=(X(1,1))
*                                       X2=(Y(1))
 +        SA3       X1        0         X3=X(1,1) A3=(X(1,1))
          SA2       X2        1         X2=Y(1)   A2=(Y(1))
          SA4       A3+JX     2         X4=X(1,2) A4=(X(1,2))
          SA5       A2+JY     3         X5=Y(2)   A5=(Y(2)
 +        SX0       J         4         X0=J  (TO SAVE J)
          SA1       A4+IX     5 X0      X1=X(2,2) A1=(X(2,2))
          S_J       IX+JX     6                             J=IX+JX
*                             7 X3
          S_J       J+J       8 X2                          J=2IX+2JX
 +        RX6       X3*X2     9 X4      X6=X(1,1)*Y(1)
          SX7       A1+J     10 X5      X7=(X(4,4))
          RX4       X4*X5    11 X7      X4=X(1,2)*Y(2)
*                            12 X1
          RX1       X1*X5    13 X6      X1=X(2,2)*Y(2)
 +        SA7       SAVEX    14         SAVEX=(X(4,4))
 SIGMA    IFC       EQ,*&*+* 15 X4      X6=&X(1,1)*Y(1)
          BX6       X6
 SIGMA    ELSE
          BX6       -X6
 SIGMA    ENDIF
          SA2       A5+JY    16 X6      X2=Y(3)   A2=(Y(3))
 +        RX6       X6_&_X4  17 X1      X6=&X(1,1)*Y(1)&X(1,2)*Y(2)
          SX7       A2+JY    18         X7=(Y(4))
          SA3       A4+JX    19 X7      X3=X(1,3) A3=(X(1,3))
          SA4       A1+JX    20 X6      X4=X(2,3) A4=(X(2,3))
*                            21
 +        SA5       A4+IX    22         X5=X(3,3) A5=(X(3,3))
          SA7       SAVEY    23 X2      SAVEY=(Y(4))
 SIGMA    IFC       EQ,*&*+* 24         X7=&X(2,2)*Y(2)
          BX7       X1
 SIGMA    ELSE
          BX7       -X1
 SIGMA    ENDIF
*                            25
 +        S_J       X0       26 X3                          RESTORE J
          MX0       0        27 X4      X0=0.
          S_N       N-3      28 X0                          N=N-3
          NX6       X6       29         NORMALIZE X6
 LAMBDA   ELSE                          # IS + OR -
*                                       X1=(X(1,1))
*                                       X2=(Y(1))
 +        SA4       X1        0         X4=X(1,1) A4=(X(1,1))
          SA5       X2        1         X5=Y(1)   A5=(Y(1))
          SA1       Z         2         X1=Z(1)   A1=(Z(1))
          SA3       A4+JX     3         X3=X(1,2) A3=(X(1,2))
 +        SX0       J         4         X0=J  (TO SAVE J)
          S_J       IX+JX     5 X0                          J=IX+JX
          NO                  6
          SA2       A5+JY     7 X4      X2=Y(2)   A2=(Y(2))
 +        S_J       J+J       8 X5                          J=2IX+2JX
          RX6       X4*X5     9 X1      X6=X(1,1)*Y(1)
 LAMBDA2  IFC       EQ,*#*+* 10 X3      X1=#Z(1)
          BX1       X1
 LAMBDA2  ELSE
          BX1       -X1
 LAMBDA2  ENDIF
          SA4       A3+IX    11 X1      X4=X(2,2) A4=(X(2,2))
*                            12
 +        SA5       Z+IZ     13 X6      X5=Z(2)   A5=(Z(2))
          RX6       X1_&_X6  14 X2      X6=#Z(1)&X(1,1)*Y(1)
          RX3       X3*X2    15         X3=X(1,2)*Y(2)
          SA1       A5+IZ    16         X1=Z(3)   A1=(Z(3))
*                            17 X6
*                            18 X4
 +        NX6       X6       19 X3
          RX4       X4*X2    20 X5      X4=X(2,2)*Y(2)
          SX7       A4+J     21 X6      X7=(X(4,4))
          RX6       X6_&_X3  22 X7      X6=#Z(1)&X(1,1)*Y(1)&X(1,2)*Y(2)
 +        SA7       SAVEX    23 X1      SAVEX=(X(4,4))
          SA2       A2+JY    24 X4      X2=Y(3)   A2=(Y(3))
          S_J       X0       25 X6                          RESTORE J
 +        SA3       A3+JX    26         X3=X(1,3) A3=(X(1,3))
 LAMBDA2  IFC       EQ,*#*+* 27         X5=#Z(2)
          BX5       X5
 LAMBDA2  ELSE
          BX5       -X5
 LAMBDA2  ENDIF
          SX7       A2+JY    28 X5      X7=(Y(4))
 LAMBDA2  IFC       EQ,*#*+* 29 X7      X0=#Z(3)
          BX0       X1
 LAMBDA2  ELSE
          BX0       -X1
 LAMBDA2  ENDIF
 +        NX6       X6       30 X0
          SA7       SAVEY    31 X2      SAVEY=(Y(4))
          RX7       X5_&_X4  32 X6      X7=#Z(2)&X(2,2)*Y(2)
 +        SA4       A4+JX    33 X3      X4=X(2,3) A4=(X(2,3))
*                            34
          SA5       A4+IX    35 X7      X5=X(3,3) A5=(X(3,3))
          S_N       N-3      36                             N=N-3
 LAMBDA   ENDIF
 RUMPY6H  ENDM
 RUMPY    MACRO     &,N,IX,JX,JY,#,Z,IZ,TEMP,J   X1=(X), X2=(Y)
          LOCAL     [1],[2],[3],[4],[5],[6],[7],[99]
          LOCAL     SAVEX,SAVEY
 SAVEX    EQU       TEMP
 SAVEY    EQU       TEMP+1
          SX5       N                   X5=N
          LX5       59                  SIGN(X5)=MOD(N,2)
          BRANCH    [4],N,LE,4,J        J=4-N;  IF N<5 GO TO [4]
          MI        X5,[2]              IF N IS ODD GO TO [2]
 [1]      BSS       0                 N = 6,8,10,...; J=4-N=-2,-4,-6,...
          RUMPY6H   N,&,IX,JX,JY,#,Z,IZ,SAVEX,SAVEY,J  N=N-3=3,5,7,...
          RMPY3H    &,JX,JY
          RMPY6R    &,JX,JY,J           J=0
          S_J       4
          RMPY6T    N,&,Z,IZ,SAVEX,SAVEY,J  J=+1,-1,-3,...
          PL        J,[5]               IF N=3 GO TO [5]
 [2]      BSS       0                   N=5,7,9,...; J=4-N=-1,-3,-5,...
          RUMPY6H   N,&,IX,JX,JY,#,Z,IZ,SAVEX,SAVEY,J  N=N-3=2,4,6,...
          RMPY6R    &,JX,JY,J           J=0
          S_J       4
          RMPY6T    N,&,Z,IZ,SAVEX,SAVEY,J  J=+2,0,-2,...
          MI        J,[1]               IF N=6,8,10,...
          ZR        J,[6]               IF N=4 GO TO [6]
 [3]      BSS       0                   N = 2
 LAMBDA   IFC       EQ,*#*0*
          SA1       X1                  X1=X(1,1) A1=(X(1,1))
          SA2       X2                  X2=Y(1)   A2=(Y(1))
          SA4       A1+JX               X4=X(1,2) A4=(X(1,2))
          SA3       A2+JY               X3=Y(2)   A3=(Y(2))
          SA5       A4+IX               X5=X(2,2) A5=(X(2,2))
          RX6       X1*X2               X6=X(1,1)*Y(1)
          RX1       X4*X3               X1=X(1,2)*Y(2)
          RX7       X5*X3               X7=X(2,2)*Y(2)
          RX6       X6+X1               X6=X(1,1)*Y(1)+X(1,2)*Y(2)
 SIGMA    IFC       EQ,*&*-*
          BX7       -X7                 X7=Z(2)
          BX6       -X6                 X6=Z(1)
 SIGMA    ENDIF
          NX6       X6
          SA7       Z+IZ                Z(2)=&X(2,2)*Y(2)
          SA6       Z                   Z(1)=&(X(1,1)*Y(1)+X(1,2)*Y(2))
 LAMBDA   ELSE
          SA4       X1                  X4=X(1,1) A4=(X(1,1))
          SA3       X2                  X3=Y(1)   A3=(Y(1))
          SA1       Z                   X1=Z(1)
          SA2       Z+IZ                X2=Z(2)
          RX0       X4*X3               X0=X(1,1)*Y(1)
          SA4       A4+JX               X4=X(1,2) A4=(X(1,2))
          SA3       A3+JY               X3=Y(2)   A3=(Y(2))
          SA5       A4+IX               X5=X(2,2) A5=(X(2,2))
 LAMBDA2  IFC       EQ,*#*+*
          BX6       X1                  X6=#Z(1)
          BX7       X2                  X7=#Z(2)
 LAMBDA2  ELSE
          BX6       -X1
          BX7       -X2
 LAMBDA2  ENDIF
          RX1       X4*X3               X1=X(1,2)*Y(2)
          RX6       X6_&_X0             X6=&X(1,1)*Y(1) # Z(1)
          RX0       X5*X3               X0=X(2,2)*Y(2)
          NX6       X6
          RX7       X7_&_X0             X7=&X(2,2)*Y(2) # Z(2)
          RX6       X6_&_X1           X6=&(X(1,1)*Y(1)+X(1,2)*Y(2))#Z(1)
          NX7       X7
          NX6       X6
          SA7       Z+IZ
          SA6       Z
 LAMBDA   ENDIF
          JP        [99]                GO TO [99]
 [4]      BSS       0                   N=1,2,3,4; J=4-N=3,2,1,0
          ZR        J,[6]               IF N=4 GO TO [6]
          S_J       J-2                 J=+1,0,-1
          ZR        J,[3]               IF N=2 GO TO [3]
          PL        J,[7]               IF N=1 GO TO [7]
 [5]      BSS       0                   N = 3
          RUMPY6H   N,&,IX,JX,JY,#,Z,IZ,SAVEX,SAVEY,J
          RMPY6T    N,&,Z,IZ,SAVEX,SAVEY,J
          JP        [99]                GO TO [99]
 [6]      BSS       0                   N = 4
          RUMPY6H   N,&,IX,JX,JY,#,Z,IZ,SAVEX,SAVEY,J
          RMPY3H    &,JX,JY
          RMPY6T    N,&,Z,IZ,SAVEX,SAVEY,J  N = 1
 [7]      BSS       0                   N = 1
          SA5       X1                  X5=X(1,1)
          SA4       X2                  X4=Y(1)
 LAMBDA   IFC       EQ,*#*0*            # IS NULL
          RX6       X5*X4               X6=X(1,1)*Y(1)
 SIGMA    IFC       EQ,*&*-*                      & IS -
          BX6       -X6                 X6=&X(1,1)*Y(1)
 SIGMA    ENDIF
          SA6       Z                   Z(1)=&X(1,1)*Y(1)
 LAMBDA   ELSE                          # IS + OR -
          SA3       Z                   X3=Z(1)
          RX6       X5*X4               X6=X(1,1)*Y(1)
 LAMBDA2  IFC       EQ,*#*-*                      # IS -
          BX3       -X3                 X3=#Z(1)
 LAMBDA2  ENDIF
          RX6       X3_&_X6             X6=&X(1,1)*Y(1) # Z(1)
          NX6       X6
          SA6       Z                   Z(1)=&X(1,1)*Y(1) # Z(1)
 LAMBDA   ENDIF
 [99]     BSS       0
 RUMPY    ENDM
 DMPY1R   MACRO     &,JX,JY,J
          LOCAL     [1]
 [1]      BSS       0                   FOR K = 0, 1, 2, ...
 +        FX6       X0+X7     0 X4      X6=U(C'(1+K)+F'(1+K))=G'(1+K)
          DX7       X0+X7     1 X2      X7=L(C'(1+K)+F'(1+K))=G''(1+K)
          FX3       X3*X4     2         X3=U(X''(2+K)*Y'(2+K))
*                             3 X6
          FX5       X2*X5     4 X7      X5=U(X'(2+K)*Y''(2+K))
 +        NX1       X6        5         X1=G'(1+K)
          FX0       X2*X4     6 X3      X0=U(X'(2+K)*Y'(2+K))=A'(2+K)
          NX7       X7        7 X1      X7=G''(1+K)
          DX2       X2*X4     8 X5      X2=L(X'(2+K)*Y'(2+K))=A''(2+K)
 +        S_J       J+1       9 X7                          J=2-N+1+K
          FX6       X1+X7    10 X0      X6=U(G'(1+K)+G''(1+K))=S'(1+K)
          DX7       X1+X7    11         X7=L(G'(1+K)+G''(1+K))=S''(1+K)
 +        FX3       X3+X5    12 X2      X3=U(X''*Y'+X'*Y'')=B'(2+K)
          SA5       A5+JY    13 X6      X5=Y''(3+K)
          DX4       X6_&_X0  14 X7      X4=L(S'(1+K)&A'(2+K))=C''(2+K)
          FX0       X6_&_X0  15 X3      X0=U(S'(1+K)&A'(2+K))=C'(2+K)
 +        FX1       X2+X3    16         X1=U(A''(2+K)+B'(2+K))=D'(2+K)
          SA3       A3+JX    17 X4      X3=X''(3+K)
          FX7       X7+X4    18 X0      X7=U(S''(1+K)+C''(2+K))=E'(2+K)
          SA4       A4+JY    19 X1      X4=Y'(3+K)
 +        SA2       A2+JX    20 X5      X2=X'(3+K)
*                            21 X7
          FX7       X7_&_X1  22         X7=U(E'(2+K)&D'(2+K))=F'(2+K)
*                            23
*                            24 X3
          MI        J,[1]    25 X7      WHILE 3-N+K=J<0; I.E.4+K .LE. N
 DMPY1R   ENDM
 DMMPY1T  MACRO     N,&,Z,IZ,SAVEX,SAVEY,J
          FX6       X0+X7    26 X4      X6=U(C'(N-1)+F'(N-1))=G'(N-1)
          DX7       X0+X7    27 X2      X7=L(C'(N-1)+F'(N-1))=G''(N-1)
 +        FX3       X3*X4    28         X3=U(X''(N)*Y'(N))
*                            29 X6
          FX5       X2*X5    30 X7      X5=U(X'(N)*Y''(N))
          NX1       X6       31         X1=G'(N-1)
          FX0       X2*X4    32 X3      X0=U(X'(N)*Y'(N))=A'(N)
*                            33 X1
 +        NX7       X7                  X7=G''(N-1)
          DX2       X2*X4               X2=L(X'(N)*Y'(N))=A''(N)
          FX6       X1+X7               X6=U(G'(N-1)+G''(N-1))=S'(N-1)
          DX7       X1+X7               X7=L(G'(N-1)+G''(N-1))=S''(N-1)
          FX3       X3+X5               X3=U(X''(N)*Y'(N)+X'(N)*Y''(N))=B'(N)
          DX4       X6_&_X0             X4=L(S'(N-1)&A'(N))=C''(N)
          FX0       X6_&_X0             X0=U(S'(N-1)&A'(N))=C'(N)
          FX5       X2+X3               X5=U(A''(N)+B'(N))=D'(N)
          SA1       SAVEX
          FX7       X7+X4               X7=U(S''(N-1)+C''(N))=E'(N)
          SA2       SAVEY
          FX7       X7_&_X5             X7=U(E'(N)&D'(N))=F'(N)
          FX6       X0+X7               X6=U(C'(N)+F'(N))=G'(N)
          DX7       X0+X7               X7=L(C'(N)+F'(N))=G''(N)
          NX5       X6                  X5=G'(N)
          NX7       X7                  X7=G''(N)
          FX6       X5+X7               X6=U(G'(N)+G''(N))=S'(N)
          DX7       X5+X7               X7=L(G'(N)+G''(N))=S''(N)
          S_J       J-N                                     J=J-N
          SA6       Z                   Z'=S'(N)
          SA7       Z+1                 Z''=S''(N)
          S_Z       Z+IZ                (Z)=(Z)+IZ
 DMMPY1T  ENDM
 DVMPY    MACRO     &,N,X,JX,Y,JY,#,Z,J
          LOCAL     [1],[2]
          S_J       1                                       J=1
 LAMBDA   IFC       NE,*#*0*
          SA1       Z                   X1=Z'
          SA2       Z+J                 X2=Z''
 LAMBDA   ENDIF
          SA5       Y+J                 X5=Y''(1)
          SA3       X+J                 X3=X''(1)
 LAMBDA   IFC       EQ,*#*+*
          BX6       X1                  X6=#Z'
          BX7       X2                  X7=#Z''
 LAMBDA   ENDIF
 LAMBDA   IFC       EQ,*#*-*
          BX6       -X1
          BX7       -X2
 LAMBDA   ENDIF
          SA4       A5-J                X4=Y'(1)
          SA2       A3-J                X2=X'(1)
          S_J       2                                       J=2
          FX3       X3*X4               X3=U(X''(1)*Y'(1))
          S_J       J-N                                     J=2-N
          FX5       X2*X5               X5=U(X'(1)*Y''(1))
          FX0       X2*X4               X0=U(X'(1)*Y'(1)=A'(1)
          DX2       X2*X4               X2=L(X'(1)*Y'(1))=A''(1)
          FX3       X3+X5               X3=U(X''*Y'+X'*Y'')=B'(1)
 LAMBDA   IFC       NE,*#*0*
          DX4       X6_&_X0             X4=L(Z'&A'(1))=C''(1)
          FX0       X6_&_X0             X0=U(Z'&A'(1))=C'(1)
          FX1       X2+X3               X1=U(A''(1)+B'(1))=D'(1)
          FX7       X7+X4               X7=U(Z''+C''(1))=E'(1)
 LAMBDA   ELSE
          MX7       0
          FX1       X2+X3               X1=U(A''(1)+B'(1))=D'(1)
 LAMBDA   ENDIF
          FX7       X7_&_X1             X7=U(E'(1)&D'(1))=F'(1)
          GT        J,B0,[2]            IF N=1 GO TO [2]
          SA5       A5+JY               X5=Y''(2)
          SA3       A3+JX               X3=X''(2)
          SA4       A4+JY               X4=Y'(2)
          SA2       A2+JX               X2=X'(2)
          ZR        J,[1]               IF N=2 GO TO [1]
          DMPY1R    &,JX,JY,J
 [1]      FX6       X0+X7               X6=U(C'(N-1)+F'(N-1))=G'(N-1)
          DX7       X0+X7               X7=L(C'(N-1)+F'(N-1))=G''(N-1)
          FX3       X3*X4               X3=U(X''(N)*Y'(N))
          FX5       X2*X5               X5=U(X'(N)*Y''(N))
          NX1       X6                  X1=G'(N-1)
          FX0       X2*X4               X0=U(X'(N)*Y'(N))=A'(N)
          NX7       X7                  X7=G''(N-1)
          DX2       X2*X4               X2=L(X'(N)*Y'(N))=A''(N)
          FX6       X1+X7               X6=U(G'(N-1)+G''(N-1))=S'(N-1)
          DX7       X1+X7               X7=L(G'(N-1)+G''(N-1))=S''(N-1)
          FX3       X3+X5               X3=U(X''*Y'+X'*Y'')=B'(N)
          DX4       X6_&_X0             X4=L(S'(N-1)&A'(N))=C''(N)
          FX0       X6_&_X0             X0=U(S'(N-1)&A'(N))=C'(N)
          FX1       X2+X3               X1=U(A''(N)+B'(N))=D'(N)
          FX7       X7+X4               X7=U(S''(N-1)+C''(N))=E'(N)
          FX7       X7_&_X1             X7=U(E'(N)&D'(N))=F'(N)
 [2]      FX6       X0+X7               X6=U(C'(N)+F'(N))=G'(N)
          DX7       X0+X7               X7=L(C'(N)+F'(N))=G''(N)
          NX1       X6                  X1=G'(N)
          NX7       X7                  X7=G''(N)
          FX6       X1+X7               X6=U(G'(N)+G''(N))=S'(N)
          DX7       X1+X7               X7=L(G'(N)+G''(N))=S''(N)
 DVMPY    ENDM
 DMMPY1H  MACRO     &,IX,JX,JY,#,Z,SAVEX,I
 LAMBDA   IFC       EQ,*#*0*            # IS 0
 +        SA4       X2        0         X4=Y'(1)    A4=(Y'(1))
          SA3       X1+1      1         X3=X''(1,1) A3=(X''(1,1))
          SA2       X1        2         X2=X'(1,1)  A2=(X'(1,1))
 +        SA5       A4+1      3         X5=Y''(1)   A5=(Y''(1))
          S_I       I+1       4                             I=I+1
*                             5
*                             6
*                             7 X4
*                             8 X3
 +        FX0       X3*X4     9 X2      X0=X''(1,1)*Y'(1)
          SX7       A2+IX    10 X5      X7=(X'(2,1))
          FX5       X2*X5    11 X7      X5=X'(1,1)*Y''(1)
          SA3       A3+JX    12         X3=X''(1,2) A3=(X''(1,2))
 +        DX6       X2*X4    13 X0      X6=L(X'(1,1)*Y'(1))=A''(1)
          SA7       SAVEX    14         SAVEX=(X'(2,1))
*                            15 X5
*                            16
          FX1       X0+X5    17 X6      X1=U(X''*Y'+X'*Y'')=B'(1)
 +        FX0       X2*X4    18         X0=U(X'(1,1)*Y'(1))=A'(1)
          SA5       A5+JY    19 X3      X5=Y''(2)   A5=(Y''(2))
*                            20 X1
          FX1       X6+X1    21         X1=U(A''(1)+B'(1))=D'(1)
          SA4       A4+JY    22 X0      X4=Y'(2)    A4=(Y'(2))
          SA2       A2+JX    23         X2=X'(1,2)  A2=(X'(1,2))
 SIGMA    IFC       EQ,*&*+*
          BX7       X1
 SIGMA    ELSE
          BX0       -X0                 X0=&A'(1)=C'(1)
          BX7       -X1                 X7=&D'(1)=F'(1)
 SIGMA    ENDIF
 LAMBDA   ELSE                          # IS + OR -
          SA5       Z+1                 X5=Z''(1)
          SA3       X1+1                X3=X''(1,1) A3=(X''(1,1))
          SX6       X1+IX               X6=(X'(2,1))
          SA4       X2                  X4=Y'(1)  A4=(Y'(1))
          SA2       X1                  X2=X'(1,1) A2=(X'(1,1))
          SA1       Z                   X1=Z'(1)
 LAMBDA2  IFC       EQ,*#*+*
          BX7       X5
 LAMBDA2  ELSE
          BX7       -X5                 X7=#Z''(1)
 LAMBDA2  ENDIF
          SA5       A4+1                X5=Y''(1) A5=(Y''(1))
          FX3       X3*X4               X3=U(X''*Y')
          SA6       SAVEX               SAVEX=(X'(2,1))
 LAMBDA2  IFC       EQ,*#*+*
          BX6       X1
 LAMBDA2  ELSE
          BX6       -X1                 X6=#Z'(1)
 LAMBDA2  ENDIF
          FX0       X2*X4               X0=U(X'*Y')=A'(1)
          FX5       X2*X5               X5=U(X'*Y'')
          S_I       I+1                                     I=I+1
          DX2       X2*X4               X2=L(X'*Y')=A''(1)
          DX4       X6_&_X0             X4=L(#Z' & A')=C''(1)
          FX3       X3+X5               X3=U(X''*Y'+X'*Y'')=B'(1)
          SA5       A5+JY               X5=Y''(2) A5=(Y''(2))
          FX0       X6_&_X0             X0=U(#Z' & A')=C'(1)
          FX1       X2+X3               X1=U(A''+B')=D'(1)
          SA3       A3+JX               X3=X''(1,2) A3=(X''(1,2))
          FX7       X7+X4               X7=U(#Z''+C'')=E'(1)
          SA4       A4+JY               X4=Y'(2)  A4=(Y'(2))
          SA2       A2+JX               X2=X'(1,2) A2=(X'(1,2))
          FX7       X7_&_X1             X7=U(E' & D')=F'(1)
 LAMBDA   ENDIF
 DMMPY1H  ENDM
 DMMPY    MACRO     &,I,N,IX,JX,JY,#,Z,IZ,TEMP,J   X1=(X), X2=(Y)
          LOCAL     [1],[2],[3],[99],SAVEX,SAVEY
 SAVEX    EQU       TEMP
 SAVEY    EQU       TEMP+1
          BX7       X2                  X7=(Y)
          SA7       SAVEY               SAVEY=(Y)
          BRANCH    [3],N,GT,2,J,I,I    J=2-N, I=-M
          NZ        J,[2]               IF N=1 GO TO [2]
 [1]      DMMPY1H   &,IX,JX,JY,#,Z,SAVEX,I  I=I+1
          DMMPY1T   N,&,Z,IZ,SAVEX,SAVEY,J
          MI        I,[1]               WHILE I<0
          JP        [99]
 [2]      BSS       0
 LAMBDA   IFC       EQ,*#*0*            # IS 0
          S_N       X1                  N=(X)
          SA1       X2                  X1=Y'
          SA2       X2+1                X2=Y''
          S_I       -I                                      I=M
 SIGMA    IFC       EQ,*&*-*
          BX1       -X1                 X1=&Y'
          BX2       -X2                 X2=&Y''
 SIGMA    ENDIF
          DVSCL     I,N,IX,Z,IZ,J
          S_N       1                   RESTORE N
 LAMBDA   ELSE
          SA3       X2                  X3=Y'
          BX4       X1                  X4=(X)
          SA1       X2+1                X1=Y''
          SX2       Z                   X2=(Z)
          S_I       -I                                      I=M
 SIGMA    IFC       EQ,*&*-*
          BX0       -X3                 X0=&Y'
          BX1       -X1                 X1=&Y''
 SIGMA    ELSE
          BX0       X3
 SIGMA    ENDIF
          DVSCA     I,IX,#,IZ,Z,IZ,J
 LAMBDA   ENDIF
          JP        [99]
 [3]      BSS       0                   N=3,4,5,...; J=2-N=-1,-2,-3,...
          DMMPY1H   &,IX,JX,JY,#,Z,SAVEX,I  I=I+1
          DMPY1R    &,JX,JY,J           J=0
          S_J       2
          DMMPY1T   N,&,Z,IZ,SAVEX,SAVEY,J  J=2-N=-1,-2,-3,...
          MI        I,[3]               WHILE I<0
 [99]     BSS       0
 DMMPY    ENDM
 DUMPY1H  MACRO     N,&,IX,JX,JY,#,Z,SAVEX,SAVEY
 LAMBDA   IFC       EQ,*#*0*            # IS 0
 +        SA4       X2        0         X4=Y'(1)    A4=(Y'(1))
          SA3       X1+1      1         X3=X''(1,1) A3=(X''(1,1))
          SX7       X1+JX     2         X7=(X'(1,2))
 +        SA2       X1        3 X7      X2=X'(1,1)  A2=(X'(1,1))
          SA5       A4+1      4         X5=Y''(1)   A5=(Y''(1))
          SX6       A4+JY     5         X6=(Y'(2))
*                             6 X6
*                             7 X4
 +        SX7       X7+IX     8 X3      X7=(X'(2,2))
          FX0       X3*X4     9 X7      X0=X''(1,1)*Y'(1)
          SA6       SAVEY    10 X2      SAVEY=(Y'(2))
 +        SA7       SAVEX    11 X5      SAVEX=(X'(2,2))
          FX5       X2*X5    12         X5=X'(1,1)*Y''(1)
          SA3       A3+JX    13 X0      X3=X''(1,2) A3=(X''(1,2))
 +        DX6       X2*X4    14         X6=L(X'(1,1)*Y'(1))=A''(1)
*                            15
*                            16 X5
*                            17
          FX1       X0+X5    18 X6      X1=U(X''*Y'+X'*Y'')=B'(1)
          FX0       X2*X4    19         X0=U(X'(1,1)*Y'(1))=A'(1)
          SA4       A4+JY    20 X3      X4=Y'(2)    A4=(Y'(2))
 +        SA2       A2+JX    21 X1      X2=X'(1,2)  A2=(X'(1,2))
          FX1       X6+X1    22         X1=U(A''(1)+B'(1))=D'(1)
          SA5       A5+JY    23 X0      X5=Y''(2)   A5=(Y''(2))
*                            24
 SIGMA    IFC       EQ,*&*+* 25 X1      X0=&A'(1)=C'(1)
          NO
 SIGMA    ELSE
          BX0       -X0
 SIGMA    ENDIF
 +        S_N       N-1      26 X0                          N=N-1
          NO                 27 X4
 SIGMA    IFC       EQ,*&*+* 28 X2      X7=&D'(1)=F'(1)
          BX7       X1
 SIGMA    ELSE
          BX7       -X1
 SIGMA    ENDIF
 LAMBDA   ELSE                          # IS + OR -
          SA5       Z                   X5=Z'(1)  A5=(Z'(1))
          SA3       X1+1                X3=X''(1,1) A3=(X''(1,1))
          SX7       X2+JY               X7=(Y'(2))
          SA4       X2                  X4=Y'(1)  A4=(Y'(1))
          SA7       SAVEY               SAVEY=(Y'(2))
          SA2       X1                  X2=X'(1,1) A2=(X'(1,1))
 LAMBDA2  IFC       EQ,*#*+*
          BX6       X5
 LAMBDA2  ELSE
          BX6       -X5                 X6=#Z'(1)
 LAMBDA2  ENDIF
          SA5       A4+1                X5=Y''(1) A5=(Y''(1))
          SX7       X1+JX               X7=(X'(1,2))
          SA1       Z+1                 X1=Z''(1)
          SX7       X7+IX               X7=(X'(2,2))
          FX3       X3*X4               X3=U(X''*Y')
          SA7       SAVEX               SAVEX=(X'(2,2))
          FX0       X2*X4               X0=U(X'*Y')=A'(1)
          FX5       X2*X5               X5=U(X'*Y'')
          S_N       N-1                                     N=N-1
          DX2       X2*X4               X2=L(X'*Y')=A''(1)
 LAMBDA2  IFC       EQ,*#*+*
          BX7       X1
 LAMBDA2  ELSE
          BX7       -X1                 X7=#Z''(1)
 LAMBDA2  ENDIF
          FX3       X3+X5               X3=U(X''*Y'+X'*Y'')=B'(1)
          DX4       X6_&_X0             X4=L(#Z' & A')=C''(1)
          FX0       X6_&_X0             X0=U(#Z' & A')=C'(1)
          SA5       A5+JY               X5=Y''(2) A5=(Y''(2))
          FX1       X2+X3               X1=U(A''+B')=D'(1)
          SA3       A3+JX               X3=X''(1,2) A3=(X''(1,2))
          FX7       X7+X4               X7=U(#Z''+C'')=E'(1)
          SA4       A4+JY               X4=Y'(2)  A4=(Y'(2))
          SA2       A2+JX               X2=X'(1,2) A2=(X'(1,2))
          FX7       X7_&_X1             X7=U(E' & D')=F'(1)
 LAMBDA   ENDIF
 DUMPY1H  ENDM
 DUMPY    MACRO     &,N,IX,JX,JY,#,Z,IZ,TEMP,J   X1=(X), X2=(Y)
          LOCAL     [1],[2],[3],SAVEX,SAVEY
 SAVEX    EQU       TEMP
 SAVEY    EQU       TEMP+1
          BRANCH    [2],N,LE,2,J        J=2-N; IF(N.LE.2) GO TO [2]
 [1]      BSS       0                   N=3,4,5,...;  J=-1,-2,-3,...
          DUMPY1H   N,&,IX,JX,JY,#,Z,SAVEX,SAVEY  N=N-1=2,3,4,...
          DMPY1R    &,JX,JY,J           J=0
          S_J       2
          DMMPY1T   N,&,Z,IZ,SAVEX,SAVEY,J  J=0,-1,-2,...
          MI        J,[1]               IF N=3,4,5,...
 [2]      BSS       0                   N=1,2         J=1,0
          NE        J,B0,[3]            IF(N=1) GO TO [3]
          DUMPY1H   N,&,IX,JX,JY,#,Z,SAVEX,SAVEY  N=N-1=1
          DMMPY1T   N,&,Z,IZ,SAVEX,SAVEY,J
 [3]      BSS       0                   N=1
 LAMBDA   IFC       EQ,*#*0*            # IS 0
          SA4       X2                  X4=Y'
          SA3       X1+1                X3=X''
          SA2       X1                  X2=X'
          SA5       A4+1                X5=Y''
          FX0       X3*X4               X0=X''Y'
          FX5       X2*X5               X5=X'Y''
          DX6       X2*X4               X6=L(X'*Y')=A''
          FX1       X0+X5               X1=U(X''*Y'+X'*Y'')=B'
          FX0       X2*X4               X0=U(X'*Y')=A'
          FX7       X6+X1               X7=U(A''+B')=D'
 SIGMA    IFC       EQ,*&*-*
          BX0       -X0                 X0=&A'=C'
          BX7       -X7                 X7=&D'=F'
 SIGMA    ENDIF
          FX6       X0+X7               X6=U(C'+F')=Z'
          DX7       X0+X7               X7=L(C'+F')=Z''
          SA6       Z
          SA7       Z+1
 LAMBDA   ELSE
          SA5       Z                   X5=Z'(1)  A5=(Z'(1))
          SA3       X1+1                X3=X''(1,1) A3=(X''(1,1))
          NO                            DELAY
          SA4       X2                  X4=Y'(1)  A4=(Y'(1))
          SA2       X1                  X2=X'(1,1) A2=(X'(1,1))
          NO
 LAMBDA2  IFC       EQ,*#*+*
          BX6       X5
 LAMBDA2  ELSE
          BX6       -X5                 X6=#Z'(1)
 LAMBDA2  ENDIF
          SA5       A4+1                X5=Y''(1) A5=(Y''(1))
          SA1       Z+1                 X1=Z''(1)
          FX3       X3*X4               X3=U(X''*Y')
          FX0       X2*X4               X0=U(X'*Y')=A'(1)
          FX5       X2*X5               X5=U(X'*Y'')
 LAMBDA2  IFC       EQ,*#*+*
          BX7       X1
 LAMBDA2  ELSE
          BX7       -X1                 X7=#Z''(1)
 LAMBDA2  ENDIF
          DX2       X2*X4               X2=L(X'*Y')=A''(1)
          FX3       X3+X5               X3=U(X''*Y'+X'*Y'')=B'(1)
          DX4       X6_&_X0             X4=L(#Z' & A')=C''(1)
          FX0       X6_&_X0             X0=U(#Z' & A')=C'(1)
          FX1       X2+X3               X1=U(A''+B')=D'(1)
          FX7       X7+X4               X7=U(#Z''+C'')=E'(1)
          FX7       X7_&_X1             X7=U(E' & D')=F'(1)
          FX6       X0+X7               X6=U(C'+F')=G'(1)
          DX7       X0+X7               X7=L(C'+F')=G''(1)
          NX1       X6                  X1=G'
          NX7       X7
          FX6       X1+X7               X6=U(G'+G'')=Z'(1)
          DX7       X1+X7               X7=L(G'+G'')=Z''(1)
          SA6       Z                   Z'(1)=...
          SA7       Z+1                 Z''(1)=...
 LAMBDA   ENDIF
 DUMPY    ENDM
 CMPY1R   MACRO     &,JX,JY,J
          LOCAL     [1]
 [1]      BSS       0                   FOR K = 0, 1, 2, ...
          RX7       X7_&_X5   0 X3      X7=Z''(1+K)
          RX6       X6_&_X0   1 X4      X6=Z'(1+K)
          RX0       X1*X3     2         X0=X'(2+K)*Y'(2+K)
          S_J       J+A5      3 X7                          J=2-N+1+K
          RX1       X1*X4     4 X6      X1=X'(2+K)*Y''(2+K)
          NX7       X7        5
          RX4       X2*X4     6 X0      X4=X''(2+K)*Y''(2+K)
          NX6       X6        7 X7
          RX5       X2*X3     8 X1      X5=X''(2+K)*Y'(2+K)
          SA2       A2+JX     9 X6      X2=X''(3+K)
          RX7       X7_&_X1  10 X4      X7=Z''(1+K) &X'(2+K)*Y''(2+K)
 SIGMA    IFC       EQ,*&*+* 11         X6=Z'(1+K)-&X''(2+K)*Y''(2+K)
          RX6       X6-X4
 SIGMA    ELSE
          RX6       X6+X4
 SIGMA    ENDIF
          SA1       A1+JX    12 X5      X1=X'(3+K)
          SA3       A3+JY    13 X7      X3=Y'(3+K)
          SA4       A4+JY    14 X6      X4=Y''(3+K)
          NX7       X7       15
          NX6       X6       16 X2
*                            17 X7
*                            18 X6
          MI        J,[1]    19 X1      WHILE 3-N+K<0; I.E. 4+K.LE.N
 CMPY1R   ENDM
 CCMPY1R  MACRO     &,JX,JY,J
          LOCAL     [1]
 [1]      BSS       0                   FOR K = 0, 1, 2, ...
          RX7       X7_&_X5   0 X3      X7=Z''(1+K)
          RX6       X6_&_X0   1 X4      X6=Z'(1+K)
          RX0       X1*X3     2         X0=X'(2+K)*Y'(2+K)
          S_J       J+A5      3 X7                          J=2-N+1+K
          RX1       X1*X4     4 X6      X1=X'(2+K)*Y''(2+K)
          NX7       X7        5
          RX4       X2*X4     6 X0      X4=X''(2+K)*Y''(2+K)
          NX6       X6        7 X7
          RX5       X2*X3     8 X1      X5=X''(2+K)*Y'(2+K)
          SA2       A2+JX     9 X6      X2=X''(3+K)
 SIGMA    IFC       EQ,*&*+* 10 X7      X7=Z''(1+K)-&X'(2+K)*Y''(2+K)
          RX7       X7-X1
 SIGMA    ELSE
          RX7       X7+X1
 SIGMA    ENDIF
          RX6       X6_&_X4  11         X6=Z'(1+K) &X''(2+K)*Y''(2+K)
          SA1       A1+JX    12 X5      X1=X'(3+K)
          SA3       A3+JY    13 X7      X3=Y'(3+K)
          SA4       A4+JY    14 X6      X4=Y''(3+K)
          NX7       X7       15
          NX6       X6       16 X2
*                            17 X7
*                            18 X6
          MI        J,[1]    19 X1      WHILE 3-N+K<0; I.E. 4+K.LE.N
 CCMPY1R  ENDM
 CMMPY1T  MACRO     N,&,Z,IZ,SAVEX,SAVEY,J
          RX7       X7_&_X5             X7=Z''(N-1)
          RX6       X6_&_X0             X6=Z'(N-1)
          RX0       X1*X3               X0=X'(N)*Y''(N)
          S_J       J-N                                     J=J-N
          RX1       X1*X4               X1=X'(N)*Y''(N)
          NX7       X7
          RX4       X2*X4               X4=X''(N)*Y'(N)
          NX6       X6
          RX5       X2*X3               X5=X''(N)*Y'(N)
          RX7       X7_&_X1             X7=Z''(N-1) &X'(N)*Y''(N)
 SIGMA    IFC       EQ,*&*+*            X6=Z'(N-1)-&X''(N)*Y''(N)
          RX6       X6-X4
 SIGMA    ELSE
          RX6       X6+X4
 SIGMA    ENDIF
          SA1       SAVEX
          NX7       X7
          NX6       X6
          SA2       SAVEY
          RX7       X7_&_X5             X7=Z''(N)
          RX6       X6_&_X0             X6=Z'(N)
          NX7       X7
          NX6       X6
          SA7       Z+1                 Z''
          SA6       Z                   Z'
          S_Z       Z+IZ                Z=Z+IZ
 CMMPY1T  ENDM
 CVMPY    MACRO     &,N,X,JX,Y,JY,#,Z,J
          LOCAL     [1],[2]
          SA3       Y                   X3=Y'(1)
          SA1       X                   X1=X'(1)
 LAMBDA   IFC       EQ,*#*0*
          SA4       A3+1                X4=Y''(1)
          SA2       A1+1                X2=X''(1)
          SA5       1                   X5=VOID  A5=1 (SEE MACRO CMPY1R)
          S_J       2                                       J=2
          RX0       X1*X3               X0=X'(1)*Y'(1)
          S_J       J-N                                     J=2-N
          RX7       X1*X4               X7=X'(1)*Y''(1)
          RX6       X2*X4               X6=X''(1)*Y''(1)
          RX5       X2*X3               X5=X''(1)*Y'(1)
 SIGMA    IFC       EQ,*&*+*
          BX6       -X6
 SIGMA    ELSE
          BX7       -X7
 SIGMA    ENDIF
 LAMBDA   ELSE
          SA2       Z+1                 X2=Z''    A2=(Z'')
          SA4       Z                   X4=Z'     A4=(Z')
          SA5       1                   X5=VOID  A5=1 (SEE MACRO CMPY1R)
          S_J       2
          S_J       J-N                                     J=2-N
 LAMBDA2  IFC       EQ,*#*+*
          BX7       X2
 LAMBDA2  ENDIF
 LAMBDA2  IFC       EQ,*#*-*
          BX7       -X2                 X7=#Z''
 LAMBDA2  ENDIF
          SA2       A1+1                X2=X''(1) A2=(X''(1))
 LAMBDA2  IFC       EQ,*#*+*
          BX6       X4
 LAMBDA2  ENDIF
 LAMBDA2  IFC       EQ,*#*-*
          BX6       -X4                 X6=#Z'
 LAMBDA2  ENDIF
          SA4       A3+1                X4=Y''(1) A4=(Y''(1))
          RX0       X1*X3               X0=X'(1)*Y'(1)
          RX1       X1*X4               X1=X'(1)*Y''(1)
          RX4       X2*X4               X4=X''(1)*Y''(1)
          RX5       X2*X3               X5=X''(1)*Y'(1)
          RX7       X7_&_X1             X7=Z''&X'(1)*Y''(1)
 SIGMA    IFC       EQ,*&*+*            X6=Z'-&X''(1)*Y''(1)
          RX6       X6-X4
 SIGMA    ELSE
          RX6       X6+X4
 SIGMA    ENDIF
          NX7       X7
          NX6       X6
 LAMBDA   ENDIF
          GT        J,B0,[2]            IF N=1 GO TO [2]
          SA1       A1+JX               X1=X'(2)
          SA3       A3+JY               X3=Y'(2)
          SA4       A4+JY               X4=Y''(2)
          SA2       A2+JX               X2=X''(2)
          ZR        J,[1]               IF N=2 GO TO [1]
          CMPY1R    &,JX,JY,J
 [1]      RX7       X7_&_X5             X7=Z''(N-1)
          RX6       X6_&_X0             X6=Z'(N-1)
          RX0       X1*X3               X0=X'(N)*Y'(N)
          RX1       X1*X4               X1=X'(N)*Y''(N)
          NX7       X7
          RX4       X2*X4               X4=X''(N)*Y''(N)
          NX6       X6
          RX5       X2*X3               X5=X''(N)*Y'(N)
          RX7       X7_&_X1             X7=Z''(N-1)&X'(N)*Y''(N)
 SIGMA    IFC       EQ,*&*+*            X6=Z'(N-1)-&X''(N)*Y''(N)
          RX6       X6-X4
 SIGMA    ELSE
          RX6       X6+X4
 SIGMA    ENDIF
          NX7       X7
          NX6       X6
 [2]      RX7       X7_&_X5             X7=Z''(N)
          RX6       X6_&_X0             X6=Z'(N)
          NX7       X7
          NX6       X6
 CVMPY    ENDM
 CMMPY1H  MACRO     &,IX,JX,JY,#,Z,SAVEX,I
 LAMBDA   IFC       EQ,*#*0*            # IS 0
          SA1       X1                  X1=X'(1,1) A1=(X'(1,1))
          SA3       X2                  X3=Y'(1)   A3=(Y'(1))
          SA4       X2+1                X4=Y''(1)  A4=(Y''(1))
          SA2       A1+1                X2=X''(1,1)A2=(X''(1,1))
          SX7       A1+IX               X7=(X'(2,1))
          S_I       I+A5                                    I=I+1
          SA7       SAVEX               SAVEX=(X'(2,1))
          RX0       X1*X3               X0=X'(1,1)*Y'(1)
          RX7       X1*X4               X7=X'(1,1)*Y''(1)
          SA1       A1+JX               X1=X'(1,2) A1=(X'(1,2))
          RX6       X2*X4               X6=X''(1,1)*Y''(1)
          SA4       A4+JY               X4=Y''(2)  A4=(Y''(2))
          RX5       X2*X3               X5=X''(1,1)*Y'(1)
          SA3       A3+JY               X3=Y'(2)   A3=(Y'(2))
          SA2       A2+JX               X2=X''(1,2)A2=(X''(1,2))
 SIGMA    IFC       EQ,*&*+*
          BX6       -X6
 SIGMA    ELSE
          BX7       -X7
 SIGMA    ENDIF
 LAMBDA   ELSE                          # IS + OR -
          SA1       X1                  X1=X'(1,1) A1=(X'(1,1))
          SA3       X2                  X3=Y'(1)  A3=(Y'(1))
          SA2       Z+1                 X2=Z''(1) A2=(Z''(1))
          SA4       Z                   X4=Z'(1)  A4=(Z'(1))
          SX7       A1+IX               X7=(X'(2,1))
          SA7       SAVEX               SAVEX=(X'(2,1))
 LAMBDA2  IFC       EQ,*#*+*
          BX7       X2
 LAMBDA2  ELSE
          BX7       -X2                 X7=#Z''(1)
 LAMBDA2  ENDIF
          SA2       A1+1                X2=X''(1,1) A2=(X''(1,1))
 LAMBDA2  IFC       EQ,*#*+*
          BX6       X4
 LAMBDA2  ELSE
          BX6       -X4                 X6=#Z'(1)
 LAMBDA2  ENDIF
          SA4       A3+1                X4=Y''(1) A4=(Y''(1))
          RX0       X1*X3               X0=X'*Y'
          S_I       I+A5                                    I=I+1
          RX5       X2*X3               X5=X''*Y'
          RX1       X1*X4               X1=X'*Y''
          RX4       X2*X4               X4=X''*Y''
          SA2       A2+JX               X2=X''(1,2) A2=(X''(1,2))
          RX7       X7_&_X1             X7=#Z''(1) & X'*Y''
 SIGMA    IFC       EQ,*&*+*
          RX6       X6-X4
 SIGMA    ELSE
          RX6       X6+X4               X6=#Z'(1) -& X''*Y''
 SIGMA    ENDIF
          SA1       A1+JX               X1=X'(1,2) A1=(X'(1,2))
          SA3       A3+JY               X3=Y'(2)  A3=(Y'(2))
          SA4       A4+JY               X4=Y''(2) A4=(Y''(2))
          NX7       X7
          NX6       X6
 LAMBDA   ENDIF
 CMMPY1H  ENDM
 CMMPY    MACRO     &,I,N,IX,JX,JY,#,Z,IZ,TEMP,J   X1=(X), X2=(Y)
          LOCAL     [1],[2],[3],[99],SAVEX,SAVEY
 SAVEX    EQU       TEMP
 SAVEY    EQU       TEMP+1
          BX7       X2                  X7=(Y)
          SA5       1                   X5=VOID   A5=1
          SA7       SAVEY               SAVEY=(Y)
          BRANCH    [3],N,GT,2,J,I,I    J=2-N, I=-M
          NZ        J,[2]               IF N=1 GO TO [2]
 [1]      CMMPY1H   &,IX,JX,JY,#,Z,SAVEX,I  I=I+1
          CMMPY1T   N,&,Z,IZ,SAVEX,SAVEY,J
          MI        I,[1]               WHILE I<0
          JP        [99]
 [2]      BSS       0
 LAMBDA   IFC       EQ,*#*0*            # IS 0
          S_N       X1                  N=(X)
          SA1       X2                  X1=Y'
          SA2       X2+1                X2=Y''
          S_I       -I                                      I=M
 SIGMA    IFC       EQ,*&*-*
          BX1       -X1                 X1=&Y'
          BX2       -X2                 X2=&Y''
 SIGMA    ENDIF
          CVSCL     I,N,IX,Z,IZ,J
          S_N       1                   RESTORE N
 LAMBDA   ELSE                          # IS + OR -
          SA3       X2                  X3=Y'
          BX4       X1                  X4=(X)
          SA1       X2+1                X1=Y''
          SX2       Z                   X2=(Z)
          S_I       -I                                      I=M
 SIGMA    IFC       EQ,*&*-*
          BX0       -X3                 X0=&Y'
          BX1       -X1                 X1=&Y''
 SIGMA    ELSE
          BX0       X3
 SIGMA    ENDIF
          CVSCA     I,IX,#,IZ,Z,IZ,J
 LAMBDA   ENDIF
          JP        [99]
 [3]      BSS       0                   N=3,4,5,...; J=2-N=-1,-2,-3,...
          CMMPY1H   &,IX,JX,JY,#,Z,SAVEX,I  I=I+1
          CMPY1R    &,JX,JY,J           J=0
          S_J       2
          CMMPY1T   N,&,Z,IZ,SAVEX,SAVEY,J  J=2-N=-1,-2,-3,...
          MI        I,[3]               WHILE I<0
 [99]     BSS       0
 CMMPY    ENDM
 CUMPY1H  MACRO     N,&,IX,JX,JY,#,Z,SAVEX,SAVEY
 LAMBDA   IFC       EQ,*#*0*            # IS 0
          SA1       X1                  X1=X'(1,1) A1=(X'(1,1))
          SA3       X2                  X3=Y'(1)   A3=(Y'(1))
          SA4       X2+1                X4=Y''(1)  A4=(Y''(1))
          SA2       A1+1                X2=X''(1,1)A2=(X''(1,1))
          SX7       A1+IX               X7=(X'(2,1))
          SX6       A3+JY               X6=(Y'(2))
          SX7       X7+JX               X7=(X'(2,2))
          SA6       SAVEY               SAVEY=(Y'(2))
          RX0       X1*X3               X0=X'(1,1)*Y'(1)
          SA7       SAVEX               SAVEX=(X'(2,2))
          RX7       X1*X4               X7=X'(1,1)*Y''(1)
          SA1       A1+JX               X1=X'(1,2) A1=(X'(1,2))
          RX6       X2*X4               X6=X''(1,1)*Y''(1)
          SA4       A4+JY               X4=Y''(2)  A4=(Y''(2))
          RX5       X2*X3               X5=X''(1,1)*Y'(1)
          SA3       A3+JY               X3=Y'(2)   A3=(Y'(2))
          SA2       A2+JX               X2=X''(1,2)A2=(X''(1,2))
 SIGMA    IFC       EQ,*&*+*
          BX6       -X6
 SIGMA    ELSE
          BX7       -X7
 SIGMA    ENDIF
          S_N       N-1                                     N=N-1
 LAMBDA   ELSE                          # IS + OR -
          SX6       X1+JX               X6=(X'(1,2))
          SX7       X2+JY               X7=(Y'(2))
          SA1       X1                  X1=X'(1,1) A1=(X'(1,1))
          SX6       X6+IX               X6=(X'(2,2))
          SA3       X2                  X3=Y'(1)  A3=(Y'(1))
          SA4       Z                   X4=Z'(1)  A4=(Z'(1))
          SA2       Z+1                 X2=Z''(1) A2=(Z''(1))
          SA6       SAVEX               SAVEX=(X'(2,2))
          SA7       SAVEY               SAVEY=(Y'(2))
 LAMBDA2  IFC       EQ,*#*+*
          BX6       X4
 LAMBDA2  ELSE
          BX6       -X4                 X6=#Z'(1)
 LAMBDA2  ENDIF
          SA4       A3+1                X4=Y''(1) A4=(Y''(1))
 LAMBDA2  IFC       EQ,*#*+*
          BX7       X2
 LAMBDA2  ELSE
          BX7       -X2                 X7=#Z''(1)
 LAMBDA2  ENDIF
          SA2       A1+1                X2=X''(1,1) A2=(X''(1,1))
          RX0       X1*X3               X0=X'(1,1)*Y'(1)
          RX1       X1*X4               X1=X'(1,1)*Y''(1)
          S_N       N-1                                     N=N-1
          RX4       X2*X4               X4=X''(1,1)*Y''(1)
          RX5       X2*X3               X5=X''(1,1)*Y'(1)
          SA2       A2+JX               X2=X''(1,2) A2=(X''(1,2))
          RX7       X7_&_X1             X7=#Z''(1)-&X'(1,1)*Y'(1)
 SIGMA    IFC       EQ,*&*+*
          RX6       X6-X4
 SIGMA    ELSE
          RX6       X6+X4               X6=#Z'(1)-&X''(1,1)*Y''(1)
 SIGMA    ENDIF
          SA1       A1+JX               X1=X'(1,2) A1=(X'(1,2))
          SA3       A3+JY               X3=Y'(2)  A3=(Y'(2))
          SA4       A4+JY               X4=Y''(2) A4=(Y''(2))
          NX7       X7
          NX6       X6
 LAMBDA   ENDIF
 CUMPY1H  ENDM
 CUMPY    MACRO     &,N,IX,JX,JY,#,Z,IZ,TEMP,J   X1=(X), X2=(Y)
          LOCAL     [1],[2],[3],SAVEX,SAVEY
 SAVEX    EQU       TEMP
 SAVEY    EQU       TEMP+1
          SA5       1                   X5=VOID   A5=1
          BRANCH    [2],N,LE,2,J        J=2-N; IF(N.LE.2) GO TO [2]
 [1]      BSS       0                   N=3,4,5,...;  J=-1,-2,-3,...
          CUMPY1H   N,&,IX,JX,JY,#,Z,SAVEX,SAVEY  N=N-1=2,3,4,...
          CMPY1R    &,JX,JY,J           J=0
          S_J       2
          CMMPY1T   N,&,Z,IZ,SAVEX,SAVEY,J  J=0,-1,-2,...
          MI        J,[1]               IF N=3,4,5,...
 [2]      BSS       0                   N=1,2         J=1,0
          NE        J,B0,[3]            IF(N=1) GO TO [3]
          CUMPY1H   N,&,IX,JX,JY,#,Z,SAVEX,SAVEY  N=N-1=1
          CMMPY1T   N,&,Z,IZ,SAVEX,SAVEY,J
 [3]      BSS       0                   N=1
 LAMBDA   IFC       EQ,*#*0*            # IS 0
          SA1       X1                  X1=X'
          SA3       X2                  X3=Y'
          SA4       X2+1                X4=Y''
          SA2       A1+1                X2=X''
          RX0       X1*X3               X0=X'*Y'
          RX7       X1*X4               X7=X'*Y''
          RX6       X2*X4               X6=X''*Y''
          RX5       X2*X3               X5=X''*Y'
 SIGMA    IFC       EQ,*&*+*
          RX6       X0-X6
          RX7       X7+X5
 SIGMA    ELSE
          RX6       X6-X0
          RX7       X7+X5
          BX7       -X7
 SIGMA    ENDIF
          NX6       X6
          NX7       X7
          SA6       Z
          SA7       Z+1
 LAMBDA   ELSE
          SA1       X1                  X1=X'(1,1) A1=(X'(1,1))
          SA3       X2                  X3=Y'(1)  A3=(Y'(1))
          SA4       Z                   X4=Z'(1)  A4=(Z'(1))
          SA2       Z+1                 X2=Z''(1) A2=(Z''(1))
 LAMBDA2  IFC       EQ,*#*+*
          BX6       X4
 LAMBDA2  ELSE
          BX6       -X4                 X6=#Z'(1)
 LAMBDA2  ENDIF
          SA4       A3+1                X4=Y''(1) A4=(Y''(1))
 LAMBDA2  IFC       EQ,*#*+*
          BX7       X2
 LAMBDA2  ELSE
          BX7       -X2                 X7=#Z''(1)
 LAMBDA2  ENDIF
          SA2       A1+1                X2=X''(1,1) A2=(X''(1,1))
          RX0       X1*X3               X0=X'(1,1)*Y'(1)
          RX1       X1*X4               X1=X'(1,1)*Y''(1)
          RX4       X2*X4               X4=X''(1,1)*Y''(1)
          RX5       X2*X3               X5=X''(1,1)*Y'(1)
          RX7       X7_&_X1             X7=#Z''(1)-&X'(1,1)*Y'(1)
 SIGMA    IFC       EQ,*&*+*
          RX6       X6-X4
 SIGMA    ELSE
          RX6       X6+X4               X6=#Z'(1)-&X''(1,1)*Y''(1)
 SIGMA    ENDIF
          NX7       X7
          NX6       X6
          RX7       X7_&_X5             X7=Z''(1)
          RX6       X6_&_X0             X6=Z'(1)
          NX7       X7
          NX6       X6
          SA7       Z+1                 Z''(1)=...
          SA6       Z                   Z'(1)=...
 LAMBDA   ENDIF
 CUMPY    ENDM
*CD       MUL
 RMUL1R   MACRO     JX,JY,JZ,J
          LOCAL     [1]
 [1]      BSS       0                   FOR K = 0, 1, 2, ...
 +        SA7       X0        0 X5      Z(1+K)=X(1+K)*Y(1+K)
          RX7       X3*X5     1         X7=X(2+K)*Y(2+K)
          SA3       A3+JX     2         X3=X(3+K) A3=(X(3+K))
          SA5       A5+JY     3         X5=Y(3+K) A5=(Y(3+K))
 +        S_J       J+1       4                             J=2-N+K+1
          SX0       X0+JZ     5 X7      X0=(Z(2+K))
          NO                  6 X0
*                             7
*                             8
 +        MI        J,[1]     9 X3      WHILE 3-N+K<0
 RMUL1R   ENDM
 RMUL2R   MACRO     JX,JY,JZ,J  J=6-N LEAVES 0 OR 1 TERM TO BE PROCESSED
    LOCAL [1]
 [1]      BSS       0                   FOR K = 0, 2, 4, ...
 +        SA6       A7+JZ     0 X4      Z(K+2)=X(K+2)*Y(K+2)
          S_J       J+2       1                             J=6-N+K+2
          SA7       A6+JZ     2 X3      Z(K+3)=X(K+3)*Y(K+3)
 +        RX6       X2*X4     3 X5      X6=X(K+4)*Y(K+4)
          SA2       A3+JX     4         X2=X(K+6)
          SA4       A5+JY     5         X4=Y(K+6)
          RX7       X3*X5     6         X7=X(K+5)*Y(K+5)
 +        SA3       A2+JX     7 X6      X3=X(K+7)
          SA5       A4+JY     8         X5=Y(K+7)
*                             9
*                            10 X7
          MI        J,[1]    11 X2      WHILE 6-N+K+2<0 I.E. 9+K .LE. N
 RMUL2R   ENDM
 RVMUL    MACRO     N,JX,JY,JZ,J        X2=(X), X4=(Y), X0=(Z)
          LOCAL     [1],[2],[3],[99],LIMIT
 LIMIT    EQU       7
          SA3       X2                  X3=X(1),  A3=(X(1))
          SA5       X4                  X5=Y(1),  A5=(Y(1))
          BRANCH    [3],N,GT,LIMIT,J    IF N>LIMIT          J=LIMIT-N
          S_J       J-LIMIT+2                               J=2-N
          RX7       X3*X5               X7=X(1)*Y(1)
          GT        J,B0,[2]            IF N=1
          SA3       A3+JX               X3=X(2),  A3=(X(2))
          SA5       A5+JY               X5=Y(2),  A5=(Y(2))
          ZR        J,[1]               IF N=2
          RMUL1R    JX,JY,JZ,J
 [1]      SA7       X0                  Z(N-1)=X(N-1)*Y(N-1)
          RX7       X3*X5               X7=X(N)*Y(N)
          SX0       X0+JZ               X0=(Z(N))
 [2]      SA7       X0                  Z(N)=X(N)*Y(N)
          JP        [99]
 [3]      SA2       A3+JX               X2=X(2)   A2=(X(2))
          SA4       A5+JY               X4=Y(2)   A4=(Y(2))
          RX7       X3*X5               X7=X(1)*Y(1)
          SA3       A2+JX               X3=X(3)   A3=(X(3))
          SA5       A4+JY               X5=Y(3)   A5=(Y(3))
          S_J       J-LIMIT+7                               J=7-N
          SA7       X0                  Z(1)=X(1)*Y(1)
          RX6       X2*X4               X6=X(2)*Y(2)
          SA2       A3+JX               X2=X(4)   A2=(X(4))
          SA4       A5+JY               X4=Y(4)   A4=(Y(4))
          RX7       X3*X5               X7=X(3)*Y(3)
          SA3       A2+JX               X3=X(5)   A3=(X(5))
          SA5       A4+JY               X5=Y(5)   A5=(Y(5))
          RMUL2R    JX,JY,JZ,J                              J=0,1
          SA6       A7+JZ
          S_J       J-2                                     J=-2,-1
          SA7       A6+JZ
          RX7       X2*X4
          SX0       A7+JZ
          RMUL1R    JX,JY,JZ,J
          SA7       X0                  Z(N-1)
          RX7       X3*X5               X7=X(N)*Y(N)
          SA7       X0+JZ               Z(N)=X(N)*Y(N)
 [99]     BSS       0
 RVMUL    ENDM
 CMUL1R   MACRO     JX,JY,JZ,J
          LOCAL     [1]
 [1]      BSS       0                   FOR K = 0, 1, 2, ...
          NX6       X6        0
          RX0       X2*X4     1 X3      X0=X'(3+K)*Y'(3+K)
          NX7       X7        2 X6
          RX1       X3*X4     3 X5      X1=X''(3+K)*Y'(3+K)
 +        SA6       A6+JZ     4 X7      Z'(2+K)=X'*Y'-X''*Y''
          RX6       X3*X5     5 X0      X6=X''(3+K)*Y''(3+K)
          SA4       A4+JY     6         X4=Y'(4+K) A4=(Y'(4+K))
          SA7       A7+JZ     7 X1      Z''(2+K)=X''*Y'+X'*Y''
 +        RX7       X2*X5     8         X7=X'(3+K)*Y''(3+K)
          S_J       J+1       9 X6                          J=3-N+K+1
          SA2       A2+JX    10         X2=X'(4+K) A2=(X'(4+K))
 +        RX6       X0-X6    11         X6=X'(3+K)*Y'(3+K)-X''(3+K)*Y''
          SA3       A3+JX    12 X7      X3=X''(4+K) A3=(X''(4+K))
          RX7       X1+X7    13 X4      X7=X''(3+K)*Y'(3+K)+X'(3+K)*Y''
          SA5       A5+JY    14 X6      X5=Y''(4+K) A5=(Y''(4+K))
*                            15
*                            16 X7
 +        MI        J,[1]    17 X2      WHILE 4-N+K<0 I.E. K.LE.N-5
 CMUL1R   ENDM
 CVMUL    MACRO     N,JX,JY,JZ,J        X2=(X), X4=(Y), X0=(Z)
          LOCAL     [1],[2],[3],[99]
          SA2       X2                  X2=X'(1)  A2=(X'(1))
          SA4       X4                  X4=Y'(1)  A4=(Y'(1))
          SA3       A2+1                X3=X''(1) A3=(X''(1))
          SA5       A4+1                X5=Y''(1) A5=(Y''(1))
          S_J       2                                       J=2
          SA6       X0                  Z'(1)=VOID A6=(Z'(1))
          S_J       J-N                                     J=2-N
          RX0       X2*X4               X0=X'(1)*Y'(1)
          RX1       X3*X4               X1=X''(1)*Y'(1)
          RX6       X3*X5               X6=X''(1)*Y''(1)
          RX7       X2*X5               X7=X'(1)*Y''(1)
          GT        J,B0,[3]            IF N=1
          SA4       A4+JY               X4=Y'(2)  A4=(Y'(2))
          SA2       A2+JX               X2=X'(2)  A2=(X'(2))
          SA3       A3+JX               X3=X''(2) A3=(X''(2))
          SA5       A5+JY               X5=Y''(2) A5=(Y''(2))
          RX6       X0-X6               X6=X'(1)*Y'(1)-X''(1)*Y''(1)
          RX7       X1+X7               X7=X''(1)*Y'(1)+X'(1)*Y''(1)
          NX6       X6
          RX0       X2*X4               X0=X'(2)*Y'(2)
          NX7       X7
          RX1       X3*X4               X1=X''(2)*Y'(2)
          SA6       A6                  Z'(1)=X'(1)*Y'(1)-X''(1)*Y''(1)
          RX6       X3*X5               X6=X''(2)*Y''(2)
          SA7       A6+1                Z''(1)=X''(1)*Y'(1)+X'(1)*Y''(1)
          RX7       X2*X5               X7=X'(2)*Y''(2)
          ZR        J,[2]               IF N=2
          SA4       A4+JY               X4=Y'(3)  A4=(Y'(3))
          SA2       A2+JX               X2=X'(3)  A2=(X'(3))
          RX6       X0-X6               X6=X'(2)*Y'(2)-X''(2)*Y''(2)
          RX7       X1+X7               X7=X''(2)*Y'(2)+X'(2)*Y''(2)
          SA3       A3+JX               X3=X''(3) A3=(X''(3))
          SA5       A5+JY               X5=Y''(3) A5=(Y''(3))
          S_J       J+1                                     J=3-N
          ZR        J,[1]               IF N=3
          CMUL1R    JX,JY,JZ,J
 [1]      NX6       X6
          RX0       X2*X4               X0=X'(N)*Y'(N)
          NX7       X7
          RX1       X3*X4               X1=X''(N)*Y'(N)
          SA6       A6+JZ               Z'(N-1)=X'(N-1)*Y'(N-1)-X''*Y''
          RX6       X3*X5               X6=X''(N)*Y''(N)
          SA7       A7+JZ               Z''(N-1)=X'(N-1)*Y''(N-1)+X''*Y'
          RX7       X2*X5               X7=X'(N)*Y''(N)
 [2]      RX6       X0-X6               X6=X'(N)*Y'(N)-X''(N)*Y''(N)
          RX7       X1+X7               X7=X''(N)*Y'(N)+X'(N)*Y''(N)
          NX6       X6
          NX7       X7
          SA6       A6+JZ               Z'(N)=X'(N)*Y'(N)-X''(N)*Y''(N)
          SA7       A7+JZ               Z''(N)=X''(N)*Y'(N)+X'(N)*Y''(N)
          JP        [99]
 [3]      RX7       X1+X7               X7=X''(1)*Y'(1)+X'(1)*Y''(1)
          RX6       X0-X6               X6=X'(1)*Y'(1)-X''(1)*Y''(1)
          NX7       X7
          NX6       X6
          SA7       A6+1                Z''(1)=X''(1)*Y'(1)+X'(1)*Y''(1)
          SA6       A6                  Z'(1)=X'(1)*Y'(1)-X''(1)*Y''(1)
 [99]     BSS       0
 CVMUL    ENDM
*CD       MULA
RMULA1    MACRO     &,JX,JY,JZ,J
          LOCAL     [1]
 [1]      BSS       0                   FOR J=2-N AND K = 0, 1, 2,...
 +        RX6       X3_&_X0   0         X6=Z(1+K) & X(1+K)*Y(1+K)
          SA3       A3+JZ     1 X4      X3=Z(2+K)
          S_J       J+1       2 X5                          J=2-N+K+1
 +        RX0       X4*X5     3 X6      X0=X(2+K)*Y(2+K)
          NX6       X6        4
          SA4       A4+JX     5         X4=X(3+K)
          SA5       A5+JY     6 X6      X5=Y(3+K)
 +        SA6       A3-JZ     7 X0      Z(1+K)=Z(1+K) & X(1+K)*Y(1+K)
*                             8 X3
*                             9
          MI        J,[1]    10         IF 3-N+K<0 I.E. 4+K .LE. N
 RMULA1   ENDM
 RMULA2   MACRO     &,JX,JY,JZ,JZ2,J
          LOCAL     [1]
 [1]      BSS       0                   FOR J=7-N AND K = 0,2,4,...
 +        RX6       X1_&_X6   0 X0      X6=Z(4+K) & X(4+K)*(4+K)
          SA1       A1+JZ     1 X2      X1=Z(5+K)
          S_J       J+2       2 X3                          J=7-N+K+2
 +        SA4       A2+JX     3 X6      X4=X(7+K)
          NX7       X7        4
          NX6       X6        5
          SA5       A3+JY     6 X7      X5=Y(7+K)
 +        SA7       A7+JZ2    7 X6      Z(3+K)=Z(3+K) & X(3+K)*Y(3+K)
          SA6       A6+JZ2    8 X1      Z(4+K)=Z(4+K) & X(4+K)*Y(4+K)
          RX7       X1_&_X0   9         X7=Z(5+K) & X(5+K)*Y(5+K)
          SA1       A1+JZ    10 X4      X1=Z(6+K)
 +        RX6       X2*X3    11         X6=X(6+K)*Y(6+K)
          SA2       A4+JX    12 X7      X2=X(8+K)
          SA3       A5+JY    13 X5      X3=Y(8+K)
          RX0       X4*X5    14         X0=X(7+K)*Y(7+K)
*                            15 X6
*                            16
 +        MI        J,[1]    17 X1      IF 9-N+K<0 I.E. 10+K .LE. N
 RMULA2   ENDM
 RVMULA   MACRO     N,&,JX,JY,Z,JZ,J    X2=(X), X4=(Y)
          LOCAL     [1],[2],[3],[4],[99]
          SA5       X4                  X5=Y(1)   A5=(Y(1))
          SA4       X2                  X4=X(1)   A4=(X(1))
          BRANCH    [1],N,LE,7,J                            J=7-N
          SA2       A4+JX               X2=X(2)   A2=(X(2))
          SA3       A5+JY               X3=Y(2)   A3=(Y(2))
          RX0       X4*X5               X0=X(1)*Y(1)
          SA1       Z                   X1=Z(1)   A1=(Z(1))
          SA4       A2+JX               X4=X(3)   A4=(X(3))
          SA5       A3+JY               X5=Y(3)   A5=(Y(3))
          RX7       X1_&_X0             X7=Z(1) & X(1)*Y(1)
          SA1       A1+JZ               X1=Z(2)   A1=(Z(2))
          RX6       X2*X3               X6=X(2)*Y(2)
          SA2       A4+JX               X2=X(4)   A2=(X(4))
          SA3       A5+JY               X3=Y(4)   A3=(Y(4))
          RX0       X4*X5               X0=X(3)*Y(3)
          RX6       X1_&_X6             X6=Z(2) & X(2)*Y(2)
          SA1       A1+JZ               X1=Z(3)   A1=(Z(3))
          SA4       A2+JX               X4=X(5)   A4=(X(5))
          NX7       X7
          NX6       X6
          SA5       A3+JY               X5=Y(5)   A5=(Y(5))
          SA7       Z                   Z(1)=Z(1) & X(1)*Y(1)
          SA6       Z+JZ                Z(2)=Z(2) & X(2)*Y(2)
          S_Z       JZ+JZ                                   Z=2*JZ
          RX7       X1_&_X0             X7=Z(3) & X(3)*Y(3)
          SA1       A1+JZ               X1=Z(4)   A1=(Z(4))
          RX6       X2*X3               X6=X(4)*Y(4)
          SA2       A4+JX               X2=X(6)   A2=(X(6))
          SA3       A5+JY               X3=Y(6)   A3=(Y(6))
          RX0       X4*X5               X0=X(5)*Y(5)
          RMULA2    &,JX,JY,JZ,Z,J      NOTE THAT Z=2*JZ
          RX6       X1_&_X6
          SA1       A1+JZ
          NZ        J,[4]               IF N IS EVEN
          SA4       A2+JX               X4=X(N)
          NX7       X7
          NX6       X6
          SA5       A3+JY               X5=Y(N)
          SA7       A6+JZ               Z(N-4)=Z(N-4) & X(N-4)*Y(N-4)
          SA6       A7+JZ               Z(N-3)=Z(N-3) & X(N-3)*Y(N-3)
          RX7       X1_&_X0             X7=Z(N-2) & X(N-2)*Y(N-2)
          SA1       A1+JZ               X1=Z(N-1) A1=(Z(N-1))
          RX6       X2*X3               X6=X(N-1)*Y(N-1)
          RX0       X4*X5               X0=X(N)*Y(N)
          RX6       X1_&_X6             X6=Z(N-1) & X(N-1)*Y(N-1)
          SA1       A1+JZ               X1=Z(N)
          NX7       X7
          NX6       X6
          SA7       A6+JZ               Z(N-2)=Z(N-2) & X(N-2)*Y(N-2)
          SA6       A7+JZ               Z(N-1)=Z(N-1) & X(N-1)*Y(N-1)
          RX7       X1_&_X0             X7=Z(N) & X(N)*Y(N)
          NX7       X7
          SA7       A6+JZ               Z(N)=Z(N) & X(N)*Y(N)
          JP        [99]
 [1]      SA3       Z                   X3=Z(1)   A3=(Z(1))
          S_J       J-5                                     J=2-N
          RX0       X4*X5               X0=X(1)*Y(1)
          GT        J,B0,[3]            IF N=1
          SA4       A4+JX               X4=X(2)   A4=(X(2))
          SA5       A5+JY               X5=Y(2)   A5=(Y(2))
          PL        J,[2]               IF N=2
          RMULA1    &,JX,JY,JZ,J                            J=0
 [2]      RX6       X3_&_X0             X6=Z(N-1) & X(N-1)*Y(N-1)
          SA3       A3+JZ               X3=Z(N)   A3=(Z(N))
          RX0       X4*X5               X0=X(N)*Y(N)
          NX6       X6
          SA6       A3-JZ               Z(N-1)=Z(N-1) & X(N-1)*Y(N-1)
 [3]      RX6       X3_&_X0             X6=Z(N) & X(N)*Y(N)
          NX6       X6
          NO
          SA6       A3                  Z(N)=Z(N) & X(N)*Y(N)
          JP        [99]
 [4]      NX7       X7
          NX6       X6
          SA7       A6+JZ               Z(N-3)=Z(N-3) & X(N-3)*Y(N-3)
          SA6       A7+JZ               Z(N-2)=Z(N-2) & X(N-2)*Y(N-2)
          RX7       X1_&_X0             X7=Z(N-1) & X(N-1)*Y(N-1)
          SA1       A1+JZ               X1=Z(N) A1=(Z(N))
          RX6       X2*X3               X6=X(N)*Y(N)
          RX6       X1_&_X6             X6=Z(N) & X(N)*Y(N)
          NX7       X7
          NX6       X6
          SA7       A6+JZ               Z(N-1)=Z(N-1) & X(N-1)*Y(N-1)
          SA6       A1                  Z(N)=Z(N) & X(N)*Y(N)
 [99]     BSS       0
 RVMULA   ENDM
 CMULA1   MACRO     &,JX,JY,KZ,J,ONE
          LOCAL     [1]
 [1]      BSS       0                   FOR J = 3-N AND K = 0,1,2,...
 AT       IFC       EQ,*&*+*
          RX6       X0-X6     0 X1      X6=Z'(2+K) & X'Y'-X''Y''
 AT       ELSE
          RX6       X0+X6
 AT       ENDIF
          RX7       X5_&_X7   1 X2      X7=Z''(2+K) & (X'Y''+X''Y')
          S_J       J+ONE                                   J=3-N+K+1
          SA5       A5+KZ     3 X6      X5=Z'(3+K)
          NX6       X6        4 X7
          RX0       X1*X3     5         X0=X'(3+K)Y'(3+K)
          NX7       X7        6 X6
          RX1       X1*X4     7         X1=X'(3+K)Y''(3+K)
          SA6       A7+KZ     8 X7      Z'(2+K)=Z'&(X'Y'-X''Y'')
          RX6       X2*X4     9 X0      X6=X''(3+K)Y''(3+K)
          SA4       A4+JY    10 X5      X4=Y''(4+K)
          RX2       X2*X3    11 X1      X2=X''(3+K)Y'(3+K)
          SA3       A3+JY    12         X3=Y'(4+K)
          RX0       X5_&_X0  13 X6      X0=Z'(3+K) & X'Y'
          SA5       A5+ONE   14         X5=Z''(3+K)
          SA7       A6+ONE   15 X2      Z''(2+K)=Z''&(X'Y''+X''Y')
*                            16 X0
          RX7       X1+X2    17 X4      X7=X'(3+K)Y''(3+K)+X''Y'
          SA1       A1+JX    18         X1=X'(4+K)
          SA2       A2+JX    19 X3      X2=X''(4+K)
          NX0       X0       20 X7
          NX7       X7       21 X5
*                            22 X0
*                            23 X7
          MI        J,[1]    24         WHILE 4-N+K<0 I.E. 5+K .LE. N
 CMULA1   ENDM
 CVMULA   MACRO     N,&,JX,JY,KZ,J,ONE  X2=(X),   X4=(Y),   A0=(Z)
          LOCAL     [1],[2],[3],[99]
 CVMULSUB MACRO     &
 AT       IFC       EQ,*&*+*
          RX6       X0-X6               X6=Z'+X'Y'-X''Y''  IF & IS +
 AT       ELSE
          RX6       X0+X6               X6=Z'-X'Y'+X''Y'' IF & IS -
 AT       ENDIF
 CVMULSUB ENDM
          SA1       X2                  X1=X'(1)
          SA3       X4                  X3=Y'(1)
          SA2       A1+ONE              X2=X''(1)
          SA4       A3+ONE              X4=Y''(1)
          SA5       A0                  X5=Z'(1)
          S_J       ONE+ONE
          S_J       J-N                                     J=2-N
          RX0       X1*X3               X0=X'(1)Y'(1)
          RX1       X1*X4               X1=X'(1)Y''(1)
          RX6       X2*X4               X6=X''(1)Y''(1)
          RX2       X2*X3               X2=X''(1)Y'(1)
          GT        J,B0,[3]            IF N=1
          SA4       A4+JY               X4=Y''(2)
          SA3       A3+JY               X3=Y'(2)
          S_KZ      KZ-ONE                                  KZ=JZ-1
          RX0       X5_&_X0             X0=Z'(1) & X'Y'
          SA5       A5+ONE              X5=Z''(1)
          RX7       X1+X2               X7=X'(1)Y''(1)+X''Y'
          SA1       A1+JX               X1=X'(2)
          SA2       A2+JX               X2=X''(2)
          NX0       X0
          NX7       X7
          CVMULSUB  &                   X6=Z'(1) & (X'Y'-X''Y'')
          RX7       X5_&_X7             X7=Z''(1) & (X'Y''+X''Y')
          SA5       A5+KZ               X5=Z'(2)
          NX6       X6
          RX0       X1*X3               X0=X'(2)Y'(2)
          NX7       X7
          RX1       X1*X4               X1=X'(2)Y''(2)
          SA6       A0                  Z'(1)=Z' & (X'Y'-X''Y'')
          RX6       X2*X4               X6=X''(2)Y''(2)
          ZR        J,[2]               IF N=2
          SA4       A4+JY               X4=Y''(3)
          RX2       X2*X3               X2=X''(2)Y'(2)
          SA3       A3+JY               X3=Y'(3)
          RX0       X5_&_X0             X0=Z'(2) & X'Y'
          SA5       A5+ONE              X5=Z''(2)
          SA7       A6+ONE              Z''(1)=Z''& (X'Y''+X''Y')
          RX7       X1+X2               X7=X'(2)Y''(2)+X''Y'
          SA1       A1+JX               X1=X'(3)
          SA2       A2+JX               X2=X''(3)
          S_J       J+ONE                                   J=3-N
          NX0       X0
          NX7       X7
          ZR        J,[1]               IF N=3
          CMULA1    &,JX,JY,KZ,J,ONE
 [1]      CVMULSUB  &                   X6=Z'(N-1) & (X'Y'-X''Y'')
          RX7       X5_&_X7             X7=Z''(N-2) & (X'Y''+X''Y')
          SA5       A5+KZ               X5=Z'(N)
          NX6       X6
          RX0       X1*X3               X0=X'(N)Y'(N)
          NX7       X7
          RX1       X1*X4               X1=X'(N)Y''(N)
          SA6       A7+KZ               Z'(N-1)=Z' & (X'Y'-X''Y'')
          RX6       X2*X4               X6=X''(N)Y''(N)
 [2]      RX2       X2*X3               X2=X''(N)Y'(N)
          RX0       X5_&_X0             X0=Z'(N) & X'Y'
          SA5       A5+ONE              X5=Z''(N)
          SA7       A6+ONE              Z''(N-1)=Z'' & (X'Y''+X''Y')
          RX7       X1+X2               X7=X'(N)Y''(N)+X''Y'
          NX0       X0
          NX7       X7
          CVMULSUB  &                   X6=Z'(N) & (X'Y'-X''Y'')
          RX7       X5_&_X7             X7=Z''(N) & (X'Y''+X''Y')
          NX6       X6
          NX7       X7
          SA6       A7+KZ               Z'(N)=Z' & (X'Y'-X''Y'')
          S_KZ      KZ+ONE                                  KZ=JZ
          SA7       A6+ONE              Z''(N)=Z'' & (X'Y''+X''Y')
          JP        [99]
 [3]      RX0       X5_&_X0             X0=Z'(1) & X'Y'
          SA5       A5+ONE              X5=Z''(1)
          RX7       X1+X2               X7=X'(1)Y''(1)+X''Y'
          NX0       X0
          NX7       X7
          CVMULSUB  &                   X6=Z'(1) & (X'Y'-X''Y'')
          RX7       X5_&_X7             X7=Z''(1) & (X'Y''+X''Y')
          NX6       X6
          NX7       X7
          SA6       A0                  Z'(1)=Z' & (X'Y'-X''Y'')
          SA7       A6+ONE              Z''(1)=Z'' & (X'Y''+X''Y')
 [99]     BSS       0
 CVMULA   ENDM
*CD       RAN
 RRAN1R   MACRO     JZ,J             X0=A,X3=C,X4=SEED,X5=MULT,A6=(Z(1))
          LOCAL     [1]
 [1]      BSS       0                   FOR K = 0, 1, 2, ...
 +        SA6       A6+JZ     0 X2      Z(2+K)=C*RANF(2+K)+A
          FX2       X2+X0     1         X2=C*RANF(3+K)+A
          S_J       J+1       2                             J=4-N+K+1
 +        NX1       X4        3         X1=RANF(4+K)
          DX4       X4*X5     4 X2      X4=SEED(4+K)*MULT=SEED(5+K)
          NX6       X2        5 X1      X6=C*RANF(3+K)+A
          FX2       X3*X1     6         X2=C*RANF(4+K)
*                             7 X6
*                             8 X4
 +        MI        J,[1]     9         WHILE 5-N+K<0; I.E. 6+K .LE. N
 RRAN1R   ENDM
 RVRAN    MACRO     N,Z,JZ,J            X0=A, X3=C,X4=SEED,X5=MULT
          LOCAL     [1],[2],[3],[4],[5],[6],[99]
          S_J       4
          DX4       X4*X5               X4=SEED(1)
          S_J       J-N                                     J=4-N
          LE        J,B0,[3]            IF 4-N .LE. 0; I.E. N>3
          S_J       J-2                                     J=2-N
          NX1       X4                  X1=RANF(1)
          GT        J,B0,[1]            IF 2-N>0; I.E. N=1
          DX4       X4*X5               X4=SEED(2)
          FX2       X3*X1               X2=C*RANF(1)
          NX1       X4                  X1=RANF(2)
          FX2       X2+X0               X2=C*RANF(1)+A
          ZR        J,[2]               IF 2-N=0; I.E. N=2
          DX4       X4*X5               X4=SEED(3)
          NX6       X2                  X6=C*RANF(1)+A
          FX2       X3*X1               X2=C*RANF(2)
          SA6       Z                   Z(1)=C*RANF(1)+A
          JP        [5]
 [1]      FX2       X3*X1               X2=C*RANF(1)
          FX2       X2+X0               X2=C*RANF(1)+A
          BX7       X4                  X7=SEED
          NX6       X2                  X6=C*RANF(1)+A
          SA6       Z                   Z(1)=C*RANF(1)+A
          JP        [99]
 [2]      NX6       X2                  X6=C*RANF(1)+A
          FX2       X3*X1               X2=C*RANF(2)
          SA6       Z                   Z(1)=C*RANF(1)+A
          JP        [6]
 [3]      NX1       X4                  X1=RANF(1)
          DX4       X4*X5               X4=SEED(2)
          FX2       X3*X1               X2=C*RANF(1)
          NX1       X4                  X1=RANF(2)
          FX2       X2+X0               X2=C*RANF(1)+A
          DX4       X4*X5               X4=SEED(3)
          NX6       X2                  X6=C*RANF(1)+A
          FX2       X3*X1               X2=C*RANF(2)
          SA6       Z                   Z(1)=C*RANF(1)+A
          NX1       X4                  X1=RANF(3)
          FX2       X2+X0               X2=C*RANF(2)+A
          DX4       X4*X5               X4=SEED(4)
          NX6       X2                  X6=C*RANF(2)+A
          FX2       X3*X1               X2=C*RANF(3)
          ZR        J,[4]               IF N=4
          RRAN1R    JZ,J                                    J=0
 [4]      SA6       A6+JZ               Z(N-2)=C*RANF(N-2)+A
 [5]      FX2       X2+X0               X2=C*RANF(N-1)+A
          NX1       X4                  X1=RANF(N)
          NX6       X2                  X6=C*RANF(N-1)+A
          FX2       X3*X1               X2=C*RANF(N)
          SA6       A6+JZ               Z(N-1)=C*RANF(N-1)+A
 [6]      FX2       X2+X0               X2=C*RANF(N)+A
          BX7       X4                  X7=SEED
          NX6       X2                  X6=C*RANF(N)+A
          SA6       A6+JZ               Z(N)=C*RANF(N)+A
 [99]     BSS       0                   X7=OLD SEED
 RVRAN    ENDM
 RMRAN    MACRO     M,N,Z,IZ,JZ,I,J    X0=A, X3=C, X4=SEED, X5=MULT
          LOCAL     [1],[2],[3],[4],[5],[99]
          LE        M,N,[1]             IF M .LE. N  GOTO [1]
          SWAP      M,N                 EXCHANGE M  AND N
          SWAP      IZ,JZ               EXCHANGE IZ AND JZ
 [1]      BRANCH    [4],N,LE,3,J
 [2]      DX4       X4*X5     0         X4=SEED(1)
          S_J       4         1
*                             2
          S_J       J-N       3                             J=4-N
*                             4 X4
 +        NX1       X4        5         X1=RANF(1)
          DX4       X4*X5     6         X4=SEED(2)
*                             7 X1
          FX2       X3*X1     8         X2=C*RANF(1)
*                             9
*                            10 X4
          NX1       X4       11         X1=RANF(2)
*                            12 X2
 +        FX2       X2+X0    13 X1      X2=C*RANF(1)+A
          DX4       X4*X5    14         X4=SEED(3)
*                            15
*                            16 X2
          NX6       X2       17         X6=X(I,1)
          FX2       X3*X1    18 X4      X2=C*RANF(2)
 +        NX1       X4       19 X6      X1=RANF(3)
          SA6       Z        20         Z(I,1)=C+RANF(1)+A
          DX4       X4*X5    21 X1      X4=SEED(4)
*                            22 X2
          FX2       X2+X0    23         X2=C*RANF(2)+A
*                            24
*                            25 X4
*                            26 X2
 +        NX6       X2       27         X6=Z(I,2)
          FX2       X3*X1    28         X2=C*RANF(3)
*                            29 X6
          ZR        J,[3]    30         IF N=4
          RRAN1R    JZ,J
 [3]      SA6       A6+JZ               Z(I,N-2)=C*RANF(N-2)+A
          FX2       X2+X0               X2=C*RANF(N-1)+A
          NX1       X4                  X1=RANF(N)
          NX6       X2                  X6=C*RANF(N-1)+A
          FX2       X3*X1               X2=C*RANF(N)
          S_M       M-1                                     M=M-1
          SA6       A6+JZ               Z(I,N-1)=C*RANF(N-1)+A
          FX2       X2+X0               X2=C*RANF(N)+A
          S_Z       Z+IZ                Z=(Z(I+1,1))
          NX6       X2                  X6=C*RANF(N)+A
          SA6       A6+JZ               Z(I,N)=C*RANF(N)+A
          NZ        M,[2]
          JP        [99]
 [4]      S_J       B0                                      J=0
          SX7       Z                   X6=(Z(I,1))
          S_M       M-1
 [5]      DX4       X4*X5               X4=SEED(J)
          NX1       X4                  X1=RANF
          FX2       X3*X1               X2=C*RANF
          FX2       X2+X0               X2=C*RANF+A
          S_J       J+1
          NX6       X2                  X6=Z(I,J)
          SA6       X7                  Z(I,J)=C*RANF+A
          SX7       X7+JZ               X7=(Z(I,J+1))
          NE        J,N,[5]
          S_Z       Z+IZ                Z=(Z(I+1,1))
          NZ        M,[4]
 [99]     BSS       0                   X4=OLD SEED
 RMRAN    ENDM
 DRAN1R   MACRO     JZ,I,J              X0=A, X3=C, X4=SEED, X5=MULT
          LOCAL     [1]
 [1]      BSS       0                   FOR K = 0, 1, 2, ...
          DX4       X4*X5               X4=S''*M=S'
          NX1       X4                  X1=R'
          DX4       X4*X5               X4=S'*M=S''
          FX1       X0*X1               X1=C*R'
          FX6       X1+X3               X6=C*R'+A'
          NX6       X6                  X6=Z'
          UX2       I,X6                                    I=E'
          BX7       X6-X4               X7=M''
          S_I       I-48                                    I=E'-48
          SA6       A6+JZ               Z'=
          PX7       X7,I                X7=Z''
          SA7       A6+1
          S_J       J+1
          MI        J,[1]
 DRAN1R   ENDM
 DMRAN    MACRO     M,N,X,IX,JX,I,J    X0=A, X3=C, X4=SEED, X5=MULT
          LOCAL     [1],[2],[3]
          LE        M,N,[1]             IF M .LE. N  GOTO [1]
          SWAP      M,N                 EXCHANGE M  AND N
          SWAP      IX,JX               EXCHANGE IX AND JX
 [1]      BSS       0
 [2]      S_J       1
          S_J       J-N                                     J=1-N
          DX4       X4*X5               X4=S''*M=S'
          NX1       X4                  X1=R'
          DX4       X4*X5               X4=S'*M=S''
          FX1       X0*X1               X1=C*R'
          FX6       X1+X3               X6=C*R'+A'
          NX6       X6                  X6=X'
          UX2       I,X6                                    I=E'
          BX7       X6-X4               X7=M''
          S_I       I-48                                    I=E'-48
          SA6       X                   X'=
          PX7       X7,I                X7=X''
          SA7       A6+1
          ZR        J,[3]
          DRAN1R    JX,I,J
 [3]      S_M       M-1
          S_X       X+IX
          NZ        M,[2]
 DMRAN    ENDM
 CRAN1R   MACRO     JX,I,J              X0=A, X3=C, X4=SEED, X5=MULT
          LOCAL     [1]
 [1]      BSS       0                   FOR K = 0, 1, 2,...
 +        NX2       X4        0 X1      X2=RANF''(2+K)
          FX6       X1+X0     1         X6=C*RANF'(2+K)+A
          SA7       A6+1      2 X2      X''(I,1+K)=C*RANF''(1+K)+A
 +        FX2       X3*X2     3         X2=C*RANF''(2+K)
          S_J       J+1       4 X6                          J=2-N+K+1
          DX4       X4*X5     5         X4=SEED'(3+K)
 +        NX6       X6        6         X6=X'(I,2+K)
          NO                  7 X2
          FX7       X2+X0     8 X6      X7=C*RANF''(2+K)+A
          SA6       A6+JX     9 X4      X'(I,2+K)=C*RANF'(2+K)+A
 +        NX1       X4       10         X1=RANF'(3+K)
          DX4       X4*X5    11 X7      X4=SEED''(3+K)
          NX7       X7       12 X1      X7=X''(I,2+K)
          FX1       X3*X1    13         X1=C*RANF'(3+K)
*                            14 X7
*                            15 X4
 +        MI        J,[1]    16         WHILE 3-N+K<0; I.E. 4+K.LE.N
 CRAN1R   ENDM
 CMRAN    MACRO     M,N,X,IX,JX,I,J    X0=A, X3=C, X4=SEED, X5=MULT
          LOCAL     [1],[2],[3],[4],[99]
          LE        M,N,[1]             IF M .LE. N  GOTO [1]
          SWAP      M,N                 EXCHANGE M  AND N
          SWAP      IX,JX               EXCHANGE IX AND JX
 [1]      BSS       0
 [2]      BSS       0
 +        DX4       X4*X5               X4=SEED'(1)
          S_J       1
          S_J       J-N                                     J=1-N
 +        NX1       X4                  X1=RANF'(1)
          DX4       X4*X5               X4=SEED''(1)
          FX1       X3*X1               X1=C*RANF'(1)
          NX2       X4                  X2=RANF''(1)
 +        FX6       X1+X0               X6=C*RANF'(1)+A
          FX2       X3*X2               X2=C*RANF''(1)
          ZR        J,[4]               IF N=1
 +        DX4       X4*X5               X4=SEED'(2)
          NX6       X6                  X6=X'(I,1)
          S_J       J+1                                     J=2-N
 +        FX7       X2+X0               X7=C*RANF''(1)+A
          SA6       X                   X'(I,1)=C*RANF'(1)+A
          NX1       X4                  X1=RANF'(2)
          DX4       X4*X5               X4=SEED''(2)
 +        NX7       X7                  X7=X''(I,1)
          FX1       X3*X1               X1=C*RANF'(2)
          ZR        J,[3]               IF N=2
          CRAN1R    JX,I,J
 [3]      NX2       X4                  X2=RANF''(N)
          FX6       X1+X0               X6=C*RANF'(N)+A
          SA7       A6+1                X''(N-1)=C*RANF''(N-1)+A
 +        FX2       X3*X2               X2=C*RANF''(N)
          S_M       M-1                                     M=M-1
          NX6       X6                  X6=X'(I,N)
 +        S_X       X+IX                X=(X(I+1,1))
          FX7       X2+X0               X7=C*RANF''(N)+A
          SA6       A6+JX               X'(I,N)=C*RANF'(N)+A
          NX7       X7                  X7=X''(I,N)
          SA7       A6+1                X''(I,N)=C*RANF''(N)+A
          NZ        M,[2]
          JP        [99]
 [4]      NX6       X6                  X6=X'(1,1)
          FX7       X2+X0               X7=C*RANF''(1)+A
          SA6       X                   X'(1,1)=C*RANF'(1)+A
          NX7       X7                  X7=X''(1,1)
          SA7       A6+1                X''(1,1)=C*RANF''(1)+A
 [99]     BSS       0
 CMRAN    ENDM
*CD       ROT
 RROT1    MACRO     JX,JY,J
          LOCAL     [1]
[1]       BSS       0                   FOR J=2-N AND K=0, 1, 2,...
          NX6       X6        0  X4
          RX2       X0*X4     1  X5     X2=CX(2+K)
          NX7       X7        2  X6
          RX3       X0*X5     3         X3=CY(2+K)
          SA6       A4-JX     4  X7     X(1+K)=CX(1+K)+SY(1+K)
          RX6       X1*X5     5  X2     X6=SY(2+K)
          SA7       A5-JY     6         Y(1+K)=CY(1+K)-SX(1+K)
          RX7       X1*X4     7  X3     X7=SX(2+K)
          S_J       J+1       8                             J=2-N+K+1
          SA4       A4+JX     9  X6     X4=X(3+K) A4=(X(3+K))
          SA5       A5+JY    10         X5=Y(3+K) A5=(Y(3+K))
          RX6       X2+X6    11 X7      X6=CX(2+K)+SY(2+K)
          RX7       X3-X7    12         X7=CY(2+K)-SX(2+K)
*                            13
*                            14  X6
          MI        J,[1]    15  X7     IF 3-N+K<0, I.E. 4+K .LE. N
 RROT1    ENDM
 RVROT    MACRO     N,X,JX,Y,JY,J
*                                       X0=C
*                                       X1=S
          LOCAL     [1],[2]
          SA4       X                   X4=X(1)   A4=(X(1))
          SA5       Y                   X5=Y(1)   A5=(Y(1))
          S_J       2
          S_J       J-N                                     J=2-N
          RX2       X0*X4               X2=CX(1)
          RX3       X0*X5               X3=CY(1)
          RX6       X1*X5               X6=SY(1)
          RX7       X1*X4               X7=SX(1)
          GT        J,B0,[2]            IF N=1
          SA4       A4+JX               X4=X(2)   A4=(X(2))
          SA5       A5+JY               X5=Y(2)   A5=(Y(2))
          RX6       X2+X6               X6=CX(1)+SY(1)
          RX7       X3-X7               X7=CY(1)-SX(1)
          ZR        J,[1]               IF N=2
          RROT1     JX,JY,J
 [1]      NX6       X6                  X6=CX(N-1)+SY(N-1)
          RX2       X0*X4               X2=CX(N)
          NX7       X7                  X7=CY(N-1)-SX(N-1)
          RX3       X0*X5               X3=CY(N)
          SA6       A4-JX               X(N-1)=CX(N-1)+SY(N-1)
          RX6       X1*X5               X6=SY(N)
          SA7       A5-JY               Y(N-1)=CY(N-1)-SX(N-1)
          RX7       X1*X4               X7=SX(N)
 [2]      RX6       X2+X6               X6=CX(N)+SY(N)
          RX7       X3-X7               X7=CY(N)-SX(N)
          NX6       X6
          NX7       X7
          SA6       A4                  X(N)=CX(N)+SY(N)
          SA7       A5                  Y(N)=CY(N)-SX(N)
 RVROT    ENDM
*CD       SCA
 RSCA1R   MACRO     JX,#,JY,JZ,J        X1=S
          LOCAL     [1]
 [1]      BSS       0                   FOR K = 0, 1, 2, ...
 +        NX7       X6        0         X7=S*X(2+K)#Y(2+K)
          RX3       X1*X4     1         X3=S*X(3+K)
          S_J       J+1       2 X7                          J=3-N+1+K
 +        SA4       A4+JX     3 X5      X4=X(4+K)
          SA7       A7+JZ     4         Z(2+K)=S*X(2+K)#Y(2+K)
*                             5 X3
          RX6       X3_#_X5   6         X6=S*X(3+K)#Y(3+K)
          SA5       A5+JY     7         X5=Y(4+K)
*                             8
*                             9 X6
 +        MI        J,[1]    10 X4      WHILE 4-N+K=J<0; I.E. 5+K.LE.N
 RSCA1R   ENDM
 RSCA4R   MACRO     JX,#,JY,JZ,J        X1=S
          LOCAL     [1]
 [1]      BSS       0                   K = 0, 4, 8, ...
 +        NX7       X7        0 X4      X7=S*X(3+K)#Y(3+K)
          RX4       X1*X4     1 X5      X4=S*X(5+K)
          SA6       A7+JZ     2 X7      Z(2+K)=S*X(2+K)#Y(2+K)
          SA2       A4+JX     3         X2=X(6+K)
 +        RX6       X0_#_X3   4         X6=S*X(4+K)#Y(4+K)
          SA7       A6+JZ     5 X4      Z(3+K)=S*X(3+K)#Y(3+K)
          RX7       X4_#_X5   6         X7=S*X(5+K)#Y(5+K)
          SA4       A2+JX     7 X6      X4=X(7+K)
 +        SA3       A5+JY     8         X3=Y(6+K)
          NX6       X6        9 X7      X6=S*X(4+K)#Y(4+K)
          NX7       X7       10 X2      X7=S*X(5+K)#Y(5+K)
          SA5       A3+JY    11 X6      X5=Y(7+K)
 +        RX0       X1*X2    12 X7      X0=S*X(6+K)
          SA6       A7+JZ    13         Z(4+K)=S*X(4+K)#Y(4+K)
          SA2       A4+JX    14 X4      X2=X(8+K)
          RX4       X1*X4    15 X3      X4=S*X(7+K)
 +        S_J       J+4      16 X0                          J=8-N+4+K
          RX6       X0_#_X3  17         X6=S*X(6+K)#Y(6+K)
          SA3       A5+JY    18 X5      X3=Y(8+K)
 +        SA7       A6+JZ    19 X4      Z(5+K)=S*X(5+K)#Y(5+K)
          RX7       X4_#_X5  20 X6      X7=S*X(7+K)#Y(7+K)
          SA4       A2+JX    21 X2      X4=X(9+K)
          SA5       A3+JY    22         X5=Y(9+K)
 +        RX0       X1*X2    23 X7      X0=S*X(8+K)
          NX6       X6       24         X6=S*X(6+K)#Y(6+K)
*                            25 X3
*                            26 X6
          MI        J,[1]    27 X0      WHILE 12-N+K=J<0; I.E. 13+K.LE.N
 RSCA4R   ENDM
 RVSCA    MACRO     N,JX,#,JY,Z,JZ,J    X4=(X) X2=(Y) X1=S
          LOCAL     [1],[2],[3],[4],[5],[6],[99]
 [1]      SA4       X4        0         X4=X(1)
          S_J       8         1                             J=8
          SA5       X2        2         X5=Y(1)
 +        S_J       J-N       3                             J=8-N
*                             4
*                             5
          PL        J,[4]     6         IF N<9
          SA2       A4+JX     7 X4      X2=X(2)
 +        RX7       X1*X4     8         X7=S*X(1)
          NO                  9 X5
          SA4       A2+JX    10         X4=X(3)
          SA3       A5+JY    11         X3=Y(2)
*                            12 X7
 +        RX7       X7_#_X5  13         X7=S*X(1)#Y(1)
          SA5       A3+JY    14 X2      X5=Y(3)
          NO                 15
          RX6       X1*X2    16 X7      X6=S*X(2)
 +        NX7       X7       17 X4      X7=S*X(1)#Y(1)
          SA2       A4+JX    18 X3      X2=X(4)
          RX4       X1*X4    19 X7      X4=S*X(3)
          SA7       Z        20 X6      Z(1)=S*X(1)#Y(1)
 +        RX6       X6_#_X3  21 X5      X6=S*X(2)#Y(2)
          SA3       A5+JY    22         X3=Y(4)
          NO                 23 X4
          RX7       X4_#_X5  24 X6      X7=S*X(3)#Y(3)
 +        SA4       A2+JX    25 X2      X4=X(5)
          NX6       X6       26         X6=S*X(2)#Y(2)
          RX0       X1*X2    27 X7      X0=S*X(4)
          SA5       A3+JY    28 X6      X5=Y(5)
*                            29 X3
*                            30
*                            31 X0
*                            32 X4
          RSCA4R    JX,#,JY,JZ,J X6
          SA6       A7+JZ    27 X0      Z(J+N-6)=S*X(J+N-6)#Y(J+N-6)
          NX7       X7       28 X4      X7=S*X(J+N-5)#Y(J+N-5)
          RX6       X0_#_X3  29 X5      X6=S*X(J+N-4)#Y(J+N-4)
          S_J       J-3                                     J=0,-1,-2,-3
          SA7       A6+JZ               Z(J+N-5)=S*X(J+N-5)#Y(J+N-5)
          MI        J,[5]               IF 1, 2, OR 3 TERMS LEFT
          JP        [6]                 NO TERMS LEFT
 [2]      RX7       X1*X4               X7=S*X(1)
          RX7       X7_#_X5             X7=S*X(1)#Y(1)
          NX7       X7
          SA7       Z                   Z(1)=S*X(1)#Y(1)
          JP        [99]
 [3]      RX7       X1*X2               X7=S*X(2)
          RX6       X6_#_X5             X6=S*X(1)#Y(1)
          RX7       X7_#_X3             X7=S*X(2)#Y(2)
          SA4       A2                            A4=(X(2))
          NX6       X6
          SA5       A3                            A5=(Y(2))
          NX7       X7
          SA6       Z                   Z(1)=S*X(1)#Y(1)
          SA7       Z+JZ                Z(2)=S*X(2)#Y(2)
          JP        [99]
 [4]      S_J       J-7       0                             J=1-N
*                             1
*                             2
          ZR        J,[2]     3         IF N=1
 +        SA2       A4+JX     4         X2=X(2)
          S_J       J+1       5 X4                          J=2-N
          RX6       X1*X4     6 X5      X6=S*X(1)
*                             7
*                             8
 +        SA3       A5+JY     9         X3=Y(2)
*                            10 X6
          ZR        J,[3]    11 X2      IF N=2
          RX6       X6_#_X5  12         X6=S*X(1)#Y(1)
 +        RX2       X1*X2    13         X2=S*X(2)
          S_J       J+1      14                             J=3-N
*                            15 X6
          NX7       X6       16 X3      X7=S*X(1)#Y(1)
 +        SA4       A2+JX    17 X2      X4=X(3)
          RX6       X2_#_X3  18 X7      X6=S*X(2)#Y(2)
          SA7       Z        19         Z(1)=S*X(1)#Y(1)
          SA5       A3+JY    20         X5=Y(3)
*                            21
 +        PL        J,[6]    22         IF N=3
 [5]      RSCA1R    JX,#,JY,JZ,J
 [6]      NX7       X6                  X7=S*X(N-1)#Y(N-1)
          RX6       X1*X4               X6=S*X(N)
          SA7       A7+JZ               Z(N-1)=S*X(N-1)#Y(N-1)
          RX7       X6_#_X5             X7=S*X(N)#Y(N)
 +        SB0       0
          NX7       X7
          SA7       A7+JZ               Z(N)=S*X(N)#Y(N)
 [99]     BSS       0
 RVSCA    ENDM
 DSCA1R   MACRO     JX,#,JY,JZ,J        X0=S'  X1=S''
          LOCAL     [1]
 [1]      BSS       0                   FOR K = 0, 1, 2, ...
 +        FX5       X3+X7     0         X5=U(D'+E')=F'
*                             1
*                             2
*                             3 X5
          FX6       X4+X5     4         X6=U(C'+F')=G'
          DX7       X4+X5     5         X7=L(C'+F')=G''
          SA3       A3+JX     6         X3=X''(3+K)
*                             7 X6
 +        NX5       X6        8 X7      X5=G'
          NX7       X7        9
          SA2       A2+JX    10 X5      X2=X'(3+K)
          SA4       A4+JY    11 X7      X4=Y'(3+K)
 +        FX6       X5+X7    12         X6=U(G'+G'')=Z'
          DX7       X5+X7    13 X3      X7=L(G'+G'')=Z''
          SA5       A5+JY    14         X5=Y''(3+K)
          FX3       X3*X0    15 X6      X3=U(X''*S')
 +        SA6       A6+JZ    16 X7      Z'(2+K)=...
          SA7       A7+JZ    17 X2      Z''(2+K)=...
          FX6       X2*X1    18 X4      X6=U(X'*S'')
*                            19 X3
          FX7       X2*X0    20         X7=U(X'*S')=A'
*                            21 X5
 +        DX2       X2*X0    22 X6      X2=L(X'*S')=A''
*                            23
          FX3       X3+X6    24 X7      X3=U(X''*S'+X'*S'')=B'
          DX6       X7_#_X4  25         X6=L(A'#Y')=C''
          FX4       X7_#_X4  26 X2      X4=U(A'#Y')=C'
 +        S_J       J+1      27 X3                          J=2-N+1+K
          FX3       X2+X3    28 X6      X3=U(A''+B')=D'
          FX7       X6_#_X5  29 X4      X7=U(C''#Y'')=E'
*                            30
*                            31 X3
 +        MI        J,[1]    32 X7      WHILE 3-N+K<0; I.E. 4+K .LE. N
 DSCA1R   ENDM
 DVSCA    MACRO     N,JX,#,JY,Z,JZ,J    X4=(X) X2=(Y) X1=S
          LOCAL     [1],[2],[99]
          SA3       X4+1                X3=X''(1)
          SA5       X2+1                X5=Y''(1)
          SA2       X4                  X2=X'(1)
          SA4       A5-1                X4=Y'(1)
          S_J       -N
          FX3       X3*X0               X3=U(X''(1)*S')
          S_J       J+2                                     J=2-N
          FX6       X2*X1               X6=U(X'(1)*S'')
          FX7       X2*X0               X7=U(X'(1)*S')=A'(1)
          DX2       X2*X0               X2=L(X'(1)*S')=A''(1)
          FX3       X3+X6               X3=U(X''*S'+X'*S'')=B'
          DX6       X7_#_X4             X6=L(A'(1)#Y'(1))=C''
          FX4       X7_#_X4             X4=U(A'(1)#Y'(1))=C'
          FX3       X2+X3               X3=U(A''+B')=D'(1)
          FX7       X6_#_X5             X7=U(C''(1)#Y''(1))=E'(1)
          FX5       X3+X7               X5=U(D'(1)+E'(1))=F'(1)
          FX6       X4+X5               X6=U(C'(1)+F'(1))=G'(1)
          DX7       X4+X5               X7=L(C'(1)+F'(1))=G''(1)
          LE        J,B0,[1]            IF N = 2,3,...
          NX5       X6                  X5=G'(1)
          NX7       X7
          FX6       X5+X7               X6=U(G'(1)+G''(1))
          DX7       X5+X7               X7=L(G'(1)+G''(1))
          SA6       Z                   Z'(1)=U(G'+G'')
          SA7       Z+1                 Z''(1)=L(G'+G'')
          JP        [99]
 [1]      SA3       A3+JX               X3=X''(2)
          NX5       X6                  X5=G'(1)
          NX7       X7
          SA2       A2+JX               X2=X'(2)
          SA4       A4+JY               X4=Y'(2)
          FX6       X5+X7               X6=U(G'(1)+G''(1))
          DX7       X5+X7               X7=L(G'(1)+G''(1))
          SA5       A5+JY               X5=Y''(2)
          FX3       X3*X0               X3=U(X''(2)*S')
          SA6       Z                   Z'(1)=U(G'+G'')
          SA7       Z+1                 Z''(1)=L(G'+G'')
          FX6       X2*X1               X6=U(X'(2)*S'')
          FX7       X2*X0               X7=U(X'(2)*S')=A'(2)
          DX2       X2*X0               X2=L(X'(2)*S')=A''(2)
          FX3       X3+X6               X3=U(X''*S'+X'*S'')=B'
          DX6       X7_#_X4             X6=L(A'(2)#Y'(2))=C''
          FX4       X7_#_X4             X4=U(A'(2)#Y'(2))=C'
          FX3       X2+X3               X3=U(A''+B')=D'(2)
          FX7       X6_#_X5             X7=U(C''(2)#Y''(2))=E'(2)
          PL        J,[2]               IF N=2
          DSCA1R    JX,#,JY,JZ,J                            J=0
 [2]      FX5       X3+X7               X5=U(D'(N)+E'(N))=F'(N)
          FX6       X4+X5               X6=U(C'(N)+F'(N))=G'(N)
          DX7       X4+X5               X7=L(C'(N)+F'(N))=G''(N)
          NX5       X6                  X5=G'(N)
          NX7       X7
          FX6       X5+X7               X6=U(G'(N)+G''(N))
          DX7       X5+X7               X7=L(G'(N)+G''(N))
          SA6       A6+JZ               Z'(N)=U(G'+G'')
          SA7       A7+JZ               Z''(N)=L(G'+G'')
 [99]     BSS       0
 DVSCA    ENDM
 CSCA1R   MACRO     JX,#,JY,JZ,J        X0=S'  X1=S''
          LOCAL     [1]
 [1]      BSS       0                   FOR K = 0, 1, 2, ...
 +        RX4       X2*X0     0         X4=X'(3+K)*S'
          NX6       X6        1
          RX5       X3*X1     2         X5=X''(3+K)*S''
          NX7       X7        3 X6
 +        RX2       X2*X1     4 X4      X2=X'(3+K)*S''
          SA6       A6+JZ     5 X7      Z'(2+K)=X'*S'-X''*S''#Y'
          RX3       X3*X0     6 X5      X3=X''(3+K)*S'
          SA7       A7+JZ     7         Z''(2+K)=X'*S''+X''*S'#Y''
 +        RX6       X4-X5     8 X2      X6=X'*S'-X''*S''
          SA4       A4+JY     9         X4=Y'(3+K)
          SA5       A5+JY    10 X3      X5=Y''(3+K)
          RX7       X2+X3    11 X6      X7=X'*S''+X''*S'
 +        SA2       A2+JX    12         X2=X'(4+K)
          NX6       X6       13
          SA3       A3+JX    14 X7      X3=X''(4+K)
*                            15 X6
          NX7       X7       16 X4
 +        RX6       X6_#_X4  17 X5      X6=X'*S'-X''*S''#Y'(3+K)
          S_J       J+1      18 X7                          J=3-N+1+K
          RX7       X7_#_X5  19 X2      X7=X'*S''+X''*S'#Y''(3+K)
*                            20 X6
*                            21 X3
 +        MI        J,[1]    22 X7      WHILE 4-N+K<0; I.E. 5+K .LE. N
 CSCA1R   ENDM
 CVSCA    MACRO     N,JX,#,JY,Z,JZ,J    X4=(X) X2=(Y) X1=S
          LOCAL     [1],[2],[3],[99]
          SA3       X4+1                X3=X''(1) A3=(X)+1
          SA4       X2                  X4=Y'(1)  A4=(Y)
          S_J       -N
          SA2       A3-1                X2=X'(1)  A2=(X)
          S_J       J+2                                     J=2-N
          RX6       X2*X0               X6=X'(1)*S'
          RX5       X3*X1               X5=X''(1)*S''
          RX2       X2*X1               X2=X'(1)*S''
          RX3       X3*X0               X3=X''(1)*S'
          RX6       X6-X5               X6=X'(1)*S'-X''(1)*S''
          SA5       A4+1                X5=Y''(1) A5=(Y'')
          RX7       X2+X3               X7=X'(1)*S''+X''(1)*S'
          LE        J,B0,[1]            IF N = 2,3,...
          NX6       X6
          NX7       X7
          RX6       X6_#_X4             X6=X'*S'-X''*S'' # Y'
          RX7       X7_#_X5             X7=X'*S''+X''*S' # Y''
          NX6       X6
          NX7       X7
          SA6       Z                   Z'(1)=...
          SA7       Z+1                 Z''(1)=...
          JP        [99]
 [1]      SA2       A2+JX               X2=X'(2)  A2=(X'(2))
          NX6       X6
          SA3       A3+JX               X3=X''(2) A3=(X''(2))
          NX7       X7
          RX6       X6_#_X4             X6=X'(1)*S'-X''(1)*S'' # Y'(1)
          RX7       X7_#_X5             X7=X'(1)*S''+X''(1)*S' # Y''(1)
          RX4       X2*X0               X4=X'(2)*S'
          NX6       X6
          RX5       X3*X1               X5=X''(2)*S''
          NX7       X7
          RX2       X2*X1               X2=X'(2)*S''
          SA6       Z                   Z'(1)=...
          RX3       X3*X0               X3=X''(2)*S'
          SA7       Z+1                 Z''(1)=...
          RX6       X4-X5               X6=X'(2)*S'-X''(2)*S''
          SA4       A4+JY               X4=Y'(2)  A4=(Y'(2))
          SA5       A5+JY               X5=Y''(2) A5=(Y''(2))
          RX7       X2+X3               X7=X'(2)*S''+X''(2)*S'
          ZR        J,[3]               IF N=2
          SA2       A2+JX               X2=X'(3)  A2=(X'(3))
          NX6       X6
          SA3       A3+JX               X3=X''(3) A3=(X''(3))
          NX7       X7
          RX6       X6_#_X4             X6=X'(2)*S'-X''(2)*S'' # Y'(2)
          S_J       J+1                                     J=3-N
          RX7       X7_#_X5             X7=X'(2)*S''+X''(2)*S' # Y''(2)
          PL        J,[2]               IF N=3
          CSCA1R    JX,#,JY,JZ,J                            J=0
 [2]      RX4       X2*X0               X4=X'(N)*S'
          NX6       X6
          RX5       X3*X1               X5=X''(N)*S''
          NX7       X7
          RX2       X2*X1               X2=X'(N)*S''
          SA6       A6+JZ               Z'(N-1)=...
          RX3       X3*X0               X3=X''(N)*S'
          SA7       A7+JZ               Z''(N-1)=...
          RX6       X4-X5               X6=X'(N)*S'-X''(N)*S''
          SA4       A4+JY               X4=Y'(N)  A4=(Y'(N))
          SA5       A5+JY               X5=Y''(N) A5=(Y''(N))
          RX7       X2+X3               X7=X'(N)*S''+X''(N)*S'
 [3]      NX6       X6
          NX7       X7
          RX6       X6_#_X4             X6=X'*S'-X''*S'' # Y'
          RX7       X7_#_X5             X7=X'*S''+X''*S' # Y''
          NX6       X6
          NX7       X7
          SA6       A6+JZ               Z'(N)=...
          SA7       A7+JZ               Z''(N)=...
 [99]     BSS       0
 CVSCA    ENDM
*CD       SCL
 RSCL2R   MACRO     JX,JY,J
          LOCAL     [1]
 [1]      BSS       0                   FOR K = 0, 2, 4,...
 +        S_J       J+2       0                             J=5-N+2+K
          RX6       X1*X4     1         X6=S*X(3+K)
          SA4       A5+JX     2 X5      X4=X(5+K)
 +        SA7       A6+JY     3         Y(2+K)=S*X(2+K)
          RX7       X1*X5     4         X7=S*X(4+K)
          SA5       A4+JX     5 X6      X5=X(6+K)
          SA6       A7+JY     6         Y(3+K)=S*X(3+K)
                              7
                              8 X7
 +        MI        J,[1]     9 X4      WHILE 7-N+K=J<0; I.E. 8+K.LE.N
 RSCL2R   ENDM
 RSCL4T   MACRO     JX,JY               FOR K = 0, 4, 8, ...
          RX6       X1*X2     1         X6=S*X(3+K)
          SA2       A5+JX     2 X7      X2=X(7+K)
          SA7       A6+JY     3 X4      Y(2+K)=S*X(2+K)
          RX7       X1*X3     4         X7=S*X(4+K)
          SA3       A2+JX     5 X6      X3=X(8+K)
          SA6       A7+JY     6 X5      Y(3+K)=S*X(3+K)
          RX6       X1*X4     7         X6=S*X(5+K)
          SA4       A3+JX     8 X7      X4=X(9+K)
          SA7       A6+JY     9 X2      Y(4+K)=S*X(4+K)
          RX7       X1*X5    10         X7=S*X(6+K)
          SA5       A4+JX    11 X6      X5=X(10+K)
          SA6       A7+JY    12 X3      Y(5+K)=S*X(5+K)
 RSCL4T   ENDM
 RSCL12R  MACRO     JX,JY,J
          LOCAL     [1]
 [1]      BSS       0                   FOR K = 0, 12, 24, ...
          S_J       J+12                                    J=17-N+12+K
          DUP       3
          RSCL4T    JX,JY
          ENDD
          MI        J,[1]               WHILE 29-N+K=J<0; I.E. 30+K.LE.N
 RSCL12R  ENDM
 RVSCL    MACRO     N,X,JX,Y,JY,J      X1=S
          LOCAL     [2],[3],[4],[5],[6],[7],[8],[99]
          BRANCH    [4],N,LE,17,J                           J=17-N
          SA4       X         0         X4=X(1)
          SA5       X+JX      1         X5=X(2)
                              2
          SA2       A5+JX     3         X2=X(3)
                              4
          SA3       A2+JX     5         X3=X(4)
                              6
                              7 X4
                              8 X5
          RX6       X1*X4     9         X6=S*X(1)
          SA4       A3+JX    10 X2      X4=X(5)
          RX7       X1*X5    11         X7=S*X(2)
          SA5       A4+JX    12 X3      X5=X(6)
          SA6       Y                   Y(1)=S*X(1)
          RSCL12R   JX,JY,J                          J=11,10,9,...,1,0
          S_J       J-10                             J=1,0,-1,...-9,-10
          RX6       X1*X2               X6=S*X(J+N-4)
          SA7       A6+JY
          RX7       X1*X3               X7=S*X(J+N-3)
          SA6       A7+JY
          ZR        J,[3]               IF ONE TERM LEFT
          PL        J,[8]               IF NO TERM LEFT
 [2]      RSCL2R    JX,JY,J   8 X7                          J=1,0
                              9 X4
          NZ        J,[8]    10         IF NO TERM LEFT
 [3]      SA3       A5+JX    11         X3=X(N)
          SA7       A6+JY    12 X5      Y(N-3)=X(N-3)
          RX7       X1*X4    13         X7=S*X(N-2)
                             14
          RX6       X1*X5    15         X6=S*X(N-1)
                             16
                             17 X7
          SA7       A7+JY    18 X3      Y(N-2)=S*X(N-2)
          RX7       X1*X3    19 X6      X7=S*X(N)
          SA6       A7+JY    20         Y(N-1)=S*X(N-1)
          SA5       A3       21                   A5=(X(N))
                             22
                             23 X7
          SA7       A6+JY    24         Y(N)=S*X(N)
                             25
                             26
          JP        [99]     27
 [4]      BSS       0                              J=16,15,14,13,...1,0
          SX6       N-1                 X6=N-1
          SA2       X                   X2=X(1)
          ZR        X6,[5]              IF N=1
          SA3       X+JX                X3=X(2)
          S_J       J-14                            J=1,0,-1,...,-13,-14
          ZR        J,[7]               IF N=3
          RX6       X1*X2               X6=S*X(1)
          PL        J,[6]               IF N=2
          SA4       A3+JX               X4=X(3)
          RX7       X1*X3               X7=S*X(2)
          SA5       A4+JX               X5=X(4)
          S_J       J+2                             J=1,0,-1,...,-11,-12
          SA6       Y                   Y(1)=S*X(1)
          MI        J,[2]               IF N=6,7,...16,17
          ZR        J,[3]               IF N=5
          JP        [8]                 IF N=4
 [5]      BSS       0
          RX7       X1*X2               X7=S*X(1)
          SA5       A2                            A5=(X(1))
          NO
          SA7       Y                   Y(1)=S*X(1)
 +        JP        [99]
 [6]      BSS       0                   X6=S*X(1)
                                        X3=X(2)
          RX7       X1*X3               X7=S*X(2)
          SA6       Y                   Y(1)=S*X(1)
          SA5       A3                            A5=(X(2))
          SA7       A6+JY               Y(2)=S*X(2)
          JP        [99]
 [7]      BSS       0                   X2=X(1)
                                        X3=X(2)
          SA5       A3+JX     0         X5=X(3)
          RX7       X1*X2     1         X7=S*X(1)
                              2
                              3
          RX6       X1*X3     4         X6=S*X(2)
                              5 X7
          SA7       Y         6         Y(1)=S*X(1)
                              7 X5
          RX7       X1*X5     8 X6      X7=S*X(3)
          SA6       A7+JY     9         Y(2)=S*X(2)
          NO                 10
                             11
                             12 X7
          SA7       A6+JY    13         Y(3)=S*X(3)
                             14
                             15
          JP        [99]     16
 [8]      BSS       0
          RX6       X1*X4     0         X6=S*X(N-1)
          SA7       A6+JY     1         Y(N-2)=S*X(N-2)
          RX7       X1*X5     2         X7=S*X(N)
          NO                  3
 +        SB0       0         4 X6
          SA6       A7+JY     5         Y(N-1)=S*X(N-1)
                              6 X7
          SA7       A6+JY     7         Y(N)=S*X(N)
 [99]     BSS       0
 RVSCL    ENDM
 RMSCL    MACRO     M,N,X,IX,JX,Z,IZ,JZ,J   (X=X0, Z=A0)  X1=S
          LOCAL     [1],[2],[3],[4],[5],[5A],[6],[6A],[7],[8],[9],[99]
          LOCAL     SAVEM
          LE        M,N,[2]             IF M .LE. N
 [1]      SWAP      M,N                 EXCHANGE M  AND N
          SWAP      IX,JX               EXCHANGE IX AND JX
          SWAP      IZ,JZ               EXCHANGE IZ AND JZ
 [2]      BRANCH    [4],N,LE,17,J       J=17-N
          SX7       M                   X7=M
          SA7       SAVEM               SAVEM = M
 [3]      BSS       0                   FOR I = 1,2,...,M
          SA4       X                   X4=X(I,1) A4=(X(I,1))
          SA5       X+JX                X5=X(I,2) A5=(X(I,2))
          S_J       -N                                      J=-N
          SA2       A5+JX               X2=X(I,3) A2=(X(I,3))
          S_J       J+17                                    J=17-N
          SA3       A2+JX               X3=X(I,4) A3=(X(I,4))
          RX6       X1*X4               X6=S*X(I,1)
          SA4       A3+JX               X4=X(I,5) A4=(X(I,5))
          RX7       X1*X5               X7=S*X(I,2)
          SA5       A4+JX               X5=X(I,6) A5=(X(I,6))
          SA6       Z                   Z(I,1)=S*X(I,1)
          RSCL12R   JX,JZ,J                           J=11,10,9,...,1,0
          RX6       X1*X2               X6=S*X(I,N+J-14)
          SA7       A6+JZ               Z(I,N+J-15)=S*X(I,N+J-15)
          RX7       X1*X3               X7=S*X(I,N+J-13)
          S_Z       Z+IZ                          Z=(Z(I+1,1))
          SA6       A7+JZ               Z(I,N+J-14)=S*X(I,N+J-14)
          RX6       X1*X4               X6=S*X(I,N+J-12)
          SA7       A6+JZ               Z(I,N+J-13)=S*X(I,N+J-13)
          RX7       X1*X5               X7=S*X(I,N+J-11)
          S_M       M-1                                     M=M-1
          SA6       A7+JZ               Z(I,N+J-12)=S*X(I,N+J-12)
          S_X       X+IX                          X=(X(I+1,1))
          SA7       A6+JZ               Z(I,N+J-11)=S*X(I,N+J-11)
          NZ        M,[3]               WHILE I<M
          BRANCH    [99],J,EQ,11,N                          N=11-J
          SA4       SAVEM               X4=M
          RESIDU    A5,X,IX,JX,A7,Z,IZ,JZ
          S_M       X4                                      M=M
          GT        M,N,[1]             IF M>N
 [4]      BSS       0                   1 .LE. M .LE. N .LE. 17
          BRANCH    [8],N,GT,3,J                            J=3-N
          SA3       X                   X3=X(1,1) A3=(X(1,1))
          ZR        J,[6]               IF N=3
          S_J       J-1                                     J=2-N
          ZR        J,[5]               IF N=2
          RX7       X1*X3               X7=S*X(1,1)
          SA7       Z                   Z(1,1)=S*X(1,1)
          JP        [99]
 SAVEM    BSS       1                   SAVE OF M
 [5]      SA4       X+JX                X4=X(1,2)
 [5A]     RX6       X1*X3               X6=S*X(I,1)
          S_M       M-1                                     M=M-1
          RX7       X1*X4               X7=S*X(I,2)
          S_X       X+IX                          X=(X(I+1,1))
          SA6       Z                   Z(I,1)=S*X(I,1)
          S_Z       Z+IZ                          Z=(Z(I+1,1))
          SA7       A6+JZ               Z(I,2)=S*X(I,2)
          ZR        M,[99]              IF I=M
          SA3       X                   X3=X(I+1,1)
          SA4       X+JX                X4=X(I+1,2)
          JP        [5A]
 [6]      SA4       X+JX                X4=X(1,2)
          SA5       A4+JX               X5=X(1,3)
 [6A]     RX6       X1*X3               X6=S*X(I,1)
          S_M       M-1                                     M=M-1
          RX7       X1*X4               X7=S*X(I,2)
          SA6       Z                   Z(I,1)=S*X(I,1)
          RX6       X1*X5               X6=S*X(I,3)
          SA7       A6+JZ               Z(I,2)=S*X(I,2)
          S_X       X+IX                          X=(X(I+1,1))
          S_Z       Z+IZ                          Z=(Z(I+1,1))
          SA6       A7+JZ               Z(I,3)=S*X(I,3)
          ZR        M,[99]              IF I=M
          SA3       X                   X3=X(I+1,1)
          SA4       X+JX                X4=X(I+1,2)
          NO
          SA5       A4+JX               X5=X(I+1,3)
          JP        [6A]
 [7]      RX6       X1*X4               X6=S*X(I,N-2)
          SA4       A5+JX               X4=X(I,N)
          SA7       A6+JZ               Z(I,N-3)=S*X(I,N-3)
          RX7       X1*X5               X7=S*X(I,N-1)
          SA6       A7+JZ               Z(I,N-2)=S*X(I,N-2)
          RX6       X1*X4               X6=S*X(I,N)
          SA7       A6+JZ               Z(I,N-1)=S*X(I,N-1)
          SA6       A7+JZ               Z(I,N)=S*X(I,N)
          ZR        M,[99]              IF I=M
 [8]      SA3       X                   X3=X(I,1) A3=(X(I,1))
          SA5       X+JX                X5=X(I,2) A5=(X(I,2))
          S_J       -N                                      J=-N
          SA4       A5+JX               X4=X(I,3) A4=(X(I,3))
          S_J       J+5                                     J=5-N
          S_M       M-1                                     M=M-1
          RX6       X1*X3               X6=S*X(I,1)
          S_X       X+IX                          X=(X(I+1,1))
          RX7       X1*X5               X7=S*X(I,2)
          SA5       A4+JX               X5=X(I,4) A5=(X(I,4))
          SA6       Z                   Z(I,1)=S*X(I,1)
          S_Z       Z+IZ                          Z=(Z(I+1,1))
          ZR        J,[7]               IF N=5
          PL        J,[9]               IF N=4
          RSCL2R    JX,JZ,J                                 J=1 OR 0
          ZR        J,[7]               IF N IS ODD
 [9]      RX6       X1*X4               X6=S*X(I,N-1)
          SA7       A6+JZ               Z(I,N-2)=S*X(I,N-2)
          RX7       X1*X5               X7=S*X(I,N)
          SA6       A7+JZ               Z(I,N-1)=S*X(I,N-1)
          SA7       A6+JZ               Z(I,N)=S*X(I,N)
          NZ        M,[8]               WHILE I<M
 [99]     BSS       0
 RMSCL    ENDM
 DSCL1R   MACRO     JX,JY,J
          LOCAL     [1]
 [1]      BSS       0                   FOR K = 0, 1, 2, ...
 +        DX7       X5+X6     0         X7=Y''(2+K)
          FX6       X5+X6     1 X3      X6=Y'(2+K)
          FX0       X2*X3     2 X4      X0=S''*X'(3+K)
*                             3 X7
          DX5       X1*X3     4 X6      X5=L(S'*X'(3+K))=A''(3+K)
 +        SA7       A7+JY     5         Y''(2+K)
          FX7       X1*X4     6 X0      X7=S'*X''(3+K)
          S_J       J+1       7                             J=3-N+1+K
 +        SA6       A6+JY     8 X5      Y'(2+K)
          FX5       X5+X0     9         X5=A''(3+K)+S''*X'(3+K)
          FX6       X1*X3    10 X7      X6=U(S'*X'(3+K))=A'(3+K)
          SA3       A3+JX    11         X3=X'(4+K)
 +        SA4       A4+JX    12 X5      X4=X''(4+K)
          FX5       X5+X7    13         X5=A''(3+K)+S''*X'+S'*X''
*                            14 X6
*                            15
          MI        J,[1]    16 X5      WHILE 4-N+K=J<0; I.E. 5+K.LE.N
 DSCL1R   ENDM
 DVSCL    MACRO     N,X,JX,Y,JY,J      X1=S', X2=S''
*                                       X=X0, X4, X5, X6, X7
          LOCAL     [1],[2],[3],[99]
          S_J       1                                       J=1
          SA3       X                   X3=X'(1)  A3=(X'(1))
          SA4       X+J                 X4=X''(1) A4=(X''(1))
          S_J       J-N                                     J=1-N
          NZ        J,[1]               IF N>1 GO TO [1]
          FX0       X2*X3               X0=S''*X'
          DX5       X1*X3               X5=L(S'*X')
          FX7       X1*X4               X7=S'*X''
          FX5       X5+X0               X5=L(S'*X')+S''*X'
          FX6       X1*X3               X6=U(S'*X')
          FX5       X5+X7               X5=L(S'*X')+S''*X'+S'*X''
          DX7       X5+X6               X7=Y''
          FX6       X5+X6               X6=Y'
          SA7       Y+1
          SA6       Y
          JP        [99]
 [1]      FX0       X2*X3               X0=S''*X'(1)
          S_J       J+2                                     J=3-N
          DX5       X1*X3               X5=L(S'*X'(1))
          FX7       X1*X4               X7=S'*X''(1)
          FX5       X5+X0               X5=L(S'*X'(1))+S''*X'(1)
          FX6       X1*X3               X6=U(S'*X'(1))
          SA3       A3+JX               X3=X'(2)  A3=(X'(2))
          SA4       A4+JX               X4=X''(2) A4=(X''(2))
          FX5       X5+X7             X5=L(S'*X'(1))+S''*X'(1)+S'*X''(1)
          DX7       X5+X6               X7=Y''(1)
          FX6       X5+X6               X6=Y'(1)
          FX0       X2*X3               X0=S''*X'(2)
          DX5       X1*X3               X5=L(S'*X'(2))
          SA7       Y+1                 Y''(1)    A7=(Y''(1))
          FX7       X1*X4               X7=S'*X''(2)
          SA6       Y                   Y'(1)     A6=(Y'(1))
          FX5       X5+X0               X5=L(S'*X'(2))+S''*X'(2)
          FX6       X1*X3               X6=U(S'*X'(2))
          GT        J,B0,[3]            IF N=2 GO TO [3]
          SA3       A3+JX               X3=X'(3)  A3=(X'(3))
          SA4       A4+JX               X4=X''(3) A4=(X''(3))
          FX5       X5+X7             X5=L(S'*X'(2))+S''*X'(2)+S'*X''(2)
          ZR        J,[2]               IF N=3 GO TO [2]
          DSCL1R    JX,JY,J                                 J=0
 [2]      DX7       X5+X6               X7=Y''(N-1)
          FX6       X5+X6               X6=Y'(N-1)
          FX0       X2*X3               X0=S''*X'(N)
          DX5       X1*X3               X5=L(S'*X'(N))
          SA7       A7+JY               Y''(N-1)  A7=(Y''(N-1))
          FX7       X1*X4               X7=S'*X''(N)
          SA6       A6+JY               Y'(N-1)   A6=(Y'(N-1))
          FX5       X5+X0               X5=L(S'*X'(N))+S''*X'(N)
          FX6       X1*X3               X6=U(S'*X'(N))
 [3]      FX5       X5+X7             X5=L(S'*X'(N))+S''*X'(N)+S'*X''(N)
          SB0       0
          DX7       X5+X6               X7=Y''(N)
          FX6       X5+X6               X6=Y'(N)
          SA7       A7+JY               Y''(N)    A7=(Y''(N))
          SA6       A6+JY               Y'(N)     A6=(Y'(N))
 [99]     BSS       0
 DVSCL    ENDM
 CSCL1R   MACRO     JX,JY,J
          LOCAL     [1]
 [1]      BSS       0                   FOR K = 0, 1, 2, ...
 +        RX7       X7+X0     0         X7=S''*X'(2+K)+S'*X''(2+K)
          S_J       J+1       1 X5                          J=3-N+1+K
          NX6       X6        2 X4      X6=Y'(2+K)
 +        RX3       X2*X5     3 X7      X3=S''*X''(3+K)
          NX7       X7        4 X6      X7=Y''(2+K)
          RX0       X1*X5     5         X0=S'*X''(3+K)
          SA6       A6+JY     6 X7      Y'(2+K)
 +        RX6       X1*X4     7 X3      X6=S'*X'(3+K)
          SA7       A7+JY     8         Y''(2+K)
          RX7       X2*X4     9 X0      X7=S''*X'(3+K)
          SA5       A5+JX    10         X5=X''(4+K)
 +        SA4       A4+JX    11 X6      X4=X'(4+K)
          RX6       X6-X3    12         X6=S'*X'(3+K)-S''*X''(3+K)
*                            13 X7
*                            14
          MI        J,[1]    15 X6      WHILE 4-N+K<0; I.E. 5+K.LE.N
 CSCL1R   ENDM
 CVSCL    MACRO     N,X,JX,Y,JY,J      X1=S' X2=S''
*                                       X = X0, X3, X4, X6, X7
          LOCAL     [1],[2],[3],[99]
          S_J       1                                       J=1
          SA5       X+1                 X5=X''(1) A5=(X''(1))
          S_J       J-N                                     J=1-N
          SA4       X                   X4=X'(1)  A4=(X'(1))
          NZ        J,[1]               IF N>1 GO TO [1]
          RX3       X2*X5               X3=S''*X''(1)
          RX0       X1*X5               X0=S'*X''(1)
          RX6       X1*X4               X6=S'*X'(1)
          RX7       X2*X4               X7=S''*X'(1)
          RX6       X6-X3               X6=S'*X'(1)-S''*X''(1)
          RX7       X7+X0               X7=S''*X'(1)+S'*X''(1)
          NX6       X6                  X6=Y'(1)
          NX7       X7                  X7=Y''(1)
          SA6       Y                   Y'(1)
          SA7       Y+1                 Y''(1)
          JP        [99]
 [1]      RX3       X2*X5               X3=S''*X''(1)
          S_J       J+2                                     J=3-N
          RX0       X1*X5               X0=S'*X''(1)
          SA5       A5+JX               X5=X''(2) A5=(X''(2))
          RX6       X1*X4               X6=S'*X'(1)
          RX7       X2*X4               X7=S''*X'(1)
          SA4       A4+JX               X4=X'(2)  A4=(X'(2))
          RX6       X6-X3               X6=S'*X'(1)-S''*X''(1)
          RX7       X7+X0               X7=S''*X'(1)+S'*X''(1)
          NX6       X6                  X6=Y'(1)
          RX3       X2*X5               X3=S''*X''(2)
          NX7       X7                  X7=Y''(1)
          RX0       X1*X5               X0=S'*X''(2)
          SA6       Y                   Y'(1)
          RX6       X1*X4               X6=S'*X'(2)
          SA7       Y+1                 Y''(1)
          RX7       X2*X4               X7=S''*X'(2)
          GT        J,B0,[3]            IF N=2 GO TO [3]
          SA5       A5+JX               X5=X''(3) A5=(X''(3))
          SA4       A4+JX               X4=X'(3)  A4=(X'(3))
          RX6       X6-X3               X6=S'*X'(2)-S''*X''(2)
          ZR        J,[2]               IF N=3 GO TO [2]
          CSCL1R    JX,JY,J
 [2]      RX7       X7+X0               X7=S''*X'(N-1)+S'*X''(N-1)
          NX6       X6                  X6=Y'(N-1)
          RX3       X2*X5               X3=S''*X''(N)
          NX7       X7                  X7=Y''(N-1)
          RX0       X1*X5               X0=S'*X''(N)
          SA6       A6+JY               Y'(N-1)
          RX6       X1*X4               X6=S'*X'(N)
          SA7       A7+JY               Y''(N-1)
          RX7       X2*X4               X7=S''*X'(N)
 [3]      RX6       X6-X3               X6=S'*X'(N)-S''*X''(N)
          RX7       X7+X0               X7=S''*X'(N)+S'*X''(N)
          SB0       0
          NO
          NX6       X6                  X6=Y'(N)
          NX7       X7                  X7=Y''(N)
          SA6       A6+JY               Y'(N)
          SA7       A7+JY               Y''(N)
 [99]     BSS       0
 CVSCL    ENDM
 TRSCL    MACRO     TYPE,M,N,D,ID,JX,JZ,J,TEMP X0=(X),X6=IX,A0=(Z),X7=IZ
          LOCAL     [1],SAVEXI1,SAVEIX,SAVEIZ
 SAVEXI1  EQU       TEMP
 SAVEIX   EQU       TEMP+1
 SAVEIZ   EQU       TEMP+2
          SA6       SAVEIX              SAVE IX
          SA7       SAVEIZ              SAVE IZ
 [1]      SA1       D                   X1=D(I)   A1=(D(I))
 TAU      IFNE      TYPE,RTYPE
          SA2       D+1                 X2=D(I)'' A2=(D(I)'')
          BX6       X0                  X6=(X(I,1))
          SA6       SAVEXI1
 TAU      ENDIF
          "P"_VSCL  N,X0,JX,A0,JZ,J     X0 IS SAVED ONLY BY RVSCL
          SA3       SAVEIX              X3=IX
          SA4       SAVEIZ              X4=IZ
 TAU      IFNE      TYPE,RTYPE
          SA5       SAVEXI1             X5=(X(I,1))
 TAU      ENDIF
          S_M       M-1                                     M=M-1
          S_J       A0                                      J=(Z(I,1))
          S_D       D+ID                                    D=(D(I+1))
 RHO      IFEQ      TYPE,RTYPE          X0=(X(I+1,1))
          IX0       X0+X3
 RHO      ELSE
          IX0       X5+X3
 RHO      ENDIF
          SA0       X4+J                          A0=(Z(I+1,1))
          NZ        M,[1]               IF(M.NE.0) REPEAT
 TRSCL    ENDM
*CD       SET
 RSET2    MACRO     JZ,J                X6=S,     J=2-N
          LOCAL     [1]
 [1]      BSS       0                   FOR K = 0, 2, 4,...
          SA6       A6+JZ     0         Z(2+K)=S
          S_J       J+2       1                             J=2-N+K+2
          SA6       A6+JZ     2         Z(3+K)=S
*                             3
*                             4
 +        MI        J,[1]     5         WHILE 4-N+K<0; I.E. 5+K .LE. N
 RSET2    ENDM                                              J=0 OR 1
 RSET8    MACRO     JZ,J                X6=X7=S,            J=8-N
          LOCAL     [1]
 [1]      BSS       0                   FOR K = 0, 8, 16,...
          S_J       J+8                                     J=8-N+K+8
          SA6       A6+JZ               Z(2+K)=S
          SA7       A7+JZ               Z(3+K)=S
          SA6       A6+JZ               Z(4+K)=S
          SA7       A7+JZ               Z(5+K)=S
          SA6       A6+JZ               Z(6+K)=S
          SA7       A7+JZ               Z(7+K)=S
          SA6       A6+JZ               Z(8+K)=S
          SA7       A7+JZ               Z(9+K)=S
*                             9
*                            10
          MI        J,[1]    11         WHILE 16-N+K<0; I.E. 17+K.LE.N
 RSET8    ENDM                          J = 7,6,...,0 FOR N=9,10,...,16
 RVSET2   MACRO     N,Z,JZ,J            X6=S,     N>0
          LOCAL     [1],[99]
          S_J       2
          SA6       Z                   Z(1)=S
          S_J       J-N                 J=2-N=+1,0,-1,-2,...
          PL        J,[1]               IF N=1 (J=1) OR N=2 (J=0)
          RSET2     JZ,J                J=1 OR 0 (FOR N ODD OR EVEN)
 [1]      NZ        J,[99]              IF N IS ODD
          SA6       A6+JZ               Z(N)=S IF N IS EVEN
          NO
 [99]     BSS       0                   N,Z,JZ,X6 UNALTERED
 RVSET2   ENDM
 RVSET    MACRO     N,Z,JZ,J            X6=S,     N>0
          LOCAL     [1],[2],[3],[99]
          S_J       9
          SA6       Z                   Z(1)=S
          S_J       J-N                 J=8,7,6,... FOR N=1,2,3,...
          BX7       X6                  X7=S
          MI        J,[1]               IF N=10,11,...
          S_J       J-8                 J=1-N=0,-1,-2,... FOR N=1,2,3,
          MI        J,[2]               IF 1<N<10
          JP        [99]                IF N=1
 [1]      SA7       Z+JZ                Z(2)=S
          S_JZ      JZ+JZ                              JZ=(Z(3))-(Z(1))
          RSET8     JZ,J                J=-1,-2,-3,... FOR N=10,11,12,
          SX7       JZ                  X7=2*JZ
          S_J       J-6                 J=+1,0,-1,-2,...,-6
          AX7       1
          S_JZ      X7                  RESTORE JZ
          GT        J,B0,[99]           IF N=10,18,...
          SA6       A7+JZ               Z(11+K)=S
          ZR        J,[99]              IF N=11,19,27,...
 [2]      S_J       J+1
          ZR        J,[3]               IF N=12,20,28,...
          RSET2     JZ,J                J=+1 OR 0
 [3]      NZ        J,[99]
          SA6       A6+JZ
 [99]     BSS       0
 RVSET    ENDM
 RMSET    MACRO     M,N,Z,IZ,JZ,I,J     X6=S, Z=BI
          LOCAL     [1],[2],[3],[4],[5],[6],[99]
          LE        M,N,[2]             IF M .LE. N
 [1]      SX7       M                   EXCHANGE M AND N
          S_M       N
          S_N       X7
          SX7       IZ                  EXCHANGE IZ AND JZ
          S_IZ      JZ
          S_JZ      X7
 [2]      BRANCH    [4],N,LE,9,J,I,M    J=9-N     I=-M
          BX7       X6                  X7=S
          SX5       JZ                  X5=JZ
          S_JZ      JZ+JZ                                   JZ=2*JZ
 [3]      BSS       0                   FOR I = 1,2,...,M
          S_J       -N
          SA6       Z                   Z(I,1)=S
          S_J       J+9                                     J=9-N
          SA7       Z+X5                Z(I,2)=S  A7=(Z(I,2))
          S_I       I+1                                     I=I+1
          S_Z       Z+IZ                          Z=(Z(I+1,1))
          RSET8     JZ,J                              J=7,6,...,1,0
          MI        I,[3]               WHILE I<M
          S_JZ      X5                  RESTORE JZ
          BRANCH    [99],J,EQ,7,N                           N=7-J
          RESIDU    A7,Z,IZ,JZ
          GT        M,N,[1]             IF M>N
 [4]      BSS       0                   1 .LE. M .LE. N .LE. 9
          BRANCH    [5],N,GT,2,J,I,M    J=2-N     I=-M
          SA6       Z                   Z(1,1)=S
          NZ        J,[99]              IF M=N=1
          S_I       I+1                 I=0 FOR M=1
          SA6       A6+JZ               Z(1,2)=S
          ZR        I,[99]              IF M=1
          SA6       Z+IZ                Z(2,1)=S
          SA6       A6+JZ               Z(2,2)=S
          JP        [99]
 [5]      S_J       -N
          SA6       Z                   Z(I,1)=S  A6=(Z(I,1))
          S_J       J+2                                     J=2-N
          RSET2     JZ,J                                    J=1,0
          S_I       I+1                                     I=1+1
 +        S_Z       Z+IZ                          Z=(Z(I+1,1))
          NZ        J,[6]               IF N IS ODD
          SA6       A6+JZ
 [6]      MI        I,[5]               WHILE I<M
 [99]     BSS       0
 RMSET    ENDM
*CD       SFACT
 TSFACT   MACRO     SAVEA,N,JA,TEMP5,A1K,AKK,IA,J,K    X1=(A)
          LOCAL     [1],[2],[99]
          LOCAL     SAVEN,IFAIL,FLOAT1,TEMP2
 SAVEN    EQU       TEMP5               N
 IFAIL    EQU       SAVEN+1             IFAIL
 FLOAT1   EQU       IFAIL+1             1.
 TEMP2    EQU       FLOAT1+1
*         IFAIL  =  0
          SX5       1
          SX6       N                   X6=N
          PX5       X5
          SA6       SAVEN               SAVEN=N
          NX6       X5                  X6=1.
          MX7       0                   X7=0
          SA6       FLOAT1              FLOAT1=1.
          SA7       IFAIL               IFAIL=0
          S_IA      LENGTH              IA=LENGTH
          S_K       B0                  K=0
          S_AKK     X1                  AKK=(A(1,1))
          S_A1K     X1                  A1K=(A(1,1))
          SX4       N                   X4=N
 [1]      BSS       0                   A1K=(A(1,K))        K=K-1
*                                       AKK=(A(K,K))
*                                       X4=N
          SA5       FLOAT1              X5=1.
          SA1       AKK                 X1=A(K,K)
 TAU      IFEQ      LENGTH,2
          SA2       AKK+1               X2=AKK''
 TAU      ENDIF
          S_N       X4                  RESTORE N
          S_J       K+1                 J=K
          ZR        X1,[2]              IF A(K,K) IS ZERO
          NG        X1,[2]                        OR NEGATIVE
          "P"RCPRCL                     AKK = X5/AKK
          SX5       B0                  X5=IFAIL=0
          EQ        J,N,[99]            IF K=N
*                                       K=K-1
*                                       X1=(A(1,1))
          TSFACT2   SAVEA,N,K,A1K,AKK,IA,JA,TEMP2,J
          SA4       SAVEN               X4=N
          S_J       IA+JA               J=IA+JA
          S_A1K     A1K+JA              A1K=(A(1,K+1))
          S_AKK     AKK+J               AKK=(A(K+1,K+1))
          JP        [1]
*         IFAIL  =  -1
 [2]      BSS       0
          SX5       -1                  X5=IFAIL=-1
 [99]     BSS       0                   X5=IFAIL
 TSFACT   ENDM
 TSFACTD  MACRO     SAVEA,N,JA,TEMP6L,A1K,AKK,I,J,K     X1=(A)
          LOCAL     [1],[2],[3],[99]
          LOCAL     SAVEN,IFAIL,DET,JFAIL,FLOAT1,TEMP2
 SAVEN    EQU       TEMP6L              N
 IFAIL    EQU       SAVEN+1             IFAIL
 DET      EQU       IFAIL+1             DET
 JFAIL    EQU       DET+LENGTH          JFAIL
 FLOAT1   EQU       JFAIL+1             1.
 TEMP2    EQU       FLOAT1+1
*         DET    =  ONE
*         JFAIL  =  0
*         IFAIL  =  0
          SX6       N                   X6=N
          MX7       0                   X7=0
          SA6       SAVEN               SAVEN=N
          SX5       1
          SA7       IFAIL               IFAIL=0
          PX6       X5
          SA7       JFAIL               JFAIL=0
          NX6       X6                  X6=1.
          SA6       FLOAT1              FLOAT1=1.
          SA6       DET                 DET=1.
 TAU      IFEQ      LENGTH,2
          SA7       DET+1               DET''=0.
 TAU      ENDIF
          S_K       B0                  K=0
          S_AKK     X1                  AKK=(A(1,1))
          S_A1K     X1                  A1K=(A(1,1))
          SX4       N                   X4=N
 [1]      BSS       0                   A1K=(A(1,K))        K=K-1
*                                       AKK=(A(K,K))
*                                       X4=N
          SA2       AKK                 X2=A(K,K) (UPPER FOR D.P.)
          SX1       AKK                 X1=(A(K,K))
          S_N       X4                  RESTORE N
          ZR        X2,[2]              IF A(K,K) IS ZERO
          NG        X2,[2]                        OR NEGATIVE
          "P"1DET   DET,JFAIL,FLOAT1,I,J  X1=(A(K,K)) NOT PRESERVED
          S_J       K+1                 J=K
          S_I       LENGTH              I=IA
          EQ        J,N,[3]             IF K=N
*                                       K=K-1
*                                       X1=(A(1,1))
          TSFACT2   SAVEA,N,K,A1K,AKK,I,JA,TEMP2,J
          SA4       SAVEN               X4=N
          S_J       JA+LENGTH           J=IA+JA
          S_A1K     A1K+JA              A1K=(A(1,K+1))
          S_AKK     AKK+J               AKK=(A(K+1,K+1))
          JP        [1]
*         IFAIL  =  -1
*         DET    =  ZERO
*         JFAIL  =  -2
 [2]      BSS       0
          SX5       -1                  X5=IFAIL=-1
          MX6       0                   DET=ZERO
 TAU      IFEQ      LENGTH,2
          MX7       0
 TAU      ENDIF
          SX0       -2                  X0=JFAIL=-2
          JP        [99]
*         DE-SCALE DET
*         SET JFAIL = -1, 0, +1
*         LOAD IFAIL INTO X5
 [3]      BSS       0
          SA3       DET                 X3=DET
 TAU      IFEQ      LENGTH,2
          SA4       DET+1
 TAU      ENDIF
          "P"2DET   JFAIL,I,J           X0=JFAIL, X6,X7=DET
          SA5       IFAIL               X5=IFAIL
 [99]     BSS       0                   X0=JFAIL, X5=IFAIL, X6,X7=DET
 TSFACTD  ENDM
 TSFACT2  MACRO     SAVEA,N,K,A1K,AKK,IA,JA,TEMP2,J    X1=(A)
*                                       K=K-1
*         CALL VSCL(N-K,A(K,K),A(K+1,K),IA,A(K,K+1),JA)
*         CALL MMNA(N-K,K,A(K+1,1),IA,JA,A(1,K+1),IA,A(K+1,K+1),IA)
          SA1       AKK                 X1=A(K,K)
 TAU      IFEQ      LENGTH,2
          SA2       AKK+1               X2=A''(K,K)
 TAU      ENDIF
          S_K       K+1                 K=(K-1)+1=K
          SA0       AKK+JA              A0=(A(K,K+1))
          SX0       AKK+IA              X0=(A(K+1,1))
          S_N       N-K                 N=N-K
          "P"VSCL   N,X0,IA,A0,JA,J     X1=A(K,K) [X2=A''(K,K)]
          SA1       SAVEA               X1=(A(1,1))
          SA0       AKK+IA              A0=(A(K+1,K))
          SX2       A1K+JA              X2=(A(1,K+1))
          SA0       A0+JA               A0=(A(K+1,K+1))
          SX1       X1+K                X1=(A(1,1))+K=(A(K+1,1))
 TAU      IFEQ      LENGTH,2
          SX1       X1+K
 TAU      ENDIF
*                                       N=N-K
*                                       X1=(A(K+1,1))
*                                       X2=(A(1,K+1))
*                                       A0=(A(K+1,K+1))
          "P"MMPY   "MINUS",N,K,IA,JA,IA,"PLUS",A0,IA,TEMP2,J
 TSFACT2  ENDM
 RRCPRCL  MACRO                         X1=AKK    A1=(AKK)
*                                       X5=1.
*         AKK  =  1./AKK
          RX6       X5/X1               X6=1./AKK
          SA6       A1                  AKK=1./AKK
 RRCPRCL  ENDM
 DRCPRCL  MACRO                         X1=AKK'   A1=(AKK')
*                                       X2=AKK''  A2=(AKK'')
*                                       X5=1.
          FX0       X5/X1               X0=1./AKK'
          FX4       X0*X1               X4=(1/AKK')*AKK'=R'
          DX3       X0*X1               X3=L(1/AKK')*AKK'=R''
          FX7       X5-X4               X7=1-R'=S'
          DX5       X5-X4               X5=L(1-R')=S''
          FX4       X0*X2               X4=(1/AKK')*AKK''
          NX7       X7                  X7=S'
          FX7       X7+X5               X7=S'+S''=T'
          FX5       X7-X3               X5=T'-R''
          FX7       X5-X4               X7=T'-R''-(1/AKK')*AKK''=U'
          FX3       X7/X1               X3=U'/AKK'
          FX7       X3+X0
          DX5       X3+X0
          NX0       X7
          DX7       X0+X5
          FX6       X0+X5
          SA7       A2
          SA6       A1
 DRCPRCL  ENDM
*CD       SFINV
 TSFINV   MACRO     TYPE,SAVEA,N,JA,TEMP4,IA,I,J,K,L   X1=(A)
          LOCAL     [99]
          SX6       N-1                 X6=N-1
          S_IA      LENGTH              IA=LENGTH
          ZR        X6,[99]             IF N=1
          TSFINV1   SAVEA,N,IA,JA,TEMP4,I,J,K,L      X1=(A)
          TSFINV2   SAVEA,N,IA,JA,TEMP4,I,J,K,L      X1=(A)
          S_N       N-1                 N=N-1
          TMUTL     TYPE,N,IA,JA,J      X1=(A)
 [99]     BSS       0
 TSFINV   ENDM
 TSFINV1  MACRO     SAVEA,N,IA,JA,TEMP3,A1I,AII,J,K    X1=(A)
          LOCAL     [1],[2],SAVEI,TEMP2
 SAVEI    EQU       TEMP3
 TEMP2    EQU       TEMP3+1
*         DO 20     I  =  2, N
*            IF(I .EQ. 2)  GOTO 10
*            CALL UMNS(I-2,A12,A13,A23,A(2,I),A(3,I),A(1,I),A(2,I))
* 10         A(I-1,I)  =  -A(I-1,I)
*            CALL VSCL(I-1,A(I,I),A(1,I),A(2,I),A(I,1),A(I,2))
* 20         CONTINUE
          SX2       X1+JA               X2=(A12)
          S_J       2                   J=I=2
          S_A1I     X2                  A1I=(A12)
          SX6       J                   X6=I=2
          S_AII     A1I+IA              AII=(A22)
          SA6       SAVEI               SAVEI=I=2
          JP        [2]
 [1]      BSS       0                   X1=(A)    J=I
          SX1       X1+JA               X1=(A12)
          SX6       J                   X6=I
          SX2       A1I+IA              X2=(A(2,I))
          SA6       SAVEI               SAVEI=I
          S_K       J-2                 K=I-2
          SA0       A1I                 A0=(A(1,I))
          "P"UMPY   "MINUS",K,IA,JA,IA,"MINUS",A0,IA,TEMP2,J
 [2]      SA4       AII-IA              X4=A(I-1,I)
          SA1       AII                 X1=A(I,I)
 TAU      IFEQ      LENGTH,2
          SA5       A4+1
          SA2       A1+1
 TAU      ENDIF
          SA3       SAVEI               X3=I
          BX6       -X4                 X6=-A(I-1,I)
 TAU      IFEQ      LENGTH,2
          BX7       -X5
 TAU      ENDIF
          SA5       SAVEA               X5=(A(1,1))
          SA6       A4                  A(I-1,I)=-A(I-1,I)
 TAU      IFEQ      LENGTH,2
          SA7       A4+1
 TAU      ENDIF
          S_K       X3-1                K=I-1
          SA0       X5+K                A0=(A(I,1))
 TAU      IFEQ      LENGTH,2
          SA0       A0+K
 TAU      ENDIF
          "P"VSCL   K,A1I,IA,A0,JA,J    X1=A(I,I) [X2=A''(I,I)]
          SA3       SAVEI               X3=I
          SA1       SAVEA               X1=(A)
          S_AII     AII+JA              AII=(A(I,I+1))
          S_A1I     A1I+JA              A1I=(A(1,I+1))
          S_AII     AII+IA              AII=(A(I+1,I+1))
          S_J       X3+1                J=I+1
          LE        J,N,[1]             IF I .LT. N
 TSFINV1  ENDM                          X1=(A(1,1))
 TSFINV2  MACRO     SAVEA,N,IA,JA,TEMP4,A1K,AKK,K,J    X1=(A)
          LOCAL     [1],SAVEN,SAVEK,TEMP2
 SAVEN    EQU       TEMP4
 SAVEK    EQU       TEMP4+1
 TEMP2    EQU       TEMP4+2
*         K  =  1
*  1      A(K,K)  =  VMPA(N-K,A(K,K+1),A(K,K+2),
*                             A(K+1,K),A(K+2,K),A(K,K))
*         K  =  K + 1
*         CALL MMPY(K-1,N-K+1,A(1,K),A(1,K+1),A(2,K),
*                             A(K,K),A(K+1,K),
*                             A(1,K),A(2,K))
*         IF(K .LT. N)  GOTO 1
          SX7       N                   X7=N
          SX6       1                   X6=1=K
          SA7       SAVEN               SAVEN=N
          S_N       N-1                 N=N-1
          SA6       SAVEK               SAVEK=K=1
          S_A1K     X1                  A1K=(A11)
          S_AKK     X1                  AKK=(A11)
 [1]      BSS       0                   N=N-K
          SX6       AKK+JA              X6=(A(K,K+1))
          SX7       AKK+IA              X7=(A(K+1,K))
          "P"VMPY   "PLUS",N,X6,JA,X7,IA,"PLUS",AKK,J
          SA5       SAVEK               X5=K
          SA6       AKK                 A(K,K)=VMPA
 TAU      IFEQ      LENGTH,2
          SA7       AKK+1
 TAU      ENDIF
          S_AKK     AKK+IA              AKK=(A(K+1,K))
          S_A1K     A1K+JA              A1K=(A(1,K+1))
          S_AKK     AKK+JA              AKK=(A(K+1,K+1))
*                                       K=K+1
*                                       N=N-(K+1)=N-K+1
          SX1       A1K                 X1=(A(1,K))
          SX2       AKK                 X2=(A(K,K))
          SA0       A1K                 A0=(A(1,K))
          SX6       X5+1                X6=K
          S_K       X5                  K=K-1
          SA6       A5                  SAVEK=K
          "P"MMPY   "PLUS",K,N,IA,JA,IA,"NULL",A0,IA,TEMP2,J
          S_N       N-1                 N=N-K
          NZ        N,[1]               IF K .LT. N
          SA5       SAVEN               X5=N
          SA1       SAVEA               X1=(A(1,1))
          S_N       X5                  RESTORE N
 TSFINV2  ENDM
*CD       SQR
 RSQR1R   MACRO     JX,J
          LOCAL     [1]
 [1]      BSS       0                   FOR K = 0, 1, 2, ...
 +        NX6       X6        0         X6=Z(K)
          S_J       J+1       1                             J=2-N+1+K
*                             2 X6
          RX6       X6+X1     3 X5      X6=Z(K)+X(1+K)**2
 +        RX1       X5*X5     4         X1=X(2+K)**2
          SA5       A5+JX     5         X5=X(3+K) A5=(X(3+K))
*                             6 X6
*                             7
          MI        J,[1]     8 X1      WHILE 3-N+K<0; I.E. 4+K .LE. N
 RSQR1R   ENDM
 RSQR4R   MACRO     JX,J
          LOCAL     [1]
 [1]      BSS       0                   FOR K = 0, 4, 8, ...
 +        NX7       X7        0 X0      X7=SUM1(1+K)
          SA2       A5+JX     1 X5      X2=X(7+K) A2=(X(7+K))
          RX1       X3*X3     2 X7      X1=X(4+K)**2
          NX6       X6        3         X6=SUM2(2+K)
 +        RX7       X7+X0     4         X7=SUM1(1+K)+X(3+K)**2
          RX0       X4*X4     5 X6      X0=X(5+K)**2
          S_J       J+4       6 X1                          J=9-N+4+K
 +        RX6       X6+X1     7 X7      X6=SUM2(2+K)+X(4+K)**2
          SA3       A2+JX     8 X2      A3=X(8+K) A3=(X(8+K))
          NX7       X7        9 X0      X7=SUM1(3+K)
          RX1       X5*X5    10 X6      X1=X(6+K)**2
 +        NX6       X6       11 X7      X6=SUM2(4+K)
          SA4       A3+JX    12         X4=X(9+K) A4=(X(9+K))
          RX7       X7+X0    13 X6      X7=SUM1(3+K)+X(5+K)**2
          SA5       A4+JX    14 X1      X5=X(10+K)A5=(X(10+K))
 +        RX6       X6+X1    15 X3      X6=SUM2(4+K)+X(6+K)**2
          RX0       X2*X2    16 X7      X0=X(7+K)**2
*                            17
*                            18 X6
          MI        J,[1]    19 X4      WHILE 13-N+K<0; I.E. 14+K .LE. N
 RSQR4R   ENDM
 RVSQR    MACRO     N,X,JX,J
          LOCAL     [1],[2],[3],[4],[5],[99]
          SA4       X                   X4=X(1)   A4=(X(1))
          BRANCH    [2],N,LE,9,J        J=9-N=8,7,6,...;N=1,2,3,...
          SA5       A4+JX               X5=X(2)   A5=(X(2))
          SA2       A5+JX               X2=X(3)   A2=(X(3))
          SA3       A2+JX               X3=X(4)   A3=(X(4))
          RX7       X4*X4               X7=SUM1(1)=X(1)**2
          SA4       A3+JX               X4=X(5)   A4=(X(5))
          RX6       X5*X5               X6=SUM2(2)=X(2)**2
          SA5       A4+JX               X5=X(6)   A5=(X(6))
          RX0       X2*X2               X0=X(3)**2
          RSQR4R    JX,J                J=3,2,1,0 FOR 0,1,2,3 TERMS LEFT
          NX7       X7                  X7=SUM1(N-8+J)
          RX1       X3*X3               X1=X(N-5+J)**2
          NX6       X6                  X6=SUM2(N-7+J)
          RX7       X7+X0               X7=SUM1(N-8+J)+X(N-6+J)**2
          RX0       X4*X4               X0=X(N-4+J)**2
          RX6       X6+X1               X6=SUM2(N-7+J)+X(N-5+J)**2
          NX7       X7                  X7=SUM1(N-6+J)
          RX1       X5*X5               X1=X(N-3+J)**2
          NX6       X6                  X6=SUM2(N-5+J)
          RX7       X7+X0               X7=SUM1(N-6+J)+X(N-4+J)**2
          RX6       X6+X1               X6=SUM2(N-5+J)+X(N-3+J)**2
          S_J       J-2                 J=+1,0,-1,-2
          NX1       X7                  X1=SUM1(N-4+ OLD J)
          GT        J,B0,[5]            IF NEW J=+1; I.E. 0 TERMS LEFT
          SA5       A5+JX               X5=X(N-3+ OLD J)
          MI        J,[3]               IF NEW J=-1,-2; I.E.2,3 TERMS LEFT
          JP        [4]                 IF NEW J=0; I.E. 1 TERM LEFT
 [1]      RX6       X4*X4               X6=X(1)**2
          JP        [99]
 [2]      BSS       0                   J=9-N=8,7,6,...,0; N=1,2,3,...,9
          S_J       J-7                 J=2-N=+1,0,-1,...,-7
          GT        J,B0,[1]            IF J>0; I.E. N=1
          RX1       X4*X4               X1=X(1)**2
          SA5       A4+JX               X5=X(2)   A5=(X(2))
          MX6       0                   X6=SUM(0)=0.
          ZR        J,[4]               IF J=0; I.E. N=2
 [3]      BSS       0                   J=2-N=-1,-2,...,-7; N=3,4,...,9
          RSQR1R    JX,J                J=0
 [4]      NX6       X6
          RX6       X6+X1
          RX1       X5*X5
 [5]      NX6       X6
          RX6       X6+X1
          NX6       X6                  X6=SUM(N)
 [99]     BSS       0
 RVSQR    ENDM
 DSQR1R   MACRO     JX,J
          LOCAL     [1]
 [1]      BSS       0                   FOR K = 0, 1, 2, ...
 +        FX6       X0+X7     0 X2      X6=U(C'(1+K)+F'(1+K))=G'(1+K)
          DX7       X0+X7     1         X7=L(C'(1+K)+F'(1+K))=G''(1+K)
          FX3       X3*X2     2         X3=U(X''(2+K)*X'(2+K))
*                             3 X6
          NO                  4 X7
 +        NX1       X6        5         X1=G'(1+K)
          FX0       X2*X2     6 X3      X0=U(X'(2+K)**2)=A'(2+K)
          NX7       X7        7 X1      X7=G''(1+K)
          DX2       X2*X2     8         X2=L(X'(2+K)**2)=A''(2+K)
 +        S_J       J+1       9 X7                          J=2-N+1+K
          FX6       X1+X7    10 X0      X6=U(G'(1+K)+G''(1+K))=S'(1+K)
          DX7       X1+X7    11         X7=L(G'(1+K)+G''(1+K))=S''(1+K)
 +        FX3       X3+X3    12 X2      X3=U(2*X''*X')=B'(2+K)
*                            13 X6
          DX4       X6+X0    14 X7      X4=L(S'(1+K)+A'(2+K))=C''(2+K)
          FX0       X6+X0    15 X3      X0=U(S'(1+K)+A'(2+K))=C'(2+K)
          FX1       X2+X3    16         X1=U(A''(2+K)+B'(2+K))=D'(2+K)
 +        SA3       A3+JX    17 X4      X3=X''(3+K)
          FX7       X7+X4    18 X0      X7=U(S''(1+K)+C''(2+K))=E'(2+K)
          SA2       A2+JX    19 X1      X2=X'(3+K)
*                            20
*                            21 X7
          FX7       X7+X1    22         X7=U(E'(2+K)+D'(2+K))=F'(2+K)
*                            23
*                            24 X3
 +        MI        J,[1]    25 X7      WHILE 3-N+K=J<0; I.E. 4+K .LE. N
 DSQR1R   ENDM
 DVSQR    MACRO     N,X,JX,J
          LOCAL     [1],[2]
          SA3       X+1                 X3=X''(1) A3=(X''(1))
          SA2       X                   X2=X'(1)  A2=(X'(1))
          S_J       -N
          S_J       J+2                                     J=2-N
          FX0       X3*X2               X0=U(X''(1)*X'(1))
          DX6       X2*X2               X6=L(X'(1)**2)
          FX1       X0+X0               X1=U(2*X''(1)*X'(1))
          FX0       X2*X2               X0=U(X'(1)**2)
          FX7       X6+X1               X7=U(A''(1)+B'(1))
          GT        J,B0,[2]            IF N=1
          SA3       A3+JX               X3=X''(2)
          SA2       A2+JX               X2=X'(2)
          ZR        J,[1]               IF N=2
          DSQR1R    JX,J                J=0
 [1]      FX6       X0+X7
          DX7       X0+X7
          FX3       X3*X2
          NX1       X6
          FX0       X2*X2
          NX7       X7
          DX2       X2*X2
          FX6       X1+X7
          DX7       X1+X7
          FX3       X3+X3
          DX4       X6+X0
          FX0       X6+X0
          FX1       X2+X3
          FX7       X7+X4
          FX7       X7+X1
 [2]      FX6       X0+X7
          DX7       X0+X7
          NX1       X6
          NX7       X7
          FX6       X7+X1
          DX7       X7+X1
 DVSQR    ENDM
 CSQR1R   MACRO     JX,J
          LOCAL     [1]
 [1]      BSS       0                   FOR K = 0, 1, 2, ...
 +        S_J       J+1       0 X4                          J=2-N+1+K
          RX0       X0+X3     1 X5      X0=SUM3(K)+X''(1+K)**2
          RX1       X4*X4     2         X1=X'(2+K)**2
 +        NX6       X6        3         X6=SUM1(1+K)
          RX2       X4*X5     4 X0      X2=X'(2+K)*X''(2+K)
          NX7       X7        5 X6      X7=SUM2(1+K)
          RX3       X5*X5     6 X1      X3=X''(2+K)**2
 +        NX0       X0        7 X7      X0=SUM3(1+K)
          SA4       A4+JX     8 X2      X4=X'(3+K)
          SA5       A5+JX     9 X0      X5=X''(3+K)
          RX6       X6+X1    10 X3      X6=SUM1(1+K)+X'(2+K)**2
 +        RX7       X7+X2    11         X7=SUM2(1+K)+X'(2+K)*X''(2+K)
*                            12
*                            13 X6
          MI        J,[1]    14 X7      WHILE 3-N+K<0; I.E. 4+K .LE. N
 CSQR1R   ENDM
 CVSQR    MACRO     N,X,JX,J
          LOCAL     [1],[2]
          SA4       X                   X4=X'(1)  A4=(X'(1))
          SA5       X+1                 X5=X''(1) A5=(X''(1))
          S_J       -N
          S_J       J+2                                     J=2-N
          RX6       X4*X4               X6=X'(1)**2
          MX3       0                   X3=0.
          RX7       X4*X5               X7=X'(1)*X''(1)
          RX0       X5*X5               X0=X''(1)**2
          GT        J,B0,[2]            IF N=1
          SA4       A4+JX               X4=X'(2)  A4=(X'(2))
          SA5       A5+JX               X5=X''(2) A5=(X''(2))
          ZR        J,[1]               IF N=2
          CSQR1R    JX,J                J=0
 [1]      RX0       X0+X3               X0=SUM3(N-2)+X''(N-1)**2
          RX1       X4*X4               X1=X'(N)**2
          NX6       X6                  X6=SUM1(N-1)
          RX2       X4*X5               X2=X'(N)*X''(N)
          NX7       X7                  X7=SUM2(N-1)
          RX3       X5*X5               X3=X''(N)**2
          NX0       X0                  X0=SUM3(N-1)
          RX6       X6+X1               X6=SUM1(N-1)+X'(N)**2
          RX7       X7+X2               X7=SUM2(N-1)+X'(N)*X''(N)
          RX0       X0+X3               X0=SUM3(N-1)+X''(N)**2
          NX6       X6                  X6=SUM1(N)
          NX7       X7                  X7=SUM2(N)
          NX0       X0                  X0=SUM3(N)
 [2]      RX6       X6-X0               X6=SUM1(N)-SUM3(N)
          RX7       X7+X7               X7=2*SUM2(N) IS NORMALIZED
          NX6       X6
 CVSQR    ENDM
*CD       SUM
 RSUM1    MACRO     JX,J                X4=X(1)   A4=(X(1))
          LOCAL     [1]
 [1]      BSS       0                   FOR K = 0, 1, 2,...
*                             0 X4
 +        RX6       X6+X4     1         X6=SUM(K)+X(1+K)
          SA4       A4+JX     2         X4=X(2+K) A4=(X(2+K))
          S_J       J+1       3                             J=-R+K+1
*                             4 X6
 +        NX6       X6        5
*                             6
*                             7 X6
          MI        J,[1]     8         WHILE 1-R+K<0; I.E. 2+K .LE. R
 RSUM1    ENDM                          X6=SUM(R) A4=(X(R)) J=0
 RSUM2    MACRO     JX,J                J=1-R
          LOCAL     [1]
 [1]      BSS       0                   FOR K = 0,2,4,...
 +        S_J       J+2       0 X3                          J=1-R+K+2
          NX6       X6        1
          RX5       X5+X3     2 X4      X5=S1(K)+X(1+K)
 +        SA3       A4+JX     3 X6      X3=X(4+K)
          RX6       X6+X4     4         X6=S2(1+K)+X(3+K)
          SA4       A3+JX     5 X5      X4=X(5+K)
          NX5       X5        6
*                             7 X6
*                             8 X5
 +        MI        J,[1]     9         WHILE 3-R+K<0; I.E. 4+K .LE. R
 RSUM2    ENDM                          J=0,1 FOR R=2Q+J
 RSUM4    MACRO     JX,J,FOUR           J=3-R
          LOCAL     [1]
 [1]      BSS       0                   FOR K = 0,4,8,...
 +        NX7       X7        0         X7=S2(2)
          NX0       X0        1 X4      X0=S3(3)
          NX6       X6        2 X7      X6=S4(4)
          RX1       X1+X3     3 X0      X1=S1(1)+X(5+K)
 +        SA3       A4+JX     4 X6      X3=X(9+K)
          RX7       X7+X5     5         X7=S2(2)+X(6+K)
          SA5       A3+JX     6 X1      X5=X(10+K)
          RX0       X0+X2     7         X0=S3(3)+X(7+K)
 +        SA2       A5+JX     8 X7      X2=X(11+K)
          RX6       X6+X4     9         X6=S4(4)+X(8+K)
          SA4       A2+JX    10 X0      X4=X(12+K)
          S_J       J+FOUR   11 X3                          J=3-R+K+4
 +        NX1       X1       12 X6      X1=S1(5+K)
*                            13 X5
*                            14 X1
          MI        J,[1]    15 X2      WHILE 7-R+K<0; I.E. 8+K .LE. R
 RSUM4    ENDM                          J=0,1,2,3 FOR R=2Q+3-J
 RVSUM1   MACRO     JX,J                X4=X(1)   A4=(X(1)) J=-N
          LOCAL     [1],[99]
          S_J       J+2                                     J=2-N
          BX6       X4                  X6=X(1)=SUM(1)
          GT        J,B0,[99]           IF N=1 ALL DONE
          SA4       A4+JX               X4=X(2)   A4=(X(2))
          ZR        J,[1]               IF N=2
          RSUM1     JX,J                X6=SUM(N-1)
 [1]      RX6       X6+X4               X6=SUM(N-1)+X(N)
          SB0       0
          NX6       X6                  X6=SUM(N)
 [99]     BSS       0
 RVSUM1   ENDM                          X6=SUM(N)
 RVSUM2   MACRO     JX,J                X4=X(1)   A4=(X(1)) J=-N
          LOCAL     [1],[2],[99],LIMIT
 LIMIT    EQU       5                   UP TO N=LIMIT, RVSUM1 IS USED
          SX7       J+LIMIT             X7=LIMIT-N
          PL        X7,[2]
          SA5       A4+JX               X5=X(2)
          S_J       J+5                                     J=5-N
          SA3       A5+JX               X3=X(3)
          BX6       X4                  X6=X(1)
          SA4       A3+JX               X4=X(4)   A4=(X(4))
          RSUM2     JX,J
          NZ        J,[1]               IF ALL TERMS FETCHED
 +        NX6       X6
          RX6       X6+X4
          SA4       A4+JX
 [1]      NX6       X6
          RX5       X5+X3
          RX6       X4+X6
          NX5       X5
          NX6       X6
          RX6       X6+X5
          NX6       X6
          JP        [99]
 [2]      RVSUM1    JX,J
 [99]     BSS       0                   X6=RVSUM(N,X(1),X(2))
 RVSUM2   ENDM
 RVSUM    MACRO     N,JX,J              X4=X(1)   A4=(X(1))
          S_J       -N                                      J=-N
          RVSUM2    JX,J
 RVSUM    ENDM                          X6=SUM
*CD       XCH
 RXCH1R   MACRO     JX,JY,J
          LOCAL     [1]
 [1]      BSS       0                   FOR K = 0, 1, 2,...
 +        S_J       J+1       0 X4                          J=2-N+1+K
          BX7       X4        1 X5      X7=X(2+K)
          BX6       X5        2 X7      X6=Y(2+K)
 +        SA4       A4+JX     3 X6      X4=X(3+K)
          SA5       A5+JY     4         X5=Y(3+K)
          SA7       A7+JY     5         Y(2+K)=X(2+K)
          SA6       A6+JX     6         X(2+K)=Y(2+K)
                              7
                              8
 +        MI        J,[1]     9         WHILE 3-N+K = J < 0; I.E. 4+K .LE. N
 RXCH1R   ENDM
 RXCH6R   MACRO     JX,JY,J
          LOCAL     [1]
 [1]      BSS       0                   FOR K = 0, 6, 12, ...
          S_J       J+6       0 X4                          J=8-N+6+K
          DUP       3
          BX7       X2        1 X5      X7=X(2+K)
          BX6       X3        2 X7      X6=Y(2+K)
          SA2       A4+JX     3 X6      X2=X(4+K)
          SA3       A5+JY     4         X3=Y(4+K)
          SA7       A7+JY     5         Y(2+K)=X(2+K)
          SA6       A6+JX     6         X(2+K)=Y(2+K)
          BX7       X4        7         X7=X(3+K)
          BX6       X5        8 X7      X6=Y(3+K)
          SA4       A2+JX     9 X6      X4=X(5+K)
          SA5       A3+JY    10 X2      X5=Y(5+K)
          SA7       A7+JY    11 X3      Y(3+K)=X(3+K)
          SA6       A6+JX    12         X(3+K)=Y(3+K)
          ENDD
          MI        J,[1]    39         WHILE 14-N+K=J<0; I.E. 15+K .LE. N
 RXCH6R   ENDM
 RVXCH    MACRO     N,X,JX,Y,JY,J
          LOCAL     [1],[2],[3],[4],[99]
          BRANCH    [2],N,GT,8,J        J = 8-N
 +        SA2       X         0         X2=X(1)
          SA3       Y         1         X3=Y(1)
          SX6       N-1       2         X6=N-1
                              3
                              4
 +        ZR        X6,[1]    5         IF N=1
          SA4       A2+JX     6         X4=X(2)
          SA5       A3+JY     7 X2      X5=Y(2)
 +        BX7       X2        8 X3      X7=X(1)
          BX6       X3        9 X7      X6=Y(1)
          S_J       J-6      10 X6                          J=2-N
 +        SA7       Y        11         Y(1)=X(1)
          SA6       X        12         X(1)=Y(1)
                             13
          ZR        J,[4]    14         IF N=2
                             15
                             16
 +        JP        [3]      17         FOR N=3,4,5,6,7,8
 [1]      BX7       X2                  X7=X(1)
          BX6       X3                  X6=Y(1)
          SA7       Y                   Y(1)=X(1)
          SA6       X                   X(1)=Y(1)
          SA4       A2                            A4=(X(1))
          SA5       A3                            A5=(Y(1))
          JP        [99]
 [2]      BSS       0                                       J=8-N
 +        SA4       X         0         X4=X(1)
          SA5       Y         1         X5=Y(1)
          NO                  2
          NO                  3
 +        SA2       A4+JX     4         X2=X(2)
          SA3       A5+JY     5         X3=Y(2)
                              6
                              7 X4
          BX7       X4        8 X5      X7=X(1)
          BX6       X5        9 X7      X6=Y(1)
 +        SA4       A2+JX    10 X6      X4=X(3)
          SA5       A3+JY    11 X2      X5=Y(3)
          SA7       Y        12 X3      Y(1)=X(1)
          SA6       X        13         X(1)=Y(1)
          RXCH6R    JX,JY,J
 +        S_J       J-5      39         J=0,-1,-2,-3,-4,-5
                             40 X4
          BX7       X2       41 X5      X7=X(J+N-1)
          BX6       X3       42 X7      X6=Y(J+N-1)
 +        SA7       A7+JY    43 X6      Y(J+N-1)=X(J+N-1)
          SA6       A6+JX    44         X(J+N-1)=Y(J+N-1)
                             45
          ZR        J,[4]    46         IF LAST TERM FETCHED
 [3]      RXCH1R    JX,JY,J
 [4]      BX7       X4                  X7=X(N)
          BX6       X5                  X6=Y(N)
          SA7       A7+JY               Y(N)=X(N)
          SA6       A6+JX               X(N)=Y(N)
 [99]     BSS       0
 RVXCH    ENDM
 DXCHPR   MACRO     P,JX,JY,J
          LOCAL     [1]
 [1]      BSS       0                   FOR K = 0, P, 2P, ...
 +        S_J       J+P
 TERM     DUP       P
          BX7       X2        1 X5      X7=X'(K)
          BX6       X3        2 X7      X6=Y'(K)
          SA2       A2+JX     3 X6      X2=X'(K+1)
          SA3       A3+JY     4         X3=Y'(K+1)
          SA6       A2-JX     5         X'(K)=Y'(K)
          SA7       A3-JY     6         Y'(K)=X'(K)
          BX7       X4        7         X7=X''(K)
          BX6       X5        8 X7      X6=Y''(K)
          SA4       A4+JX     9 X6      X4=X''(K+1)
          SA5       A5+JY    10 X2      X5=Y''(K+1)
          SA6       A4-JX    11 X3      X''(K)=Y''(K)
          SA7       A5-JY    12         Y''(K)=X''(K)
 TERM     ENDD
          MI        J,[1]               WHILE J<0
 DXCHPR   ENDM
 DVXCH    MACRO     N,X,JX,Y,JY,J
          LOCAL     [1],[2]
          SA2       X                   X2=X'(1)
          SA3       Y                   X3=Y'(1)
          SA4       X+1                 X4=X"(1)
          SA5       Y+1                 X5=Y"(1)
          BRANCH    [1],N,LE,3,J                            J=3-N
          DXCHPR    3,JX,JY,J                               J=2,1,0
 [1]      S_J       J-2                                     J=0,-1,-2
          ZR        J,[2]
          DXCHPR    1,JX,JY,J                               J=0
 [2]      BX7       X2                  X7=X'(N)
          BX6       X3                  X6=Y'(N)
          SA7       A3                  Y'(N)=X'(N)
          SA6       A2                  X'(N)=Y'(N)
          BX7       X4                  X7=X"(N)
          BX6       X5                  X6=Y"(N)
          SA7       A5                  Y"(N)=X''(N)
          SA6       A4                  X"(N)=Y''(N)
 DVXCH    ENDM
