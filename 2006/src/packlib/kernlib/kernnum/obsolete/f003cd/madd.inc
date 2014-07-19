*
* $Id: madd.inc,v 1.1.1.1 1996/02/15 17:48:05 mclareni Exp $
*
* $Log: madd.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:05  mclareni
* Kernlib
*
*
*
* madd.inc
*
*CALL     BASIC
*CALL     ADD
 ADD      SET       1
 SUB      SET       2
 MAIN     MACRO     TYPE,FCN,M,N,JX,JY,Z,IZ,JZ,J
          SETYPE    TYPE
          IFEQ      FCN,ADD,2
 SIGN     MICRO     1,,*+*
 VERB     MICRO     1,,*MADD*
          IFEQ      FCN,SUB,2
 SIGN     MICRO     1,,*-*
 VERB     MICRO     1,,*MSUB*
 NAME     MICRO     1,,*"P"_"VERB"*
          HEADER    "NAME",SAVEA0
 TEMP6    BSS       6                   WORKING STORE (.MADD)
 TAU      IFEQ      TYPE,CTYPE
 SWITCH   BSS       1
 PLIST    BSS       1
 TAU      ENDIF
          EXIT      "NAME"           (M,N,X,X12,X21,Y,Y12,Y21,Z,Z12,Z21)
 TAU      IFEQ      TYPE,CTYPE
          MX6       0
          SX7       A1                  X7=((M))
          SA6       SWITCH              SWITCH=.FALSE.
          SA7       PLIST               PLIST=((M))
          PROLOG    M,N,JX,JY,IZ,JZ,J
 [1]      RMADD     M,N,JX,"SIGN",JY,Z,IZ,JZ,J,TEMP6
          SA5       SWITCH              X5=SWITCH
          SA1       PLIST               X1=((M))
          SA2       SAVEA0              X2=OLD A0
          MI        X5,[2]              IF SWITCH=.TRUE.
          SA1       X1                  X1=(M)    A1=((M))
          MX6       1
          SA0       X2                  RESTORE OLD A0
          SA6       SWITCH              SWITCH=.TRUE.
          PROLOG    M,N,JX,JY,IZ,JZ,J
          SX2       X2+1                X2=(X'')
          SX4       X4+1                X4=(Y'')
          S_Z       Z+1                 Z=(Z'')
          JP        [1]
 [2]      EPILOG
 TAU      ELSE
          PROLOG    M,N,JX,JY,IZ,JZ,J
          "P"MADD   M,N,JX,"SIGN",JY,Z,IZ,JZ,J,TEMP6
          EPILOG
 TAU      ENDIF
          ENDM
 PROLOG   MACRO     MM,NN,JX,JY,IZ,JZ,ONE
*                                       X2=(X)    A0=(Z)
*                                       X3=IX
*                                       X4=(Y)
*                                       X5=IY
          BSS       0                   X1=(M)    A1=((M))
 +        S_ONE     1         0                             ONE=1
          SA2       X1        1         X2=M      A2=(M)
          SA3       A1+ONE    2         X3=(N)    A3=((N))
 +        SX6       A0        3         X6=OLD A0
          SA4       A3+ONE    4 X6      X4=(X)    A4=((X))
          SA6       SAVEA0    5         SAVEA0=OLD A0
*                             6
*                             7
 +        SA5       A4+ONE    8 X2      X5=(X12)  A5=((X12))
          S_MM      X2        9 X3                          MM=M
          SA3       X3       10         X3=N      A3=(N)
          SA1       A5+ONE   11 X4      X1=(X21)  A1=((X21))
 +        BX0       X4       12         X0=(X)
          SA2       A1+ONE   13 X0      X2=(Y)    A2=((Y))
*                            14
          LE        MM,B0,EXIT  X5      IF M .LE. 0 GO TO EXIT
 +        SA4       A2+ONE   16         X4=(Y12)  A4=((Y12))
*                            17 X3
          IX6       X5-X0    18 X1      X6=(X12)-(X)=JX
          SA5       A4+ONE   19 X6      X5=(Y21)  A5=((Y21))
          IX7       X1-X0    20 X2      X7=(X21)-(X)=IX
 +        S_NN      X3       21 X7                          NN=N
          SA1       A5+ONE   22         X1=(Z)    A1=((Z))
*                            23 X4
          LE        NN,B0,EXIT          IF N .LE. 0 GO TO EXIT
 +        SA3       A1+ONE   25         X3=(Z12)  A3=((Z12))
          S_JX      X6       26 X5                          JX=(X12)-(X)
          IX6       X4-X2    27         X6=(Y12)-(Y)
          SA4       A3+ONE   28 X6      X4=(Z21)  A4=((Z21))
 +        IX5       X5-X2    29 X1      X5=(Y21)-(Y)=IY
          SA0       X1       30 X5                A0=(Z)
          S_JY      X6       31                             JY=(Y12)-(Y)
*                            32 X3
          IX6       X3-X1    33         X6=(Z12)-(Z)=JZ
*                            34 X6
 +        S_JZ      X6       35 X4                          JZ=(Z12)-(Z)
          IX6       X4-X1    36         X6=(Z21)-(Z)=IZ
          BX4       X2       37 X6      X4=(Y)
          S_IZ      X6       38 X4                          IZ=(Z21)-(Z)
 +        BX2       X0       39         X2=(X)
          BX3       X7       40 X2      X3=(X21)-(X)=IX
*                            41 X3
 PROLOG   ENDM
