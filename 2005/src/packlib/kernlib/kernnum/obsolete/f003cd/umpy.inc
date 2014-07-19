*
* $Id: umpy.inc,v 1.1.1.1 1996/02/15 17:48:06 mclareni Exp $
*
* $Log: umpy.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:06  mclareni
* Kernlib
*
*
*
* umpy.inc
*
*CALL     BASIC
*CALL     MPY
 UMNA     SET       1
 UMNS     SET       2
 UMPA     SET       3
 UMPS     SET       4
 UMPY     SET       5
 MAIN     MACRO     TYPE,FCN,M,IX,JX,JY,IZ,J
          SETYPE    TYPE
          IFEQ      FCN,UMNA,3
 SIGNX    MICRO     1,,*-*
 SIGNZ    MICRO     1,,*+*
 VERB     MICRO     1,,*UMNA*
          IFEQ      FCN,UMNS,3
 SIGNX    MICRO     1,,*-*
 SIGNZ    MICRO     1,,*-*
 VERB     MICRO     1,,*UMNS*
          IFEQ      FCN,UMPA,3
 SIGNX    MICRO     1,,*+*
 SIGNZ    MICRO     1,,*+*
 VERB     MICRO     1,,*UMPA*
          IFEQ      FCN,UMPS,3
 SIGNX    MICRO     1,,*+*
 SIGNZ    MICRO     1,,*-*
 VERB     MICRO     1,,*UMPS*
          IFEQ      FCN,UMPY,3
 SIGNX    MICRO     1,,*+*
 SIGNZ    MICRO     1,,*0*
 VERB     MICRO     1,,*UMPY*
 NAME     MICRO     1,,*"P"_"VERB"*
          HEADER    "NAME",SAVEA0
 TEMP2    BSS       2                   WORKING STORE
          EXIT      "NAME"              (M,X,X12,X21,Y,Y2,Z,Z2)
          PROLOG    M,IX,JX,JY,IZ,J
*                                       X1=(X), X2=(Y), A0=(Z)
          "P"UMPY   "SIGNX",M,IX,JX,JY,"SIGNZ",A0,IZ,TEMP2,J
          EPILOG
          ENDM
 PROLOG   MACRO     MM,IX,JX,JY,IZ,ONE  X1=(X),X2=(Y),A0=(Z)
          BSS       0                   X1=(M)    A1=((M))
 +        SA5       A1+6      0         X5=(Z)    A5=((Z))
          S_ONE     1         1                             ONE=1
 +        SA2       X1        2         X2=M      A2=(M)
          SA3       A5+ONE    3         X3=(Z2)   A3=((Z2))
          SA4       A5-ONE    4         X4=(Y2)   A4=((Y2))
          SX7       A0        5         X7=OLD A0
*                             6 X7
*                             7 X5
 +        SA1       A1+ONE    8         X1=(X)    A1=((X))
          SA7       SAVEA0    9 X2      SAVEA0 = OLD A0
          S_MM      X2       10 X3                          MM=M
 +        SA2       A4-ONE   11 X4      X2=(Y)    A2=((Y))
          IX6       X3-X5    12         X6=(Z2)-(Z)=IZ
          SA3       A1+ONE   13 X6      X3=(X12)  A3=((X12))
          SA0       X5       14                   A0=(Z)
 +        SA5       A2-ONE   15 X1      X5=(X22)  A5=((X22))
*                            16
          LE        MM,B0,EPILOG        IF M.LE.0 GOTO EPILOG
          S_IZ      X6       18 X2                          IZ=(Z2)-(Z)
 +        NO                 19
          IX7       X4-X2    20 X3      X7=(Y2)-(Y)=JY
*                            21 X7
          IX6       X3-X1    22 X5      X6=(X12)-(X)=JX
          IX0       X5-X3    23 X6      X0=(X22)-(X12)=IX
 +        S_JY      X7       24 X0                          JY=(Y2)-(Y)
          S_JX      X6       25                             JX=(X12)-(X)
          S_IX      X0       26                             IX=(X22)-(X12)
 PROLOG   ENDM
