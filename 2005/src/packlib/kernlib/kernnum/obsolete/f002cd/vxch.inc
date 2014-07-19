*
* $Id: vxch.inc,v 1.1.1.1 1996/02/15 17:48:00 mclareni Exp $
*
* $Log: vxch.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:00  mclareni
* Kernlib
*
*
*
* vxch.inc
*
*CALL     BASIC
*CALL     XCH
 MAIN     MACRO     TYPE,N,JX,JY,J
          SETYPE    TYPE
 NAME     MICRO     1,,*"P"_VXCH_"S"*
          HEADER    "NAME"              A0 IS NEITHER USED NOR SAVED
          EXIT      "NAME"              (N,X,X2,Y,Y2)
          PROLOG    N,JX,JY,J
 TAU      IFEQ      TYPE,CTYPE
          DVXCH     N,X0,JX,X1,JY,J
 TAU      ELSE
          "P"VXCH   N,X0,JX,X1,JY,J
 TAU      ENDIF
          EPILOG
          ENDM
 PROLOG   MACRO     NN,JX,JY,ONE
*                                       X0=(X)
*                                       X1=(Y)
*                                       X5=N
          BSS       0                   X1=(N)    A1=((N))
 +        S_ONE     1         0                             ONE=1
          SA5       X1        1         X5=N      A5=(N)
          SA3       A1+ONE    2         X3=(X)    A3=((X))
 +        SA1       A1+3      3         X1=(Y)    A1=((Y))
                              4 A1
          SA2       A1+ONE    5         X2=(Y2)   A2=((Y2))
          SA4       A3+ONE    6         X4=(X2)   A4=((X2))
                              7
                              8 X5
 +        S_NN      X5        9 X3                          NN=N
          BX0       X3       10 X1      X0=(X)
                             11 X0
          LE        NN,B0,EXIT  X2      IF N.LE.0 EXIT WITH A0 PRESERVED
 +        IX6       X2-X1    13 X4      X6=(Y2)-(Y)
          IX7       X4-X3    14 X6      X7=(X2)-(X)
          S_JY      X6       15 X7                          JY=(Y2)-(Y)
          S_JX      X7       16                             JX=(X2)-(X)
 PROLOG   ENDM
