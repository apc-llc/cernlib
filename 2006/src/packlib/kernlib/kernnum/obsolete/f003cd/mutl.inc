*
* $Id: mutl.inc,v 1.1.1.1 1996/02/15 17:48:06 mclareni Exp $
*
* $Log: mutl.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:06  mclareni
* Kernlib
*
*
*
* mutl.inc
*
*CALL     BASIC
*CALL     CPY
*CALL     UTL
 MAIN     MACRO     TYPE,NM1,IX,JX,J
          SETYPE    TYPE                SET PREFIX, SUFFIX, LENGTH
 NAME     MICRO     1,,*"P"_MUTL*
          HEADER    "NAME"              A0 IS NEITHER USED NOR SAVED
          EXIT      "NAME"
          PROLOG    NM1,IX,JX
*                                       X1=(X(1,1))
          TMUTL     TYPE,NM1,IX,JX,J
          EPILOG
          ENDM
 PROLOG   MACRO     NM1,IX,JX
          BSS       0                   X1=(N)    A1=((N))
 +        SA5       X1        0         X5=N      A5=(N)
          SA2       A1+2      1         X2=(X12)  A2=((X12))
          NO                  2
 +        SA1       A2-1      3         X1=(X)    A1=((X))
          SA3       A2+1      4         X3=(X21)  A3=((X21))
*                             5
*                             6
*                             7 X5
 +        S_NM1     X5-1      8 X2                          NM1=N-1
*                             9
*                            10 X1
          LE        NM1,B0,EXIT X3      IF N .LE. 1 EXIT
 +        IX6       X2-X1    12         X6=(X12)-(X)=JX
          IX7       X3-X1    13 X6      X7=(X21)-(X)=IX
          S_JX      X6       14 X7                          JX=(X12)-(X)
          S_IX      X7       15                             IX=(X21)-(X)
 PROLOG   ENDM
