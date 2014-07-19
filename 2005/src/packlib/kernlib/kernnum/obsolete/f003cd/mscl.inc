*
* $Id: mscl.inc,v 1.1.1.1 1996/02/15 17:48:05 mclareni Exp $
*
* $Log: mscl.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:05  mclareni
* Kernlib
*
*
*
* mscl.inc
*
*CALL     BASIC
*CALL     SCL
 MAIN     MACRO     TYPE,M,N,IX,JX,IZ,JZ,J
          SETYPE    TYPE
 NAME     MICRO     1,,*"P"_MSCL*
          HEADER    "NAME",SAVEA0
          EXIT      "NAME"              (M,N,S,X,X12,X21,Z,Z12,Z21)
          PROLOG    EXIT,M,N,IX,JX,IZ,JZ,J    X0=(X), X1=S, A0=(Z)
          "P"MSCL   M,N,X0,IX,JX,A0,IZ,JZ,J
          EPILOG
          ENDM
 PROLOG   MACRO     EX,MM,NN,IX,JX,IZ,JZ,ONE
          BSS       0                   X1=(M)    A1=((M))
          SA4       X1                  X4=M      A4=(M)
          S_ONE     1                                       ONE=1
          SA5       A1+ONE              X5=(N)    A5=((N))
          SA2       A1+6                X2=(Z)    A2=((Z))
          SA1       A5+ONE              X1=(S)    A1=((S))
          SA3       A2+ONE              X3=(Z12)  A3=((Z12))
          S_MM      X4                                      MM=M
          SA4       A3+ONE              X4=(Z21)  A4=((Z21))
          LE        MM,B0,EX            IF M.LE.0 EXIT WITH A0 PRESERVED
          SA5       X5                  X5=N      A5=(N)
          SX6       A0                  X6=OLD A0
          IX7       X3-X2               X7=(Z12)-(Z)
          SA3       A1+ONE              X3=(X)    A3=((X))
          SA1       X1                  X1=S      A1=(S)
          SA6       SAVEA0              SAVE OLD A0
          S_JZ      X7                                      JZ=(Z12)-(Z)
          IX6       X4-X2               X6=(Z21)-(Z)
          SA4       A3+ONE              X4=(X12)  A4=((X12))
          S_NN      X5                                      NN=N
          SA5       A4+ONE              X5=(X21)  A5=((X21))
          LE        NN,B0,EX            IF N.LE.0 EXIT WITH A0 PRESERVED
          BX0       X3                  X0=(X)
          S_IZ      X6                                      IZ=(Z21)-(Z)
          SA0       X2                            A0=(Z)
          IX6       X4-X3               X6=(X12)-(X)
          IX7       X5-X3               X7=(X21)-(X)
          S_JX      X6                                      JX=(X12)-(X)
          S_IX      X7                                      IX=(X21)-(X)
 PROLOG   ENDM
