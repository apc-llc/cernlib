*
* $Id: rscl.inc,v 1.1.1.1 1996/02/15 17:48:06 mclareni Exp $
*
* $Log: rscl.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:06  mclareni
* Kernlib
*
*
*
* rscl.inc
*
*CALL     BASIC
*CALL     SCL
 MAIN     MACRO     TYPE,M,N,D,ID,JX,JZ,J
          SETYPE    TYPE
 NAME     MICRO     1,,*"P"_RSCL*
          HEADER    "NAME",SAVEA0
 TEMP     BSS       3
          EXIT      "NAME"              (M,N,D,D2,X,X12,X21,Z,Z12,Z21)
          PROLOG    EXIT,M,N,D,ID,JX,JZ,J    X0=(X),X6=IX,A0=(Z),X7=IZ
          TRSCL     TYPE,M,N,D,ID,JX,JZ,J,TEMP
          EPILOG
          ENDM
 PROLOG   MACRO     EX,MM,NN,D,ID,JX,JZ,ONE
          BSS       0                   X1=(M)    A1=((M))
          SA4       X1                  X4=M      A4=(M)
          S_ONE     1                                       ONE=1
          SA5       A1+ONE              X5=(N)    A5=((N))
          SA2       A1+7                X2=(Z)    A2=((Z))
          SA1       A5+ONE              X1=(D)    A1=((D))
          SA3       A2+ONE              X3=(Z12)  A3=((Z12))
          S_MM      X4                                      MM=M
          SA4       A3+ONE              X4=(Z21)  A4=((Z21))
          LE        MM,B0,EX            IF M.LE.0 EXIT WITH A0 PRESERVED
          SA5       X5                  X5=N      A5=(N)
          SX6       A0                  X6=OLD A0
          IX7       X3-X2               X7=(Z12)-(Z)
          SA3       A1+2                X3=(X)    A3=((X))
          SA6       SAVEA0              SAVE OLD A0
          S_D       X1                                      D=(D(1))
          BX6       X1                  X6=(D)
          SA1       A1+ONE              X1=(D2)   A1=((D2))
          S_JZ      X7                                      JZ=(Z12)-(Z)
          IX7       X4-X2               X7=(Z21)-(Z)
          SA4       A3+ONE              X4=(X12)  A4=((X12))
          S_NN      X5                                      NN=N
          SA5       A4+ONE              X5=(X21)  A5=((X21))
          LE        NN,B0,EX            IF N.LE.0 EXIT WITH A0 PRESERVED
          BX0       X3                  X0=(X)
          SA0       X2                            A0=(Z)
          IX1       X1-X6               X1=(D2)-(D)
          IX4       X4-X3               X4=(X12)-(X)
          IX6       X5-X3               X6=(X21)-(X)
          S_ID      X1                                      ID=(D2)-(D)
          S_JX      X4                                      JX=(X12)-(X)
 PROLOG   ENDM
