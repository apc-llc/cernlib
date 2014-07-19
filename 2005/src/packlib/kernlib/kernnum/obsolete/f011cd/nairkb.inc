*
* $Id: nairkb.inc,v 1.1.1.1 1996/02/15 17:48:12 mclareni Exp $
*
* $Log: nairkb.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:12  mclareni
* Kernlib
*
*
*
* nairkb.inc
*
 PROLOG   MACRO     ERROR,SAVEA,NN,JA,KK,B,ONE  X3=(R), X1=(A), X2=(B)
          BSS       0                   X1=(N)    A1=((N))
          SA3       A1+3      0         X3=(R)    A3=((R))
          S_ONE     1         1                             ONE=1
 +        SA1       X1        2         X1=N      A1=(N)
          SA5       A3+ONE    3         X5=(K)    A5=((K))
          SA4       A3-ONE    4         X4=(IDIM) A4=((IDIM))
          SX7       A0        5         X7=OLD A0
*                             6 X7
*                             7 X3
 +        SA7       SAVEA0    8         SAVEA0=OLD A0
          SA2       A5+ONE    9 X1      X2=(B)    A2=((B))
          S_NN      X1       10 X5                          NN=N
 +        SA5       X5       11 X4      X5=K      A5=(K)
          SA1       A4-ONE   12         X1=(A)    A1=((A))
          SA4       X4       13         X4=IDIM   A4=(IDIM)
          SX0       NN       14         X0=N
 +        NO                 15 X0
*                            16 X2
          S_B       X2       17                             B=(B)
*                            18 X5
          LT        NN,ONE,ERROR  X1    IF N<1
 +        S_KK      X5       20 X4                          KK=K
          S_JA      X4       21                             JA=IDIM
*                            22
          LT        KK,ONE,ERROR        IF K<1
*                            24
 +        LT        JA,NN,ERROR         IF IDIM<N
          BX6       X1       26         X6=(A)
 TAU      IFEQ      LENGTH,2
          S_JA      JA+JA                              JA=LENGTH*IDIM
 TAU      ENDIF
          SA6       SAVEA               SAVEA=(A)
 PROLOG   ENDM                          X1=(A), X2=(B), X3=(R)
