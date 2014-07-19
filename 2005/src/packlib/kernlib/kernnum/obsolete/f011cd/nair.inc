*
* $Id: nair.inc,v 1.1.1.1 1996/02/15 17:48:12 mclareni Exp $
*
* $Log: nair.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:12  mclareni
* Kernlib
*
*
*
* nair.inc
*
 PROLOG   MACRO   ERROR,SAVEA,SAVER,NN,JA,ONE  X1=(A), X3=(R), A3=((R))
          BSS       0                   X1=(N)    A1=((N))
 +        SA2       A1+2      0         X2=(IDIM) A2=((IDIM))
          S_ONE     1         1                             ONE=1
 +        SA4       X1        2         X4=N      A4=(N)
          SA1       A2-ONE    3         X1=(A)    A1=((A))
          SA3       A2+ONE    4         X3=(R)    A3=((R))
          SX7       A0        5         X7=OLD A0
*                             6 X7
*                             7 X2
 +        SA2       X2        8         X2=IDIM   A2=(IDIM)
          SA7       SAVEA0    9 X4      SAVEA0=OLD A0
          S_NN      X4       10 X1                          NN=N
*                            11 X3
*                            12
 +        LT        NN,ONE,ERROR        IF N<1
*                            14
          BX6       X1       15 X2      X6=(A)
          S_JA      X2       16 X6                          JA=IDIM
 +        BX7       X3       17         X7=(R)
*                            18 X7
          LT        JA,NN,ERROR         IF IDIM<N
 TAU      IFEQ      LENGTH,2
          S_JA      JA+JA                             JA=LENGTH*IDIM
 TAU      ENDIF
          SA6       SAVEA               SAVEA=(A(1,1))
          SA7       SAVER               SAVER=(R(1))
 PROLOG   ENDM                          X1=(A), X3=(R), A3=((R))
