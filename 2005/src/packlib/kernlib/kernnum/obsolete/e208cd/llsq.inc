*
* $Id: llsq.inc,v 1.1.1.1 1996/02/15 17:47:59 mclareni Exp $
*
* $Log: llsq.inc,v $
* Revision 1.1.1.1  1996/02/15 17:47:59  mclareni
* Kernlib
*
*
*
* llsq.inc
*
*CALL     BASIC
*CALL     LSQ
 MAIN     MACRO     N,I,J,ONE
          HEADER    LLSQ
          EXIT      LLSQ                (N,X,Y,C1,C2,IFAIL)
 +        SA2       X1                  X2=N      A2=(N)
          S_ONE     1                                       ONE=1
          SA4       A1+ONE              X4=(X)    A4=((X))
          SX6       -2                  X6=-2 (=IFAIL IF N<2)
          SA5       A4+ONE              X5=(Y)    A5=((Y))
          S_I       A5+ONE                                  I=((C1))
          S_N       X2                                      N=N
          LE        N,ONE,FAIL2         IF N<2
          RVLLSQ    FAIL1,N,X4,ONE,X5,ONE,J       X6=C1, X7=C2
          SA1       I                   X1=(C1)   A1=((C1))
          SA2       I+ONE               X2=(C2)   A2=((C2))
          SA3       A2+ONE              X3=(IFAIL) A3=((IFAIL))
          SA6       X1                  C1=C1
          MX6       0                   X6=0 (=IFAIL IF O.K.)
          SA7       X2                  C2=C2
          SA6       X3                  IFAIL=0
          EPILOG
 FAIL1    SX6       -1                  X6=-1 (=IFAIL IF DET .LE. 0)
 FAIL2    SA3       I+2                 X3=(IFAIL) A3=((IFAIL))
          SA6       X3                  IFAIL=-1,-2
          JP        EPILOG
          ENDM
