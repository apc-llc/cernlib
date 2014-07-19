*
* $Id: poweze.s,v 1.1.1.1 1996/02/15 17:51:23 mclareni Exp $
*
* $Log: poweze.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:23  mclareni
* Kernlib
*
*
          IDENT POWEZE
*
* CERN PROGLIB# B400    POWEZE          .VERSION KERNCDC  1.05  780503
*
          ENTRY POWEZE
*
* SUBROUTINE POWEZE(X,A,N)
*
 NAME     VFD   36/6HPOWEZE,24/POWEZE
 POWEZE   BSSZ  1              X1= ADR(X)
          SB7   1              B7= 1
          SA2   A1+B7          X2= ADR(A)
          SA5   A2+B7          X5= ADR(N)
*
          SB2   X2             B2= ADR(A)
          SA3   X1             X3= X
          SA1   X5             X1= N
*
          SA2   ONE                LOAD 1.0 IN X2
          SB3   59
          SB4   B7+B7
          LX0   X1,B3              NEG X0 = ODD  POS = EVEN
          SB6   X1+B2              ADDR OF A(N+1) IN B6
          BX6   X2
          SA6   B2                 STORE 1.0 IN A(1)
          SB6   B6+B7              ADDR OF A(N+1)+1 IN B6
          FX2   X3*X3              X**2 IN X2
          PL    X0,DOWN            IF EVEN DO NOT STORE A(2)
          SB2   B2+B7              ADDR OF A(2) IN B2
          BX6   X3                 X IN X6
          SA6   B2                 STORE X IN A(2)
 DOWN     FX7   X6*X2              FIRST PAIR**********
          FX6   X6*X3              N EVEN  A(2)=X    A(3)=X**2
          SB3   B2+B7              N ODD   A(3)=X**2 A(4)=X**3
          SB5   B2+B4
          EQ    B3,B6,POWEZE       TEST FOR I=1
 LOOP     SA6   B3                 STORE IN A(J)
          SA7   B5                 STORE IN A(J+1)
          FX4   X6*X2              NEXT X**(J)
          FX5   X7*X2              NEXT X**(J+1)
          SB3   B3+B4              INCR STORE ADDR BY 2
          SB5   B5+B4              INCR STORE ADDR BY 2
          BX6   X4
          LX7   X5,B0
          NE    B3,B6,LOOP         TEST IF LAST A HAS BEEN STORED
          EQ    POWEZE
 ONE      DATA  1.0
          END
#ifdef CERNLIB_TCGEN_POWEZE
#undef CERNLIB_TCGEN_POWEZE
#endif
