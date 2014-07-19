*
* $Id: mxuty.s,v 1.1.1.1 1996/02/15 17:51:23 mclareni Exp $
*
* $Log: mxuty.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:23  mclareni
* Kernlib
*
*
          IDENT MXUTY              10-JUNE-68
*
* CERN PROGLIB# F110    MXUTY           .VERSION KERNCDC  0.1   750820
*
          ENTRY MXUTY
*     SUBROUTINE MXUTY (A,I)
          VFD   30/5HMXUTY,30/MXUTY
 MXUTY    BSS   1
          SB3   1              B3= 1
          MX7   0              X7= 0.
          SA2   A1+B3
          SA3   X2             X3= I
          SX0   B3             X0= 1
          ZR    X3,MXUTY
          IX5   X3-X0
          SB2   B0                 J= 1
          SA2   =1.
          SB4   X3             B4= I
          BX6   X2             X6= (1.0)
          ZR    X5,LOC2            J.E0.1
 LOOP1    SA6   X1+B2              A(J)=1.0
          IX5   X3-X0          X5= I
          SB2   B2+X0              J=J+1
          SB3   B3+X0              INDEX
 LOOP2    IX5   X5-X0              INDEX
          SA7   X1+B2
          SB2   B2+X0              J=J+1
          PL    X5,LOOP2
          LT    B3,B4,LOOP1
 LOC2     SA6   X1+B2
          EQ    MXUTY
          END
#ifdef CERNLIB_TCGEN_MXUTY
#undef CERNLIB_TCGEN_MXUTY
#endif
