*
* $Id: mxtrp.s,v 1.1.1.1 1996/02/15 17:51:23 mclareni Exp $
*
* $Log: mxtrp.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:23  mclareni
* Kernlib
*
*
          IDENT MXTRP              10-JUNE-68
*
* CERN PROGLIB# F110    MXTRP           .VERSION KERNCDC  0.1   750820
*
          ENTRY MXTRP
*     SUBROUTINE MXTRP (A,B,I,J)
          VFD   30/5HMXTRP,30/MXTRP
 MXTRP    BSS   1
          SB7   1
          SA2   A1+B7
          SB1   X1
          SX0   B7             X0= 1
          SB2   X2
          MX7   59             X7= -1
          SA4   A2+B7
          SB6   B0                 IA=K
          SA3   X4
          ZR    X3,MXTRP       X3= I
          SA5   A4+B7
          SB3   B6             B3= K
          SA4   X5
          SB4   X4             B4= J
          ZR    X4,MXTRP       X4= J
 LOOP1    SA1   B1+B6
          IX5   X3-X0              LOOP ON I
          SB3   B3+X0          B3= K
 LOOP2    FX7   X7+X0
          IX5   X5-X0
          BX6   X1
          SA1   A1+B4
          SA6   B2+X7              B(IB)=A(IA)
          PL    X5,LOOP2
          SB6   B3                 IA=K
          LT    B3,B4,LOOP1
          EQ    MXTRP
          END
#ifdef CERNLIB_TCGEN_MXTRP
#undef CERNLIB_TCGEN_MXTRP
#endif
