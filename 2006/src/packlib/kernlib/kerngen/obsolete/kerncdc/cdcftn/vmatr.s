*
* $Id: vmatr.s,v 1.1.1.1 1996/02/15 17:51:27 mclareni Exp $
*
* $Log: vmatr.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:27  mclareni
* Kernlib
*
*
          IDENT VMATR
*
* CERN PROGLIB# F121    VMATR           .VERSION KERNCDC  0.1   750820
*
*        CALL VMATR(A,G,X,N,K)
*        SETS
*        X = G * A       WHERE   G IS A MATRIX OF DIMENSION K*N
*                                A IS A VECTOR OF DIMENSION N
*                                X IS A VECTOR OF DIMENSION K
*
*
          ENTRY VMATR
          VFD   30/5HVMATR,30/VMATR
 VMATR    DATA  0
          SB7   1
          SB1   X1
          SA2   A1+B7
          SB2   X2
          SA1   A2+B7
          SB3   X1
          SA2   A1+B7
          MX6   0
          SA4   X2
          SB6   B0
          SA3   A2+B7
          SB4   X4
          SA5   X3
          SB5   X5
          LE    B5,B0,VMATR
          GT    B4,B0,NEXT
 LOOP1    SA6   B3+B6
          SB6   B6+B7
          LT    B6,B5,LOOP1
          EQ    B0,B0,VMATR
 NEXT     SX4   B2
          SB6   B0
 LOOP3    SB5   B5-B7
          SB2   X4+B5
          MX6   0
 LOOP2    SA2   B2
          SA1   B1+B6
          RX3   X1*X2
          RX7   X6+X3
          SB6   B6+B7
          NX6   B0,X7
          SB2   B2+X5
          LT    B6,B4,LOOP2
          SA6   B3+B5
          SB6   B0
          GT    B5,B0,LOOP3
          EQ    B0,B0,VMATR
          END
#ifdef CERNLIB_TCGEN_VMATR
#undef CERNLIB_TCGEN_VMATR
#endif
