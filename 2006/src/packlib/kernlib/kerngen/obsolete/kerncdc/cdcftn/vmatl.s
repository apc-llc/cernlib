*
* $Id: vmatl.s,v 1.1.1.1 1996/02/15 17:51:27 mclareni Exp $
*
* $Log: vmatl.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:27  mclareni
* Kernlib
*
*
          IDENT VMATL
*
* CERN PROGLIB# F121    VMATL           .VERSION KERNCDC  0.1   750820
*
*        CALL VMATL(H,A,X,K,N)
*        SETS
*        X = A * H       WHERE   H IS A MATRIX OF DIMENSION N*K
*                                A IS A VECTOR OF DIMENSION N
*                                X IS A VECTOR OF DIMENSION K
*
*
          ENTRY VMATL
          VFD   30/5HVMATL,30/VMATL
 VMATL    DATA  0
          SB7   1
          SB1   X1
          SA2   A1+B7
          MX6   0
          SA3   A2+B7
          SB6   B0
          SA1   A3+B7
          SA4   X1
          SB3   X3
          SA5   A1+B7
          SB4   X4
          SA1   X5
          SB5   X1
          LE    B4,B0,VMATL
          GT    B5,B0,NEXT
 LOOP1    SA6   B3+B6
          SB6   B6+B7
          LT    B6,B4,LOOP1
          EQ    B0,B0,VMATL
 NEXT     SA1   B1-B7
          SB3   B3-1
          SB1   B0
 LOOP3    SB6   B0
          MX6   0
          SB1   B1+B7
 LOOP2    SA1   A1+B7
          SA4   X2+B6
          RX3   X1*X4
          RX7   X6+X3
          SB6   B6+B7
          NX6   B0,X7
          LT    B6,B5,LOOP2
          SA6   B3+B1
          LT    B1,B4,LOOP3
          EQ    B0,B0,VMATL
          END
#ifdef CERNLIB_TCGEN_VMATL
#undef CERNLIB_TCGEN_VMATL
#endif
