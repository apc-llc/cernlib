*
* $Id: vzero.s,v 1.1.1.1 1996/02/15 17:51:28 mclareni Exp $
*
* $Log: vzero.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:28  mclareni
* Kernlib
*
*
          IDENT VZERO
*
* CERN PROGLIB# F121    VZERO           .VERSION KERNCDC  0.1   750820
*
*        (WRITTEN FOR SCOPE 3.3 - FTN)
*        CALL VZERO(X,N)
*        SETS
*        X(I) = 0   FOR   I = 1,N
*
*
          ENTRY VZERO
          VFD   30/5HVZERO,30/VZERO
 VZERO    DATA  0
          SA4   A1+1
          SB4   -1
          SA2   X4             X2= N
          MX6   0
          SB3   X2+B4
          ZR    X2,VZERO
          NG    X2,VZERO
 LOOP     SA6   X1+B3
          SB3   B3+B4
          GE    B3,B0,LOOP
          EQ    B0,B0,VZERO
          END
#ifdef CERNLIB_TCGEN_VZERO
#undef CERNLIB_TCGEN_VZERO
#endif
