*
* $Id: uzero.s,v 1.1.1.1 1996/02/15 17:51:27 mclareni Exp $
*
* $Log: uzero.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:27  mclareni
* Kernlib
*
*
          IDENT UZERO
*
* CERN PROGLIB# V300    UZERO           .VERSION KERNCDC  0.1   750820
*
          ENTRY UZERO
*     SUBROUTINE UZERO (IV,JL,JR)
*        (WRITTEN FOR SCOPE 3.3 - FTN)
          VFD   30/5HUZERO,30/UZERO
 UZERO    BSS   1
          SB7   -1
          SA2   A1-B7
          SB2   X1+B7          B2= START OF IV -1
          SA3   A2-B7
          MX6   0
          SA2   X2             X2= JL
          SA3   X3             X3= JR
          SB7   -B7
 LOOP     IX7   X2-X3              DO INDEX TEST
          SA6   B2+X2              STORE
          SX2   X2+B7              INCREASE INDEX
          NG    X7,LOOP
          EQ    UZERO
          END
#ifdef CERNLIB_TCGEN_UZERO
#undef CERNLIB_TCGEN_UZERO
#endif
