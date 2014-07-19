*
* $Id: ublank.s,v 1.1.1.1 1996/02/15 17:51:25 mclareni Exp $
*
* $Log: ublank.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:25  mclareni
* Kernlib
*
*
          IDENT UBLANK
*
* CERN PROGLIB# V300    UBLANK          .VERSION KERNCDC  0.1   750820
*
          ENTRY UBLANK
*     SUBROUTINE UBLANK (IV,JL,JR)
*        (WRITTEN FOR SCOPE 3.3 - FTN)
          VFD   36/6HUBLANK,24/UBLANK
 UBLANK   BSS   1
          SB7   -1
          SA2   A1-B7
          SB2   X1+B7          B2= START OF IV -1
          SA3   A2-B7
          SA1   BLANK
          SA2   X2             X2= JL
          SA3   X3             X3= JR
          BX6   X1
          SB7   -B7
 LOOP     IX7   X2-X3              DO INDEX TEST
          SA6   B2+X2              STORE
          SX2   X2+B7              INCREASE INDEX
          NG    X7,LOOP
          EQ    UBLANK
 BLANK    DATA  55555555555555555555B
          END
#ifdef CERNLIB_TCGEN_UBLANK
#undef CERNLIB_TCGEN_UBLANK
#endif
