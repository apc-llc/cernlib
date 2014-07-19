*
* $Id: ufill.s,v 1.1.1.1 1996/02/15 17:51:25 mclareni Exp $
*
* $Log: ufill.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:25  mclareni
* Kernlib
*
*
          IDENT UFILL
*
* CERN PROGLIB# V300    UFILL           .VERSION KERNCDC  0.1   750820
*
          ENTRY UFILL
*     SUBROUTINE UFILL (IV,JL,JR,ISTUFF)
          VFD   30/5HUFILL,30/UFILL
 UFILL    BSS   1
          SA4   A1+3
          SB7   -1
          SA2   A1-B7
          SA4   X4             X4= ISTUFF
          SA5   A2-B7
          BX6   X4
          SA2   X2             X2= JL
          SA3   X5             X3= JR
          SB2   X1+B7          B2= START OF IV -1
          SB7   -B7
 LOOP     IX7   X2-X3
          SA6   B2+X2
          SX2   X2+B7
          NG    X7,LOOP
          EQ    UFILL
          END
#ifdef CERNLIB_TCGEN_UFILL
#undef CERNLIB_TCGEN_UFILL
#endif
