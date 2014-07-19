*
* $Id: uswop.s,v 1.1.1.1 1996/02/15 17:51:27 mclareni Exp $
*
* $Log: uswop.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:27  mclareni
* Kernlib
*
*
          IDENT USWOP
*
* CERN PROGLIB# V303    USWOP           .VERSION KERNCDC  0.1   750820
*
          ENTRY USWOP
*     SUBROUTINE USWOP (A,B,N)
*        (WRITTEN FOR SCOPE 3.3 - FTN)
          VFD   30/5HUSWOP,30/USWOP
 USWOP    BSS   1
          SA5   A1+2
          SA4   A1+1
          SA3   X5             X3= N
          SB4   1              B4= 1
          SA1   X1             X1= FIRST A-ELEMENT
          SA2   X4             X2= FIRST B-ELEMENT
          SB5   X3             B5= N
          ZR    X3,USWOP
 LOOP     BX7   X1             X7= A-ELEMENT
          SA1   A1+B4          X1= A-ELEMENT
          LX6   B0,X2          X6= B-ELEMENT
          SA2   A2+B4          X2= B-ELEMENT
          SB5   B5-B4
          SA6   A1-B4
          SA7   A2-B4
          NE    B5,B0,LOOP
          EQ    USWOP              RETURN
          END
#ifdef CERNLIB_TCGEN_USWOP
#undef CERNLIB_TCGEN_USWOP
#endif
