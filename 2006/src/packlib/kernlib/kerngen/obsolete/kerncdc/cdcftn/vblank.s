*
* $Id: vblank.s,v 1.1.1.1 1996/02/15 17:51:27 mclareni Exp $
*
* $Log: vblank.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:27  mclareni
* Kernlib
*
*
          IDENT VBLANK
*
* CERN PROGLIB# F121    VBLANK          .VERSION KERNCDC  0.1   750820
*
*        (WRITTEN FOR SCOPE 3.3 - FTN)
*         CALL VBLANK (A,N)
*         TO BLANK A VECTOR A OF DIMENSION N
          ENTRY VBLANK
          VFD   36/6HVBLANK,24/VBLANK
 VBLANK   DATA  0
          SA4   A1+1
          SA3   BLANK
          SA2   X4             X2= N
          SB4   -1
          BX6   X3
          ZR    X2,VBLANK
          NG    X2,VBLANK
          SB3   X2+B4
 LOOP     SA6   X1+B3
          SB3   B3+B4
          GE    B3,B0,LOOP
          EQ    B0,B0,VBLANK
 BLANK    DATA  H* *                    *BL
          END
#ifdef CERNLIB_TCGEN_VBLANK
#undef CERNLIB_TCGEN_VBLANK
#endif
