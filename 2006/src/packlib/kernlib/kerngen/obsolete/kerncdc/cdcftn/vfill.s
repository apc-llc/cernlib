*
* $Id: vfill.s,v 1.1.1.1 1996/02/15 17:51:27 mclareni Exp $
*
* $Log: vfill.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:27  mclareni
* Kernlib
*
*
          IDENT VFILL
*
* CERN PROGLIB# F121    VFILL           .VERSION KERNCDC  0.1   750820
*
*        (WRITTEN FOR SCOPE 3.3 - FTN)
*        CALL VFILL(X,N,ALPHA)
*        SETS
*        X(I) = ALPHA   FOR   I = 1,N
*
*
          ENTRY VFILL
          VFD   30/5HVFILL,30/VFILL
 VFILL    DATA  0
          SA4   A1+1
          SA5   A1+2
          SA2   X4             X2= N
          SB6   -1
          ZR    X2,VFILL
          SA3   X5             X3= ALPHA
          BX7   X3
          NG    X2,VFILL
          SB4   X2+B6
 LOOP     SA7   X1+B4
          SB4   B4+B6
          GE    B4,B0,LOOP
          EQ    B0,B0,VFILL
          END
#ifdef CERNLIB_TCGEN_VFILL
#undef CERNLIB_TCGEN_VFILL
#endif
