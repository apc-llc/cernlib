*
* $Id: proxim.s,v 1.1.1.1 1996/02/15 17:51:23 mclareni Exp $
*
* $Log: proxim.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:23  mclareni
* Kernlib
*
*
          IDENT PROXIM
*
* CERN PROGLIB# B102    PROXIM          .VERSION KERNCDC  0.1   750820
*
          ENTRY PROXIM
*   FUNCTION PROXIM (ANGL,STAND)
*   RETURNS THAT (ANGL + N*PI2) WHICH IS THE CLOSEST TO STAND
          VFD   36/6HPROXIM,24/PROXIM
 PROXIM   BSS   1
          SA3   X1             X3= ANGL(=A)
          SA2   A1+1
          SA4   X2             X4= STAND
          SA1   PI             X1= PI
          SA2   PI2            X2= 2*PI
*
 LOOP     FX5   X3-X4
          NX0   X5             X3= A - STAND
          ZR    X0,SORT
          BX6   X2
          NG    X0,GOON
          BX0   -X0
          BX6   -X2
 GOON     FX5   X1+X0
          NG    X5,NEG
          EQ    SORT
 NEG      FX7   X3+X6
          NX3   X7
          EQ    LOOP
 SORT     BX6   X3
          EQ    PROXIM
 PI       DATA  3.14159265
 PI2      DATA  6.28318530
          END
#ifdef CERNLIB_TCGEN_PROXIM
#undef CERNLIB_TCGEN_PROXIM
#endif
