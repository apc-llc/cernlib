*
* $Id: invert.s,v 1.1.1.1 1996/04/01 15:03:20 mclareni Exp $
*
* $Log: invert.s,v $
* Revision 1.1.1.1  1996/04/01 15:03:20  mclareni
* Mathlib gen
*
*
          IDENT  INVERT
          ENTRY INVERT
* FUNCTION INVERT(I,K)
#include "entftn.inc"
 NAME     VFD    36/6HINVERT,24/2
          ENTFTN  INVERT,2
          SA3         B2               .LOAD BIT COUNT IN X3
          SA1         B1               .LOAD WORD TO BE REVERSED IN X1
          NO
          SX6         B0               .CLEAR X6
          SB7         1                .SET DECREMENT
          SB2         X3               .SET BIT COUNT
          SX0         B7               .SET UP MASK
 LOOP     LX6         1                .MAKE ROOM IN X6
          BX2         X0*X1            .EXTRACT ONE BIT
          SB2         B2-B7            .DECREMENT BIT COUNT
          AX1         1                .RIGHT ADJUST NEXT BIT
          BX6         X2+X6            .MERGE IN NEW BIT
          NZ     B2,LOOP
          JP          INVERT
          END
