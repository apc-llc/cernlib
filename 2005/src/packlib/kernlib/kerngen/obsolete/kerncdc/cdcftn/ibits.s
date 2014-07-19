*
* $Id: ibits.s,v 1.1.1.1 1996/02/15 17:51:20 mclareni Exp $
*
* $Log: ibits.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:20  mclareni
* Kernlib
*
*
          IDENT       IBITS
*
* CERN PROGLIB# M441    IBITS           .VERSION KERNCDC  2.18  870602
* ORIG. ?
*
*     FUNCTION IBITS(I,N,L)
*
*      COMPASS VERSION OF MIL-STD-1753 FUNCTION IBITS
*      I IS AN INTEGER VARIABLE IN THE CALLING SEQUENCE
*      N IS THE RIGHTMOST BIT OF THE REQUIRED FIELD
*      L IS THE LENGTH OF THE REQUIRED FIELD
*
      ENTRY       IBITS
          VFD         30/5HIBITS,30/IBITS
 IBITS     DATA        0
          SA2         A1+1
          SA3         A2+1
          SA3         X3
          ZR          X3,ZERO          .TEST ON ZERO FIELD
          SA2         X2
          SA1         X1
          SB3         X3
          SB2         X2
          MX4         1
          SB4         B3-1
          AX4         B4,X4
          SB6         B2+B3
          LX4         B6,X4
          BX6         X4*X1
          AX6         B2,X6
          EQ          B0,B0,IBITS
*
 ZERO     SX6         B0
          EQ          B0,B0,IBITS
      END
#ifdef CERNLIB_TCGEN_IBITS
#undef CERNLIB_TCGEN_IBITS
#endif
