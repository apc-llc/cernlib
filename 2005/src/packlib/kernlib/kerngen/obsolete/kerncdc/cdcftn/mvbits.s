*
* $Id: mvbits.s,v 1.1.1.1 1996/02/15 17:51:22 mclareni Exp $
*
* $Log: mvbits.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:22  mclareni
* Kernlib
*
*
          IDENT       MVBITS
*
* CERN PROGLIB# M441    MVBITS          .VERSION KERNCDC  2.18  870602
* ORIG. ?
*
*     SUBROUTINE MVBITS(I,N,L,J,M)
*
*      COMPASS VERSION OF MIL-STD-1753 SUBROUTINE MVBITS
*      I AND J ARE INTEGERS IN THE CALLING SEQUENCE
*      N IS THE POSITION IN I FROM WHICH BITS ARE EXTRACTED
*      L IS THE NUMBER OF BITS TO BE EXTRACTED
*      M IS THE POSITION IN J INTO WHICH BITS ARE TO BE PLACED
*
      ENTRY       MVBITS
          VFD         36/6HMVBITS,24/MVBITS
 MVBITS     DATA        0
          SA2         A1+1
          SA3         A2+1
          SA0         A3
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
          AX6         B2,X6             .BITS EXTRACTED, NOW INSERT INTO J
          SA4         A0+1
          SA5         A4+1
          SA4         X4
          SA5         X5
          SB5         X5
          LX6         B5,X6      .BITS POSITIONED
          MX0         1
          SB7         59
          SB7         B7-B3
          AX0         B7,X0
          LX0         B5,X0      .MASK OF 60-L-M (UPPER) AND M (LOWER)
          BX4         X0*X4
          BX6         X6+X4
          SA6         A4
          EQ          B0,B0,MVBITS
*
 ZERO     SX6         B0
          SA4         A0+1
          SA6         X4
          EQ          B0,B0,MVBITS
      END
#ifdef CERNLIB_TCGEN_MVBITS
#undef CERNLIB_TCGEN_MVBITS
#endif
