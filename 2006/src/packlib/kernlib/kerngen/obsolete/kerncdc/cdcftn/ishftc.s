*
* $Id: ishftc.s,v 1.1.1.1 1996/02/15 17:51:21 mclareni Exp $
*
* $Log: ishftc.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:21  mclareni
* Kernlib
*
*
          IDENT       ISHFTC
*
* CERN PROGLIB# M441    ISHFTC          .VERSION KERNCDC  2.18  870602
* ORIG. ?
*
*     FUNCTION ISHFTC(I,K,L)
*
*      COMPASS VERSION OF MIL-STD-1753 ISHFTC FUNCTION
*      I IS AN INTEGER VARIABLE IN THE CALLING SEQUENCE
*      K IS THE SHIFT COUNT - NEGATIVE TO THE RIGHT
*                             POSITIVE TO THE LEFT
*      L IS THE NUMBER OF THE RIGHTMOST BITS TO BE SHIFTED, ALL
*         OTHER BITS ARE LEFT UNCHANGED
*
      ENTRY       ISHFTC
          VFD         36/6HISHFTC,24/ISHFTC
 ISHFTC     DATA        0
          SA2         A1+1
          SA3         A2+1
          SA2         X2
          SA1         X1
          ZR          X2,ZERO           .TEST ON ZERO SHIFT
          SB1         1
          MX4         1
          SA3         X3
          SB3         X3
          SB2         X2
          SX5         B3-B2
          ZR          X5,ZERO           .TEST ON SHIFT SAME AS FIELD
          SX5         60
          IX5         X5-X3
          NG          X2,NEG            .TEST FOR NEGATIVE SHIFT
          NZ          X5,NE60           .TEST FOR 60 FIELD
          LX6         B2,X1
          EQ          B0,B0,ISHFTC
*      SWAP K BITS WITH L-K BITS IN L RIGHTMOST FIELD,
*      RETAINING UPPER 60-L BITS
 NE60     SB4         X5
          SB4         B4-B1
          AX4         B4,X4
          BX6         X4*X1              .UNCHANGED PART
 JUMP     MX4         1
          SB4         B2-B1
          AX4         B4,X4
          LX4         B3,X4
          BX5         X4*X1
          SB5         B3-B2              .L-K
          AX5         B5,X5
          BX6         X6+X5              .THE K BITS
          MX4         1
          SB7         B5-B1
          AX4         B7,X4
          LX4         B5,X4
          BX5         X4*X1
          LX5         B2,X5
          BX6         X6+X5              .THE L-K BITS
          EQ          B0,B0,ISHFTC
*      END OF SWAP
 NEG      SB2         B3+B2              . L-ABS(K)
          SX6         B0
          ZR          X5,JUMP            .TEST FOR 60 FIELD
          EQ          B0,B0,NE60
 ZERO     BX6         X1
          EQ          B0,B0,ISHFTC
*
      END
#ifdef CERNLIB_TCGEN_ISHFTC
#undef CERNLIB_TCGEN_ISHFTC
#endif
