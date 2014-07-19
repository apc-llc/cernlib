*
* $Id: ishft.s,v 1.1.1.1 1996/02/15 17:51:21 mclareni Exp $
*
* $Log: ishft.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:21  mclareni
* Kernlib
*
*
          IDENT       ISHFT
*
* CERN PROGLIB# M441    ISHFT           .VERSION KERNCDC  2.18  870602
* ORIG. ?
*
*     FUNCTION ISHFT(I,K)
*
*      COMPASS VERSION OF ANSI/ISA FUNCTION ISHFT
*      I IS AN INTEGER VARIABLE IN THE CALLING SEQUENCE
*      K IS THE SHIFT COUNT - NEGATIVE TO THE RIGHT
*                             POSITIVE TO THE LEFT
*
      ENTRY       ISHFT
          VFD         30/5HISHFT,30/ISHFT
 ISHFT     DATA        0
          SA2         A1+1
          SA2         X2
          SA1         X1
          ZR          X2,ZERO           .TEST ON ZERO SHIFT
          SX5         60
          IX5         X2-X5
          NZ          X5,NE60           .TEST FOR 60 SHIFT
          SX6         B0
          EQ          B0,B0,ISHFT
NE60      MX3         1
          NG          X2,NEG            .TEST FOR NEGATIVE SHIFT
          SB2         X2
          LX1         B2,X1
          SB4         59
          SB4         B4-B2
          AX3         B4,X3             .FORM MASK
          BX6         X1*X3
          EQ          B0,B0,ISHFT
NEG       SB4         X2
          SB2         -B4
          AX1         B2,X1
          SB4         B2-1
          AX3         B4,X3             .FORM MASK
          BX6         -X3*X1
          EQ          B0,B0,ISHFT
 ZERO     BX6         X1
          EQ          B0,B0,ISHFT
*
      END
#ifdef CERNLIB_TCGEN_ISHFT
#undef CERNLIB_TCGEN_ISHFT
#endif
