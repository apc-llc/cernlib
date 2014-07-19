*
* $Id: mbytet.s,v 1.1.1.1 1996/02/15 17:51:22 mclareni Exp $
*
* $Log: mbytet.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:22  mclareni
* Kernlib
*
*
          IDENT MBYTET
*
* CERN PROGLIB# M421    MBYTET          .VERSION KERNCDC  2.15  850813
* ORIG.  JZ, 12/8/85
*
*         FUNCTION MBYTET (A,B,LB,NBITS)
*
          ENTRY MBYTET
          VFD   36/6HMBYTET,24/MBYTET
 MBYTET   DATA  0
          SB7   -1             B7= -1
          SA2   A1-B7          X2= ADR(X)
          SA3   A2-B7          X2= ADR(LX)
          SA4   A3-B7          X4= ADR(NBITS)
          SA1   X1             X1= A
          SA2   X2             X2= X
          SA4   X4             X4= NBITS
          SA3   X3             X3= LX
          MX0   1                  MASK STARTED
          SB5   X4+B7          B5= NBITS-1
          SB4   X4             B4= NBITS
          AX0   B5,X0              MASK ON THE LEFT
          LX0   B4,X0              MASK ON THE RIGHT
          SB3   X3+B7          B3= LX-1
          BX1   -X0+X1             SET NON BYTE IN A TO ALL ONES
          LX1   B3,X1              SHIFT A TO LX
          BX6   X2*X1              AND BYTE FROM A TO X
          EQ    MBYTET
          END
#ifdef CERNLIB_TCGEN_MBYTET
#undef CERNLIB_TCGEN_MBYTET
#endif
