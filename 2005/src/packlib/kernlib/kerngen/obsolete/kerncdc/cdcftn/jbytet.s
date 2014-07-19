*
* $Id: jbytet.s,v 1.1.1.1 1996/02/15 17:51:22 mclareni Exp $
*
* $Log: jbytet.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:22  mclareni
* Kernlib
*
*
          IDENT JBYTET
*
* CERN PROGLIB# M421    JBYTET          .VERSION KERNCDC  0.1   730801
*
*         FUNCTION JBYTET(A,B,LB,NBITS)
*
*         ROUTINE WRITTEN BY P. CROZIER FOR FTN
*
          ENTRY JBYTET
          VFD   36/6HJBYTET,24/JBYTET
 JBYTET   DATA  0
          SB7   -1             B7= -1
          SA2   A1-B7          X2= ADR(B)
          SA3   A2-B7          X3= ADR(LB)
          SA4   A3-B7          X4= ADR(NBITS)
          SA1   X1             X1= A
          SA2   X2             X2= B
          SA4   X4             X4= NBITS
          SA3   X3             X3= LB
          MX0   1                  FORM MASK
          SB4   X4             B4= NBITS
          SB5   X4+B7          B5= NBITS-1
          AX0   B5,X0              MASK ON LEFT
          SB3   X3-61          B3= LB-61
          LX0   B4,X0              MASK ON RIGHT
          BX1   X0*X1              CLEAR NON BYTE IN A
          AX2   B3,X2              SHIFT BYTE IN B TO RIGHT
          NO
          NO
          BX6   X2*X1              STORE LOGICAL SUM OF BYTES
          EQ    JBYTET
          END
#ifdef CERNLIB_TCGEN_JBYTET
#undef CERNLIB_TCGEN_JBYTET
#endif
