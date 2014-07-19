*
* $Id: jbytor.s,v 1.1.1.1 1996/02/15 17:51:22 mclareni Exp $
*
* $Log: jbytor.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:22  mclareni
* Kernlib
*
*
          IDENT JBYTOR
*
* CERN PROGLIB# M421    JBYTOR          .VERSION KERNCDC  2.18  870602
*
*         FUNCTION JBYTOR(A,B,LB,NBITS)
*
*         ROUTINE DERIVED FROM JBYTET
*
          ENTRY JBYTOR
          VFD   36/6HJBYTOR,24/JBYTOR
 JBYTOR   DATA  0
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
          AX2   B3,X2              SHIFT BYTE IN B TO RIGHT
          BX2   X0*X2              CLEAR NON BYTE
          BX6   X2+X1              STORE LOGICAL OR
          EQ    JBYTOR
          END
#ifdef CERNLIB_TCGEN_JBYTOR
#undef CERNLIB_TCGEN_JBYTOR
#endif
