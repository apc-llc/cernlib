*
* $Id: sbytor.s,v 1.1.1.1 1996/02/15 17:51:23 mclareni Exp $
*
* $Log: sbytor.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:23  mclareni
* Kernlib
*
*
          IDENT SBYTOR
*
* CERN PROGLIB# M421    SBYTOR          .VERSION KERNCDC  0.1   730801
*
*         SUBROUTINE SBYTOR(A,X,LX,NBITS)
*
*         ROUTINE WRITTEN BY P. CROZIER FOR FTN
*
          ENTRY SBYTOR
          VFD   36/6HSBYTOR,24/SBYTOR
 SBYTOR   DATA  0
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
          BX1   X0*X1              CLEAR NON BYTE IN A
          LX1   B3,X1              SHIFT A TO LX
          NO
          NO
          BX6   X2+X1              ADD BYTE FROM A TO X
          SA6   A2                 STORE RESULT IN X
          EQ    SBYTOR
          END
#ifdef CERNLIB_TCGEN_SBYTOR
#undef CERNLIB_TCGEN_SBYTOR
#endif
