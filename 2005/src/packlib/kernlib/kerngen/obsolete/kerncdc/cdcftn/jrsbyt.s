*
* $Id: jrsbyt.s,v 1.1.1.1 1996/02/15 17:51:22 mclareni Exp $
*
* $Log: jrsbyt.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:22  mclareni
* Kernlib
*
*
          IDENT JRSBYT
*
* CERN PROGLIB# M421    JRSBYT          .VERSION KERNCDC  0.1   730801
*
*         FUNCTION JRSBYT(A,X,LX,NBITS)
*
*         ROUTINE WRITTEN BY P. CROZIER FOR FTN
*
          ENTRY JRSBYT
          VFD   36/6HJRSBYT,24/JRSBYT
 JRSBYT   DATA  0
          SB7   -1             B7= -1
          SA2   A1-B7          X2= ADR(X)
          SA3   A2-B7          X3= ADR(LX)
          SA4   A3-B7          X4= ADR(NBITS)
          SA1   X1             X1= A
          SA2   X2             X2= X
          SA3   X3             X3= LX
          SA4   X4             X4= NBITS
          SB3   -61            B3= -61
          MX0   1
          SB5   X4+B7          B5= NBITS-1
          AX0   B5,X0          X0= MASK ON THE LRFT
          SB5   X3+B5          B5= LX+NBITS-1
          LX0   B5,X0          X0= MASK IN POSITION
          SB4   X3+B7          B4= LX-1
          BX6   X0*X2          X6= RESULT SHIFTED
          SB3   X3+B3          B3= LX-61
          LX1   B4,X1          X1= A POSITIONED
          BX1   X0*X1              CLEAR NON BYTE IN A
          BX2   -X0*X2             CLEAR BYTE IN X
          AX6   B3,X6          X6= RESULT JRSBYT
          BX7   X1+X2          X7= X, UPDATED
          SA7   A2                 STORE RESULT X
          EQ    JRSBYT
          END
#ifdef CERNLIB_TCGEN_JRSBYT
#undef CERNLIB_TCGEN_JRSBYT
#endif
