*
* $Id: mcbyt.s,v 1.1.1.1 1996/02/15 17:51:22 mclareni Exp $
*
* $Log: mcbyt.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:22  mclareni
* Kernlib
*
*
          IDENT MCBYT
*
* CERN PROGLIB# M421    MCBYT           .VERSION KERNCDC  2.15  850813
* ORIG.  JZ, 12/8/85
*
*         FUNCTION MCBYT (A,LA,X,LX,NBITS)
*
          ENTRY MCBYT
          VFD   30/5HMCBYT,30/MCBYT
 MCBYT    DATA  0
          SB7   -1             B7= -1
          SA2   A1-B7          X2= ADR(LA)
          SA3   A2-B7          X3= ADR(X)
          SA4   A3-B7              X+= ADR(LX)
          SA5   A4-B7          X5= ADR(NBITS)
          SA2   X2             X2= LA
          SA4   X4             X4= LX
          MX0   1                  MASK STARTED
          SA5   X5             X5= NBITS
          IX2   X4-X2          X2= LX-LA = IDISP DISPLACEMENT A TO X
          SB6   X5+B7          B6= NBITS-1
          BX6   X2             X6= IDISPL
          SX7   60             X7= 60
          SB5   X4+B6          B5= NBITS+LX-1
          AX6   59             X6= 0 OR -0
          SA1   X1             X1= A
          SA3   X3             X3= X
          AX0   B6,X0              MASK ON LEFT
          BX7   X6*X7          X7= 0 OR 60 FOR IDISPL +VE OR -VE
          SB2   X2             B2= IDISPL
          LX0   B5,X0              SHIFT MASK TO LX
          SB2   B2             B2= IDISPL OR IDISPL+60
          BX3   -X0*X3             MASK OUT BYTE IN X
          LX1   B2,X1              SHIFT BYTE IN A TO LX
          BX1   X0*X1              MASK OUT NON BYTE IN A
          BX6   X1+X3              ADD BYTE TO X
          EQ    MCBYT
          END
#ifdef CERNLIB_TCGEN_MCBYT
#undef CERNLIB_TCGEN_MCBYT
#endif
