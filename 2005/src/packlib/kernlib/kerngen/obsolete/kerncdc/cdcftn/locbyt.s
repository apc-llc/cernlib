*
* $Id: locbyt.s,v 1.1.1.1 1996/02/15 17:51:22 mclareni Exp $
*
* $Log: locbyt.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:22  mclareni
* Kernlib
*
*
          IDENT LOCBYT
*
* CERN PROGLIB# M428    LOCBYT          .VERSION KERNCDC  0.1   750820
*
          ENTRY LOCBYT
*
*     J = LOCBYT (IT,VECT,N,INC,L,NBITS)
          VFD   36/6HLOCBYT,24/LOCBYT
 LOCBYT   DATA  0
          SB7   -1             B7= -1
          SA2   A1-B7          X2= ADR(VECT)
          SA3   A2-B7          X3= ADR(N)
          SB2   X2             B2= ADR(VECT)
          SA4   A3-B7          X4= ADR(INC)
          SA2   A4-B7          X2= ADR(L)
          SA5   A2-B7          X5= ADR(NBITS)
          SA4   X4             X4= INC
          SA5   X5             X5= NBITS
          SB4   X4             B4= INC
          SA4   X2             X4= L
          MX0   1
          SB6   X5+B7          B6= NBITS-1
          SB5   X5             B5= NBITS
          AX0   B6,X0              NBITS BITS CREATED
          SA1   X1             X1= IT
          SA3   X3             X3= N
          LX0   B5,X0              NBITS BITS JUSTIFIED
          SB5   X4+B7          B5= L-1
          LX0   B5,X0          X0= POSITIONED MASK OF NBITS  BITS
          SA2   B2             A2= ADR OF FIRST WORD
          LX1   B5,X1          X1= POSITIONED  IT
          ZR    X3,NOPE            N=0
          SB3   X3             B3= N
          SB5   B0             B5= 0
*                              X0  MASK, POSITIONED
*                              X1  IT,   POSITIONED
*                              A2  ADR CURRENT WORD
*                              B3  N
*                              B4  INC
*                              B5  LOOP -  0, INC, 2*INC, ...
 LOOP     BX4   X0*X2              MASK BYT
          SA2   A2+B4              READY NEXT WORD
          BX4   X1-X4
          ZR    X4,FOUND
          SB5   B5+B4
          LT    B5,B3,LOOP
 NOPE     MX6   0                  NOT FOUND
          EQ    LOCBYT
 FOUND    SX6   B5-B7
          EQ    LOCBYT
          END
#ifdef CERNLIB_TCGEN_LOCBYT
#undef CERNLIB_TCGEN_LOCBYT
#endif
