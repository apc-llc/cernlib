*
* $Id: bitpos.s,v 1.1.1.1 1996/02/15 17:51:19 mclareni Exp $
*
* $Log: bitpos.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:19  mclareni
* Kernlib
*
*
          IDENT       BITPOS
*
* CERN PROGLIB# M508    BITPOS          .VERSION KERNCDC  2.18  870602
* ORIG. ?
*
*     CALL BITPOS(I,N,K,M)
*     I=INPUT VECTOR
*     N=NO. OF BITS TO BE EXAMINED
*     K= OUTPUT ARRAY OF POSITIONS IN VECTOR OF SET BITS
*     M= NO. OF SET BITS
*
      ENTRY       BITPOS
          VFD         36/6HBITPOS,24/BITPOS
 BITPOS   DATA        0
          SB1         1
          SX5         B1
          SA2         A1+B1
          SX7         B0               . SET M
          SA1         X1               . GET I
          SA3         A2+B1            . GET BASE ADDRESS OF K
          SA4         A3+B1
          SA2         X2
          SB4         B1               . LOOP INDEX
          SB2         X2               . GET N
          SB6         60
          SB3         B6
          NZ          X1,LOOP
          SB4         B6                . EMPTY WORD
          SB3         B1
          EQ          L1
 LOOP     BX2         X1*X5            . GET BIT
          ZR          X2,L1
          SX6         B4-B1
          IX7         X7+X5            . INCREMENT M
          SA6         X3               . STORE K(M)
          IX3         X3+X5
 L1       GE          B4,B2,OUT
          SB3         B3-B1
          SB4         B4+B1
          NE          B3,L2            . CHECK WHETHER END OF WORD
          SA1         A1+B1
          SB3         B6
          NZ          X1,LOOP
          SB4         B4-B1
          SB3         B1
          SB4         B4+B6
          EQ          L1
 L2       AX1         B1               . SHIFT I
          NZ          X1,LOOP
          NG          X1,LOOP
          SB4         B4+B3            . NO MORE SET BITS IN THIS WORD
          SB4         B4-B1
          SB3         B1
          EQ          L1
 OUT      SA7         X4               . STORE M
          EQ          B0,B0,BITPOS
      END
#ifdef CERNLIB_TCGEN_BITPOS
#undef CERNLIB_TCGEN_BITPOS
#endif
