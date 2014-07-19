*
* $Id: sbit.s,v 1.1.1.1 1996/02/15 17:51:23 mclareni Exp $
*
* $Log: sbit.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:23  mclareni
* Kernlib
*
*
          IDENT SBIT
*
* CERN PROGLIB# M421    SBIT            .VERSION KERNCDC  0.1   750820
*
          ENTRY SBIT
*         SUBROUTINE SBIT (VALUE,WORD,LOC)
*        (WRITTEN FOR SCOPE 3.3 - FTN)
          VFD   24/4HSBIT,36/SBIT
 SBIT     BSS   1
          SA5   A1+2
          SA1   X1             X1= VALUE
          MX6   1
          SA3   X5             X3= LOC
          SA4   A5-1
          LX1   59
          SB3   X3
          BX7   X1*X6              MASK LAST BIT OF VALUE
          SA2   X4             X2= WORD
          LX6   X6,B3
          BX4   -X6*X2             CLEAR BIT IN WORD
          LX1   X7,B3
          BX6   X1+X4
          NO
          SA6   A2
          EQ    SBIT
          END
#ifdef CERNLIB_TCGEN_SBIT
#undef CERNLIB_TCGEN_SBIT
#endif
