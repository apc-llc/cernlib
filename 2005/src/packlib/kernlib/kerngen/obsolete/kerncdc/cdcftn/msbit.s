*
* $Id: msbit.s,v 1.1.1.1 1996/02/15 17:51:22 mclareni Exp $
*
* $Log: msbit.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:22  mclareni
* Kernlib
*
*
          IDENT MSBIT
*
* CERN PROGLIB# M421    MSBIT           .VERSION KERNCDC  2.15  850813
* ORIG.  JZ, 12/8/85
*
*         FUNCTION MSBIT (VALUE,WORD,LOC)
*
          ENTRY MSBIT
          VFD   30/5HMSBIT,30/MSBIT
 MSBIT    BSS   1
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
          EQ    MSBIT
          END
#ifdef CERNLIB_TCGEN_MSBIT
#undef CERNLIB_TCGEN_MSBIT
#endif
