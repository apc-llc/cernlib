*
* $Id: maxfzeml.s,v 1.1.1.1 1996/04/01 15:03:06 mclareni Exp $
*
* $Log: maxfzeml.s,v $
* Revision 1.1.1.1  1996/04/01 15:03:06  mclareni
* Mathlib gen
*
*
#if (defined(CERNLIB_CDC))&&(!defined(CERNLIB_FORTRAN))
          IDENT  MAXFZE
          ENTRY MAXFZE
* FUNCTION MAXFZE(A,N)
 SAVEA0   MACRO
          SX7         A0
          SA0         A1
          SA7         TEMPA0
          ENDM
 RESETA0  MACRO
          SA1         TEMPA0
          SA0         X1
          ENDM
 NAME     VFD 36/0HMAXFZE,24/2
 MAXFZE   DATA        0
          SAVEA0
          SA2         A1+1
          SA2         X2
          SB5         1                .SET B5 FOR TEST
          SB3         X2               .N IN B3
          SX6         B5               .INITIALISE FUNCTION VALUE
          GE          B5,B3,MAXFZER
          SB4         B0               .INITIALISE COUNT B4
          SA1         X1
 LOOP     SB4         B4+B5            .INCREMENT COUNT
          SA2         A1+B4
          EQ          B3,B4,MAXFZER
          FX3         X1-X2            .FLOATING DIFF
          PL          X3,LOOP          .IF POS GO BACK FOR NEXT ELEMENT
          BX1         X2               .REPLACE X1 BY X2
          SX6         B4+B5            .RESET FUNCTION VALUE
          EQ          B0,B0,LOOP             .GO BACK FOR NEXT ELEMENT
 MAXFZER  RESETA0
          EQ          B0,B0,MAXFZE
 TEMPA0   DATA        0
          END
#endif
