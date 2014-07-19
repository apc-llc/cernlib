*
* $Id: irnd01ml.s,v 1.1.1.1 1996/04/01 15:03:02 mclareni Exp $
*
* $Log: irnd01ml.s,v $
* Revision 1.1.1.1  1996/04/01 15:03:02  mclareni
* Mathlib gen
*
*
#if defined(CERNLIB_CDC)
          IDENT IRND01
          ENTRY IRND01                          .I=IRND01(DUMMY)
          ENTRY IR01IN                          .CALL IR01IN(SEED)
          ENTRY IR01UT                          .CALL IR01UT(SEED)
* FUNCTION IRND01(DUMMY)
* REVISED BY T LINDELOF, JAN 1980 (ENTRIES IR01IN AND IR01UT ADDED)
 NAME     VFD    36/6HIRND01,24/1
 IRND01   DATA   0
          SA1 SEED
          SB2 7
          MX3 25
          SA2 C
          LX0 B2,X1
          SB3 34
          IX1 X1+X0
          IX1 X1+X2
          BX7 -X3*X1
          AX6 B3,X7
          SA7 SEED
          EQ IRND01
 NAME1    VFD 36/0HIR01UT,24/1
 IR01UT   DATA   0
          SA1 X1
          SA2 SEED
          BX6 X2
          SA6 A1
          EQ IR01UT
 NAME2    VFD 36/0HIR01IN,24/1
 IR01IN   DATA   0
          SA1 X1
          BX6 X1
          SA6 SEED
          EQ  IR01IN
 SEED     DATA   200000000000B
 C        DATA   311715164025B
          END
#endif
