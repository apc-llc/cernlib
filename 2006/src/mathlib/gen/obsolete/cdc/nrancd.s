*
* $Id: nrancd.s,v 1.1.1.1 1996/04/01 15:03:02 mclareni Exp $
*
* $Log: nrancd.s,v $
* Revision 1.1.1.1  1996/04/01 15:03:02  mclareni
* Mathlib gen
*
*
#if (defined(CERNLIB_CDC))&&(!defined(CERNLIB_FORTRAN))
          IDENT  NRAN
          ENTRY NRAN
          ENTRY NRANIN
          ENTRY NRANUT
NAME1     VFD 24/0HNRAN,36/1
*     SUBROUTINE NRAN(VECTOR,LENGTH)
*         GENERATE N UNIFORMLY-DISTRIBUTED RANDOM NUMBERS
*         T. LINDELOF     MAY,1976
NRAN      DATA 0
          SA3 A1+1
          SA2 X3
          SB1 X2
          LE B1,B0,NRAN
          SA5 A1
          MX4 59
          SA2 LAST
          SA3 MULT
NEXT      DX6 X2*X3
          BX2 X6
          NX6 X6
          SA6 X5
          SB1 B1-1
          LE B1,B0,END
          IX5 X5-X4
          EQ NEXT
END       BX6 X2
          SA6 LAST
          EQ NRAN
*     SUBROUTINE NRANIN(OCTAL)
NRANIN    DATA 0
          SA5 X1
          BX6 X5
          SA6 LAST
          EQ NRANIN
*     SUBROUTINE NRANUT(OCTAL)
NRANUT    DATA 0
          SA5 A1
          SA2 LAST
          BX6 X2
          SA6 X5
          EQ NRANUT
LAST      DATA 17170000000000000001B
MULT      DATA 20001170673633457725B
          END
#endif
