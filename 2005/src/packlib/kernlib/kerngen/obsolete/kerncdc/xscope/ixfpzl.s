*
* $Id: ixfpzl.s,v 1.1.1.1 1996/02/15 17:51:17 mclareni Exp $
*
* $Log: ixfpzl.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:17  mclareni
* Kernlib
*
*
          IDENT IXFPZL
*
* CERN PROGLIB# Z200    IXFPZL          .VERSION KERNCDC  1.16  820129
*
          ENTRY IXFPZL
*
*     FUNCTION IXFPZL (LUN)
*
*        IT RETURNS     1 FOR E.O.R
*                       2 FOR E.O.S
*                       4 FOR E.O.P
*                       8 FOR E.O.I
*
*         PROGRAM TAKEN FROM CERN PROGLIB  15-OCT-77, CHL
*
 TRACE.   VFD   36/6HIXFPZL,24/IXFPZL
 TEMPA0   BSSZ  1
 IXFPZL   EQ    *+1BS17
          MX6   1
          BX1   X1+X6
+RJ=XGETFIT
-         VFD   30/TRACE.
          SX6   A0
          SA6   TEMPA0
          FETCH X1,FP,X6
          SA1   TEMPA0
          AX6   3
          SA0   X1
          EQ    IXFPZL
          END
