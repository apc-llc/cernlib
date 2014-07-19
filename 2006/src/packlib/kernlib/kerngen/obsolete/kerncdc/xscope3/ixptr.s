*
* $Id: ixptr.s,v 1.1.1.1 1996/02/15 17:51:17 mclareni Exp $
*
* $Log: ixptr.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:17  mclareni
* Kernlib
*
*
          IDENT IXPTRZL
*
* CERN PROGLIB# Z200    IXPTR           .VERSION KERNCDC  1.16  820129
*
          ENTRY IXPTRZL
*
*     FUNCTION IXPTRZL (LUN)
*
 TRACE.   VFD   42/0HIXPTRZL,18/IXPTRZL
 TEMPA0   BSS   1
 IXPTRZL  EQ    *+1BS17
          MX6   1
          BX1   X1+X6
+RJ=XGETFIT
-         VFD   30/TRACE.
          SX6   A0
          SA6   TEMPA0
          FETCH X1,PTL,X6
          SA1   TEMPA0
          SA0   X1
          EQ    IXPTRZL
          END
