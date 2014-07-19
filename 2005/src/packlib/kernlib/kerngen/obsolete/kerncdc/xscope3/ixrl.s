*
* $Id: ixrl.s,v 1.1.1.1 1996/02/15 17:51:17 mclareni Exp $
*
* $Log: ixrl.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:17  mclareni
* Kernlib
*
*
          IDENT IXRLZL
*
* CERN PROGLIB# Z200    IXRL            .VERSION KERNCDC  1.16  820129
*
          ENTRY IXRLZL
*
*     FUNCTION IXRLZL (LUN)
*
 TEMPA0   BSS   1
 TRACE.   VFD   36/0HIXRLZL,24/IXRLZL
 IXRLZL   EQ    *+1BS17
          MX6   1
          BX1   X1+X6
 +        RJ     =XGETFIT.
 -        VFD    30/TRACE.
         SX6          A0
         SA6          TEMPA0
          FETCH       X1,RL,X6
         SA1          TEMPA0
         SA0          X1
          EQ    IXRLZL
          END
