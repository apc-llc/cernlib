*
* $Id: fnzero.s,v 1.1.1.1 1996/02/15 17:51:34 mclareni Exp $
*
* $Log: fnzero.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:34  mclareni
* Kernlib
*
*
          IDENT FNZERO
*
* CERN PROGLIB# Z204    FNZERO          .VERSION KERNCDC  1.02  771015
*
          ENTRY FNZERO
*
*     CALL FNZERO (LUN)      RETURN  LUN  MODIFIED :
*                            CDC FILE-NAME WITH ZERO-FILL
*
*         PROGRAM WRITTEN 15-OCT-77, JZ
*
          VFD   36/6HFNZERO,24/FNZERO
 FNZERO   BSSZ  1
          SA1   X1
          MX2   42             X2= 00  IN POS. N=8
          SX3   55B
          LX3   12             X3= 55  IN POS. N=8
 LOOP     BX1   X1*X2              KEEP FIRST N-1 CHAR.
          LX2   6              X2= 00  IN POS. N = N-1
          LX3   6              X3= 55  IN POS. N
          BX4   -X2*X1         X4= CHAR. N ISOLATED
          BX4   X4-X3          X4= CHAR. N  -  55B
          ZR    X4,LOOP
          BX6   X1
          SA6   A1
          EQ    FNZERO
          END
