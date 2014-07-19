*
* $Id: iuweed.s,v 1.1.1.1 1996/02/15 17:51:34 mclareni Exp $
*
* $Log: iuweed.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:34  mclareni
* Kernlib
*
*
          IDENT IUWEED
*
* CERN PROGLIB# N103    IUWEED          .VERSION KERNCDC  0.1   750820
*
          ENTRY IUWEED
*     FUNCTION IUWEED (IVEC,N)
*
*                     IT RETURNS THE RELATIVE ADDRESS WITHIN THE N-ELEME
*         VECTOR IVEC OF THE FIRST ELEMENT CONTAINING A QUANTITY EITHER
*         INDEFINITE OR OUT OF RANGE IF ANY, 0 OTHERWISE.
*
          VFD   36/6HIUWEED,24/IUWEED
 IUWEED   BSS   1
          SA3   A1+1           X3= ADDR (N)
          SB1   X1
          SA1   X1
          BX7   X1
          SA2   X3             X2= N
          SX0   1              X0= 1
          LX6   B0,X0          X6= IUWEED
 LOOP     SB6   X6
          SA1   B1+X6
          ID    X7,IUWEED
          OR    X7,IUWEED
          BX7   X1
          IX6   X6+X0
          IX2   X2-X0
          NZ    X2,LOOP
          MX6   0
          EQ    IUWEED
          END
