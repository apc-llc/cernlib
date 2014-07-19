*
* $Id: fnblan.s,v 1.1.1.1 1996/02/15 17:51:33 mclareni Exp $
*
* $Log: fnblan.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:33  mclareni
* Kernlib
*
*
          IDENT FNBLAN
*
* CERN PROGLIB# Z204    FNBLAN          .VERSION KERNCDC  1.02  771015
*
          ENTRY FNBLAN
*
*     CALL FNBLAN (LUN)      RETURN  LUN  MODIFIED :
*                            CDC FILE-NAME WITH BLANK-FILL
*
*         PROGRAM WRITTEN 15-OCT-77, JZ
*
          VFD   36/6HFNBLAN,24/FNBLAN
 FNBLAN   BSSZ  1
          SA1   X1
          MX2   42
          BX1   X1*X2              KEEP FIRST 7 CHAR.
          MX2   54             X2= 00  IN POS. 10
          SX3   55B            X3= 55  IN POS. 10
 LOOP     BX1   X1+X3              MERGE BLANK INTO POS. N
          LX2   6              X2= 00 IN POS. N = N-1
          LX3   6              X3= 55 IN POS. N
          BX4   -X2*X1         X4= CHAR. N ISOLATED
          ZR    X4,LOOP
          BX6   X1
          SA6   A1
          EQ    FNBLAN
          END
