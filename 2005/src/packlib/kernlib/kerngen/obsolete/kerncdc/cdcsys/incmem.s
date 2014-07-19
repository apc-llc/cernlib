*
* $Id: incmem.s,v 1.1.1.1 1996/02/15 17:51:34 mclareni Exp $
*
* $Log: incmem.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:34  mclareni
* Kernlib
*
*
          IDENT INCMEM
*
* CERN PROGLIB# Z027    INCMEM          .VERSION KERNCDC  0.1   751115
*
          ENTRY INCMEM
*
*     CALL INCMEM (NEW)
*     N =  INCMEM (NEW)
*
*                    NEW:      NO. OF WORDS INCREASE (+) OR DECREASE (-)
*                               INPUT:  REQUEST TO CHANGE BY NEW
*                              OUTPUT:  ACTUALLY CHANGED BY NEW
*         FUNCTION VALUE:      NEW FIELD LENGTH
*
          VFD   36/6HINCMEM,24/INCMEM
 SVA0     DATA  0
 SVX1     DATA  0
 BEFORE   DATA  0
#include "incmem1.inc"
 AFTER    DATA  0
 INCMEM   DATA  0
          SB7   1              B7= 1
          SX6   A0
          BX7   X1
          SA6   SVA0               SAVE A0
          SA7   A6+B7              SAVE X1
          MX7   0
          SA7   A7+B7              ZERO BEFORE
#include "incmem2.inc"
          MEMORY SCM,BEFORE,RECALL
*
          SB7   1              B7= 1
          SA1   SVX1           X1= ADR(NEW)
          SA2   A1+B7          X2= BEFORE  (LH)
          SA3   A2+B7          X3= MAXFLD
          SA4   X1             X4= NEW
          AX2   30             X2= BEFORE  (RH)
          ZR    X4,RETN
          IX6   X2+X4          X6= BEFORE + NEW = AFTER
#include "incmem7.inc"
          IX5   X6-X3          X5= AFTER - MAXFLD = EXCESS REQUEST
          AX5   59
          BX6   X5*X6              TAKE  AFTER  IF IN-LIMIT
          BX3   -X5*X3             TAKE  MAXFLD IF EX-LIMIY
          BX6   X3+X6          X6= AFTER TO BE USED
          LX6   30             X6= AFTER  (LH)
          SA6   A3+B7              STORE AFTER
          MEMORY SCM,AFTER,RECALL
*
          SA1   SVX1           X1= ADR(NEW)
          SB7   1              B7= 1
          SA2   AFTER          X2= AFTER  (LH)
          SA3   A1+B7          X3= BEFORE (LH)
          AX2   30
          AX3   30
          IX7   X2-X3          X7= NEW
          SA7   X1                 STORE -NEW-
*
 RETN     SA5   A1-B7
          BX6   X2             X6= AFTER AS FUNCTION-VALUE
          SA0   X5                 A0  RESTORED
          EQ    INCMEM
          END
