*
* $Id: dzdpar.F,v 1.1.1.1 1996/03/04 16:13:12 mclareni Exp $
*
* $Log: dzdpar.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:12  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE  DZDPAR(LB,IB,L,
     &  HID, NID, HIDUP, HIDNX, JB, NL, NS, ND)
*.
*...DZDPAR    get parameters for a bank
*.
*. INPUT     :  LB, IB   the array containing the ZEBRA store
*.                       as used in the call to MZSTOR
*.              L        link to bank
*. OUTPUT    :  HID      Id of bank
*.              HIDUP    Id of up-bank
*.              HIDNX    Id of next bank
*.              JB       JBIAS link offset
*.              NL,NS,ND number of struc links,tot links,data words
*.
*. CALLS     : UHTOC
*. CALLED    : DZDRA2, DZDHEA
*.
*. AUTHOR    : O.Schaile
*. VERSION   : 1.00
*. CREATED   : 11-Dec-87
*. LAST MOD  : 18-Mar-88
*.
*. Modification Log.
*. 18-Mar-88   implement input from doc file
*.**********************************************************************
*.
#include "dzdprm.inc"
      CHARACTER*4 HID,HIDUP,HIDNX
      INTEGER LB(999), IB(999)
      NS = IB(L-2)
      IF(IFRCAR .EQ. 0)THEN
            ND = IB(L-1)
            NL = IB(L-3)
      ELSE
            ND = IB(L+1)
            NL = IB(L+3)
      ENDIF
      NID =IB(L-5)
      CALL UHTOC(IB(L-4),4,HID,4)
      LUPB = LB(L+1)
      IF(LUPB .GT. 2)THEN
        CALL UHTOC(IB(LUPB-4),4,HIDUP,4)
      ELSE
        HIDUP = 'NONE'
      ENDIF
      IF(LB(L) .NE. 0)THEN
        CALL UHTOC(IB(LB(L)-4),4,HIDNX,4)
      ELSE
        HIDNX = '    '
      ENDIF
      LORIGB = LB(L+2)
      IF(LORIGB .GT. 2 .AND. LUPB .NE. 0)THEN
        NSU = IB(LUPB-2)
        IF(LORIGB .GT. LUPB .OR.
     &       LORIGB .LT. LUPB-NSU)THEN
            JB = -1
        ELSE
            JB = LUPB-LORIGB
        ENDIF
      ELSE
        JB = 0
      ENDIF
      END
**********************************************************************
