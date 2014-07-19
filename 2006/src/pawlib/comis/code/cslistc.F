*
* $Id: cslistc.F,v 1.1.1.1 1996/02/26 17:16:32 mclareni Exp $
*
* $Log: cslistc.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:32  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/14 29/09/94  17.34.49  by  Vladimir Berezhnoi
*-- Author :    Vladimir Berezhnoi   14/12/93
      INTEGER FUNCTION CSLISTC(PROADDR,NEXT)
***--------------------------------------------------------------
*    returns the address of next called routine
*    for given procedure with proaddr
*
*         NADDR=CSLISTC(proaddr,next)
*
*   for address of first proc called call CSLISTC with next=0
*   if no more procedures in the list     CSLISTC=0
***----------------------------------------------------------------
      INTEGER PROADDR
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/cstab.inc"
#include "comis/cscbwl.inc"
      DATA IPCALLL/0/, LASTPA/0/
      SAVE IPCALLL,    LASTPA
*
*-  proelem=( next, listcalled, proaddress )
*-  calelem=( next, address )
*
      NEXTO=0
      CSLISTC=0
 1    IF(NEXT.GT.0)THEN
        IF( PROADDR.NE.LASTPA)THEN
          NEXTO = NEXT
          NEXT = 0
          GOTO 1
        ENDIF
        IF(IPCALLL.EQ.0)RETURN
        CSLISTC=IQ(IPCALLL+1)
        IPCALLL=IQ(IPCALLL)
        NEXT=NEXT+1
        IF (NEXT .LE. NEXTO) THEN
           CSLISTC=0
           GOTO 1
        ENDIF
      ELSE
        I=LISTCL
   2    IF(I.EQ.0)THEN
          LASTPA=0
          RETURN
        ENDIF
        IF(PROADDR.NE.IQ(I+2))THEN
         I=IQ(I)
         GO TO 2
        ENDIF
        IPCALLL=IQ(I+1)
        LASTPA=PROADDR
        NEXT=1
        GO TO 1
      ENDIF
      END
