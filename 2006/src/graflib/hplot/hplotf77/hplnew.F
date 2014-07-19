*
* $Id: hplnew.F,v 1.1.1.1 1996/01/19 10:50:09 mclareni Exp $
*
* $Log: hplnew.F,v $
* Revision 1.1.1.1  1996/01/19 10:50:09  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.18/16 25/08/94  15.46.49  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE HPLNEW
*.==========>
*.            Makes a new picture
*..=========>
#include "hplot/hpl0.inc"
#include "hplot/hpl1.inc"
#include "hplot/hpl13.inc"
#include "hbook/hcbook.inc"
*
      CHARACTER*10 CHPTNO,STRING
*._____________________________
*
*              Request string (CR) if option PTO selected
*
      IF (LOPTO) THEN
         IF ((IPLNUM.GE.1).OR.(IPLNUM.EQ.-1)) CALL HPLPTO(' ',STRING)
      ENDIF
*
*              Create a new picture if necessary
*
      IF (LOZFL) THEN
         IF (LOZFL1) THEN
            PNAME = 'PICT00'
            IF (IZRPIP(PNAME).NE.0) CALL IZPICT(PNAME,'S')
         ELSE
   10       WRITE(CHPTNO,'(I10)')IPLTNO
            NLAST  = LENOCC(CHPTNO)
            NFIRST = 0
   20       NFIRST = NFIRST+1
            IF (CHPTNO(NFIRST:NFIRST).EQ.' ') GOTO 20
            PNAME  = 'PICT'//CHPTNO(NFIRST:NLAST)
            IF (IZRPIP(PNAME).NE.0) THEN
               IPLTNO = IPLTNO+1
               GOTO 10
            ENDIF
         ENDIF
         CALL IZPICT(PNAME,'M')
      ENDIF
*
*              Clear screen
*
      CALL ICLRWK(0,0)
*
*              the global title will be drawn by HPLGTL
*
      LOGTIT = .FALSE.
*
*              Check if there is more than one "LIFE" bank active
*              (unless it is just a change of scale for option "Update".
*
      LP = LQ(LHPLOT-1)
      IF ((LP.NE.0).AND.(.NOT.LOSCAL)) THEN
         CALL SBIT0(IQ(LP),1)
         IF (LQ(LP).NE.0) THEN
            CALL MZDROP(IHDIV,LQ(LP),'L')
         ENDIF
      ENDIF
*
      CALL HPLSCM
*
      END
