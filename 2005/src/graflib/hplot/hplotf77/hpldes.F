*
* $Id: hpldes.F,v 1.1.1.1 1996/01/19 10:49:58 mclareni Exp $
*
* $Log: hpldes.F,v $
* Revision 1.1.1.1  1996/01/19 10:49:58  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.04/12 16/01/89  12.08.14  by  O.Couet
*-- Author :
      SUBROUTINE HPLDES(IDH)
*.==========>
*.            Fills the common HPL0 which contains a description
*.            of each windows of the picture (number of
*.            transformation NTWIN, flag LOGXFL and LOGYFL, ...)
*..=========>
#include "hplot/hpl0.inc"
#include "hplot/hpl1.inc"
#include "hplot/hpl2.inc"
#include "hplot/hpl13.inc"
*._____________________________
*
      IF((IOPTS.NE.0).OR.(IOPTU.NE.0).OR.(IOPTP.NE.0))GOTO 999
*----
*     Fills IPICT according to NTWIN, LOGXFL and LOGYFL for each zone.
*----
      IW=NTWIN/10
      IF(IW.LE.100)THEN
        IPICT(IW,1)=0
        IPICT(IW,2)=0
        IPICT(IW,3)=IDH
        IF(LOGXFL)IPICT(IW,1)=1
        IF(LOGYFL)IPICT(IW,2)=1
      ENDIF
*
 999  RETURN
      END

