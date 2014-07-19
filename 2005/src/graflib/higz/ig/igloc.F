*
* $Id: igloc.F,v 1.1.1.1 1996/02/14 13:10:36 mclareni Exp $
*
* $Log: igloc.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:36  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.10/00 06/06/89  09.13.35  by  O.Couet
* No more special case for DI3000
*-- Author : O.Couet
      SUBROUTINE IGLOC(ICURS,NT,IBN,XNDC,YNDC,XWC,YWC)
*.===========>
*.
*.   This routine returns the locator coordinates in the NDC space and in
*. the WC space. If REDIT.NE.0 Or REDIT.NE.1 the XNDC and YNDC value are
*. always returns like REDIT=1 or 0 but the XWC and YWC values are computed
*. according to the value of REDIT.
*.
*..==========> (O.Couet)
#include "higz/hiatt.inc"
#include "higz/hiques.inc"
*.______________________________________
*
      CALL IRQLC(IDID,ICURS,ISTA,NNT,PX,PY)
      NT=NNT
      IBN=ISTA
      XWC=PX
      YWC=PY
      XNDC=RQUEST(11)
      YNDC=RQUEST(12)
*
      END


