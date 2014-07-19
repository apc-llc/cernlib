*
* $Id: dzdgst.F,v 1.1.1.1 1996/03/04 16:13:08 mclareni Exp $
*
* $Log: dzdgst.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:08  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDGST(CTEMP,X,Y,CS,NC,IWDISP)
*.
*...DZDGST Get a character string from keyboard
*.
*. INPUT     :  CTEMP  Initial string
*.              X, Y   Coordinates for initial string
*.              CS     character size
*.              IWDISP workstation Id
*. OUTPUT    :  CTEMP  the typed string
*.              NC     number of characters returned
*.
*. CALLS     : DZDTXT GINST  GQCNTN GQNT   GQWKT  GRQST  GSTXCI
*. CALLED    : DZDISP
*.
*. AUTHOR    : O.Schaile
*. VERSION   : 1.00
*. CREATED   : 11-Dec-87
*.*********************************************************************
*.
      CHARACTER*(*) CTEMP
      SAVE INIFLG
      DATA INIFLG/0/
*---
      IFC = INDEXC(CTEMP,' ')
      IF(IFC .LE. 0)IFC = 1
      NC = LEN(CTEMP) - IFC + 1
      CALL ISTXCI(0)
      CALL DZDTXT(CTEMP(IFC:),NC,X,Y,CS,0.,2)
      CALL ISTXCI(1)
      CTEMP = ' '
      IDEV = 1
      CALL IRQST(IWDISP,IDEV,ISTAT,NCC,CTEMP)
      NC = INDXBC(CTEMP,' ')
      CALL DZDTXT(CTEMP(1:NC),NC,X,Y,CS,0.,2)
      END
