*
* $Id: hprcon.F,v 1.1.1.1 1996/01/16 17:07:46 mclareni Exp $
*
* $Log: hprcon.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:46  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/01 04/12/92  10.31.10  by  Rene Brun
*-- Author :
      SUBROUTINE HPRCON(ID1,KOPT)
*.==========>
*.           CONTENTS ARE NOT PRINTED IF(IOPT=2HNO)
*..=========> ( R.Brun )
#include "hbook/hcprin.inc"
      CHARACTER*(*) KOPT
*.___________________________________________
      CALL UCTOH(KOPT,IOPT,4,4)
      IF(IOPT.NE.INO)THEN
         CALL HIDOPT(ID1,'PCON')
      ELSE
         CALL HIDOPT(ID1,'NPCO')
      ENDIF
      CALL HOBSOL('HPRCON','NPCO','PCON')
      END
