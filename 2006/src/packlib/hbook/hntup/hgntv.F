*
* $Id: hgntv.F,v 1.1.1.1 1996/01/16 17:07:57 mclareni Exp $
*
* $Log: hgntv.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:57  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/09 21/07/94  16.14.43  by  Fons Rademakers
*-- Author :    Fons Rademakers   29/01/92
      SUBROUTINE HGNTV(IDN,VAR,NVAR,IDNEVT,IERROR)
*.==========>
*.
*.           Return in the preset addresses (set by HBNAME)
*.           the variables VAR(NVAR) of event (row) IDNEVT.
*.
*.  This routine calls HGNT1. HGNT1 checks the IDD and scans over all
*.  blocks, calling for each block HGNT2. HGNT2 does the actual retrieval.
*.
*..=========> ( A.A.Rademakers )
*
      CHARACTER*(*)  VAR(*)
*
      CALL HGNT1(IDN, '*', VAR, 0, NVAR, IDNEVT, IERROR)
*
      END
