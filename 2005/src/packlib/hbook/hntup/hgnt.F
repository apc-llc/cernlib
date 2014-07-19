*
* $Id: hgnt.F,v 1.1.1.1 1996/01/16 17:07:57 mclareni Exp $
*
* $Log: hgnt.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:57  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/09 21/07/94  16.12.17  by  Fons Rademakers
*-- Author :    Fons Rademakers   29/01/92
      SUBROUTINE HGNT(IDN,IDNEVT,IERROR)
*.==========>
*.
*.           Return in the preset addresses (set by HBNAME)
*.           the variables of event (row) IDNEVT.
*.
*.  This routine calls HGNT1. HGNT1 checks the IDD and scans over all
*.  blocks, calling for each block HGNT2. HGNT2 does the actual retrieval.
*.
*..=========> ( A.A.Rademakers )
*
      CALL HGNT1(IDN, '*', '*', 0, 0, IDNEVT, IERROR)
*
      END
