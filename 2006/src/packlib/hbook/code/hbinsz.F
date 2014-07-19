*
* $Id: hbinsz.F,v 1.1.1.1 1996/01/16 17:07:32 mclareni Exp $
*
* $Log: hbinsz.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:32  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/01 04/12/92  10.29.36  by  Rene Brun
*-- Author :
      SUBROUTINE HBINSZ(KORDRE)
*.==========>
*.           will provide rounded bin size for all histograms
*.           booked after this call
*..=========> ( R.Brun )
#include "hbook/hcprin.inc"
      CHARACTER*(*) KORDRE
*.___________________________________________
      KBINSZ=0
      CALL UCTOH(KORDRE,KORD,4,4)
      IF(KORD.NE.INO)            KBINSZ=1
      END
