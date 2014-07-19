*
* $Id: fphgrinit.F,v 1.3 1996/05/31 16:24:33 couet Exp $
*
* $Log: fphgrinit.F,v $
* Revision 1.3  1996/05/31 16:24:33  couet
* - Cleaned up useless commented lines.
*
* Revision 1.2  1996/05/31 16:06:49  couet
* - pilot.h was missing
*
* Revision 1.1.1.1  1996/03/01 11:39:09  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.07/00 09/04/95  22.45.12  by  Gregory Kozlovsky
*-- Author :    Gregory Kozlovsky   09/04/95
      SUBROUTINE FPHGRINIT
*.===========> Author: G. Kozlovsky, 1994
*.
*. Initialize graphics for Histo Fit Panel
*..==========>
*
#include "paw/fpcoms.inc"
      IOPTM=INDEX(CHPOPTN,'M')
      CHPOPTN(IOPTM:IOPTM)=' '

      IOPTL=INDEX(CHPOPTN,'L')
      CHPOPTN(IOPTL:IOPTL)='?'

      IOPTB=INDEX(CHPOPTN,'B')
      CHPOPTN(IOPTB:IOPTB)='?'

      IOPTE=INDEX(CHPOPTN,'E')
      CHPOPTN(IOPTE:IOPTE)='?'

      END
