*
* $Id: xzvers.F,v 1.5 1996/04/12 07:40:07 cernlib Exp $
*
* $Log: xzvers.F,v $
* Revision 1.5  1996/04/12 07:40:07  cernlib
* new handling of cspack title
*
* Revision 1.4  1996/03/29 11:33:55  jamie
* qftitlch
*
* Revision 1.3  1996/03/29 10:59:28  jamie
* print 'title' more like patchy
*
* Revision 1.2  1996/03/28 11:13:39  jamie
* update idatqq/itimqq
*
* Revision 1.1.1.1  1996/03/08 15:44:32  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE XZVERS
      IDATQQ = 960329
      ITIMQQ = 1200
#include "cspack/qftitlch.inc"
      PRINT *,
     + CspackTitleFortranString
      PRINT *,'         This version created on ',IDATQQ,
     +        ' at ',ITIMQQ
      END
 
