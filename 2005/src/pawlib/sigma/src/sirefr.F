*
* $Id: sirefr.F,v 1.1.1.1 1995/12/12 14:36:20 mclareni Exp $
*
* $Log: sirefr.F,v $
* Revision 1.1.1.1  1995/12/12 14:36:20  mclareni
* Imported sources
*
*
#include "sigma/pilot.h"
*CMZ :  1.09/04 26/10/93  09.49.19  by  Carlo E. Vandoni
*-- Author :    Carlo E. Vandoni   30/07/93
      SUBROUTINE SIREFR
#include "sigma/sicsig.inc"
#include "sigma/sicst4.inc"
C
      KK=9
      IF(LINE.LT.10)KK=LINE -1
      KLIN=LINE-KK
      PRINT 2657,(I,KOLTEX(I),I=KLIN,LINE)
2657     FORMAT(I5,'.',1X,A72)
      END
