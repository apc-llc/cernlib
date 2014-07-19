*
* $Id: dzechf.F,v 1.1.1.1 1996/03/04 16:13:23 mclareni Exp $
*
* $Log: dzechf.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:23  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZECHF
#include "dzeditkeep.inc"
      LUNCOM=LUPCOM
      CALL KUOPEN(LUNCOM,CHCOMP,'OLD',ISTAT)
      IF(ISTAT.NE.0)THEN
         WRITE(LUNOUT,*)' Error opening ',CHCOMP
      ELSE
         CALL DZECHK
      ENDIF
      END
***********************************************************************
