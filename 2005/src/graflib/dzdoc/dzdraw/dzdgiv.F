*
* $Id: dzdgiv.F,v 1.1.1.1 1996/03/04 16:13:18 mclareni Exp $
*
* $Log: dzdgiv.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:18  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDGIV(CV,IV,NCV)
      CHARACTER*(*) CV
*     try to read an INTEGER from CV
      CHARACTER*4 CHFORM
      IV=0
      NCV=0
*     first non blank
      IFNB=INDEXC(CV,' ')
      ILC=LENOCC(CV)
*     last numeric
      NCV=INDXNC(CV(IFNB:))+IFNB-2
      IF(NCV.LE.0)NCV=ILC+IFNB-1
      IF(NCV.LT.IFNB)GOTO 999
      WRITE(CHFORM,'(A,I1,A)') '(I',NCV-IFNB+1,')'
      READ(CV(IFNB:NCV),CHFORM,ERR=999)IV
999   END
***********************************************************************
