*
* $Id: hspace.F,v 1.1.1.1 1996/01/16 17:07:48 mclareni Exp $
*
* $Log: hspace.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:48  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 06/09/89  08.13.14  by  Rene Brun
*-- Author :
      SUBROUTINE HSPACE (N,CHROUT,IDD)
*.==========>
*.           ROUTINE CALLED BY SUBROUTINE CHROUT
*.           IERR=0  N WORDS CAN BE USED FOR STORAGE STARTING
*.                   AT ADRESS NEWHIS
*.               =1  NOT ENOUGH SPACE
*..=========> ( R.Brun )
#include "hbook/hcflag.inc"
#include "hbook/hcbook.inc"
      COMMON/QUEST/IQUEST(100)
      CHARACTER*(*) CHROUT
*.___________________________________________
      IDLAST=0
      IERR=0
      CALL MZNEED(IHDIV,N,' ')
      IF(IQUEST(11).LT.0)THEN
         CALL MZNEED(IHDIV,N,'G')
      ENDIF
      IQUEST(1)=0
      IF(IQUEST(11).LT.0)THEN
         CALL HBUG('Not enough space in memory',CHROUT,IDD)
         IERR  =1
      ENDIF
      END
