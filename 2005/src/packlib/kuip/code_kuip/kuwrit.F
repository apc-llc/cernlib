*
* $Id: kuwrit.F,v 1.1.1.1 1996/03/08 15:32:54 mclareni Exp $
*
* $Log: kuwrit.F,v $
* Revision 1.1.1.1  1996/03/08 15:32:54  mclareni
* Kuip
*
*
#include "kuip/pilot.h"
*CMZ :  2.00/00 05/08/92  16.31.24  by  Alfred Nathaniel
*-- Author :    Alfred Nathaniel   04/08/92
      SUBROUTINE KUWRIT(LUN,LINE)
*
********************************************************************************
*
* Write text line to unit LUN
*
*     Input :
*
      INTEGER LUN
      CHARACTER*(*) LINE
*
********************************************************************************
#if defined(CERNLIB_IBMVM)
      CHARACTER FORM*12,LINE80*80
      LINE80=LINE
      INQUIRE(LUN,FORM=FORM)
      IF(FORM.EQ.'FORMATTED') THEN
         WRITE(LUN,'(A80)') LINE80
      ELSE
         WRITE(LUN,NUM=NBYT) LINE80
      ENDIF

#endif
#if !defined(CERNLIB_IBMVM)
      L=MAX(1,LENOCC(LINE))
      WRITE(LUN,'(A)') LINE(:L)
#endif
*
      END
