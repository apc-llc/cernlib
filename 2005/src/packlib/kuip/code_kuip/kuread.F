*
* $Id: kuread.F,v 1.1.1.1 1996/03/08 15:32:54 mclareni Exp $
*
* $Log: kuread.F,v $
* Revision 1.1.1.1  1996/03/08 15:32:54  mclareni
* Kuip
*
*
#include "kuip/pilot.h"
*CMZ :  2.00/00 05/08/92  15.56.21  by  Alfred Nathaniel
*-- Author :
      SUBROUTINE KUREAD(LUN,LINE,NCH)
*
********************************************************************************
*
* Read next line from unit LUN
*
*     Input :
*
      INTEGER LUN
*
*     Output:
      CHARACTER*(*) LINE
      INTEGER NCH
*
*     NCH : number of characters or -1 if error/end-of-file
*
********************************************************************************
#if defined(CERNLIB_IBMVM)
      CHARACTER FORM*12
      INQUIRE(LUN,FORM=FORM)
      IF(FORM.EQ.'FORMATTED') THEN
         READ(LUN,'(A)',END=1,ERR=1) LINE
      ELSE
         LINE=' '
         READ(LUN,NUM=NBYT,END=1,ERR=1) LINE
      ENDIF
#endif
#if (defined(CERNLIB_IBMMVS))&&(!defined(CERNLIB_NEWLIB))
      READ(LUN,'(A80)',END=1,ERR=1) LINE
#endif
#if (!defined(CERNLIB_IBMVM))&&(!defined(CERNLIB_IBMMVS)||defined(CERNLIB_NEWLIB))
      READ(LUN,'(A)',END=1,ERR=1) LINE
#endif
      NCH = LENOCC(LINE)
      RETURN
*
 1    CONTINUE
      LINE = ' '
      NCH = -1
*
      END
