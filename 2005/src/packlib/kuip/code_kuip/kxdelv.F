*
* $Id: kxdelv.F,v 1.1.1.1 1996/03/08 15:32:54 mclareni Exp $
*
* $Log: kxdelv.F,v $
* Revision 1.1.1.1  1996/03/08 15:32:54  mclareni
* Kuip
*
*
#include "kuip/pilot.h"
*CMZ :  1.61/06 19/07/89  14.18.49  by
** Calling KUVDEL instead of KXDLV1
*-- Author :
      SUBROUTINE KXDELV
*
********************************************************************************
*
* Execution routine for command '/VECTOR/DELETE'
*
********************************************************************************
*
#include "kuip/kcvect.inc"
      CHARACTER*32 VNAME
*
      CALL KUGETC(VNAME,N)
      IF (VNAME.EQ.'*') THEN
        CALL KUVDEL('*')
        GO TO 999
      ENDIF
      IF (NUMVEC.LE.1) THEN
        CALL KUALFA
        PRINT *,'*** No vector defined'
        GO TO 999
      ENDIF
10    CALL KUGETL(VNAME,N)
      IF (N.GT.0) THEN
        CALL KUVDEL(VNAME)
        GO TO 10
      ENDIF
999   END


