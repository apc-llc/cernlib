*
* $Id: htitle.F,v 1.1.1.1 1996/01/16 17:07:49 mclareni Exp $
*
* $Log: htitle.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:49  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 21/02/89  16.14.54  by  Rene Brun
*-- Author :
      SUBROUTINE HTITLE(CHTITL)
*.==========>
*.           STORE GLOBAL TITLE IN LGTIT
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcflag.inc"
      CHARACTER*(*) CHTITL
*.___________________________________________
      CALL HBTIT(CHTITL,NWT,NCHT)
      IF(LGTIT.NE.0)THEN
         CALL MZDROP(IHDIV,LGTIT,' ')
         LGTIT=0
      ENDIF
      IF(NWT.EQ.0)GO TO 99
      CALL HSPACE(NWT+20,'HTITLE',0)
      IF(IERR.NE.0)GO TO 99
*
      CALL MZBOOK(IHDIV,LGTIT,LGTIT,1,'HGTI',0,0,NWT,4,-1)
      CALL UCTOH(CHTITL,IQ(LGTIT+1),4,NCHT)
*
  99  RETURN
      END
