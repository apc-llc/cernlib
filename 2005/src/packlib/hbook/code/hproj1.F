*
* $Id: hproj1.F,v 1.1.1.1 1996/01/16 17:07:46 mclareni Exp $
*
* $Log: hproj1.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:46  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.20/09 27/09/93  08.19.10  by  Rene Brun
*-- Author :
      SUBROUTINE HPROJ1(ID1,IDN,ISEL,UWFUNC,IFROM,ITOM,IVARX)
*.==========>
*.           Fill histogram ID1 using variable number IVARX
*.           from the N-tuple IDN.
*.
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
      COMMON/PAWIDN/IDNEVT,VIDN1,VIDN2,VIDN3,X(522)
      EXTERNAL UWFUNC
*.___________________________________________
*
      CALL HGNPAR(IDN,'HPROJ1')
      IF(LCIDN.LE.0)GO TO 99
      NDIM=IQ(LCIDN+2)
      IF(IVARX.LE.0.OR.IVARX.GT.NDIM)THEN
         CALL HBUG('Error in N-tuple parameters','HPROJ1',IDN)
         RETURN
      ENDIF
      ITO=MIN(ITOM,IQ(LCIDN+3))
*
      DO 10 IDNEVT=IFROM,ITO
         CALL HGNF(IDN,IDNEVT,X,IERROR)
         IF(IERROR.NE.0)GO TO 99
         VIDN1=X(IVARX)
         IF(ISEL.EQ.0)THEN
            CALL HF1(ID1,X(IVARX),1.)
         ELSE
            W=UWFUNC(X,ISEL)
            IF(W.NE.0.)THEN
               CALL HF1(ID1,X(IVARX),W)
            ENDIF
         ENDIF
  10  CONTINUE
*
  99  END
