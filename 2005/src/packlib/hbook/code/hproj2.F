*
* $Id: hproj2.F,v 1.1.1.1 1996/01/16 17:07:46 mclareni Exp $
*
* $Log: hproj2.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:46  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.20/09 27/09/93  08.19.21  by  Rene Brun
*-- Author :
      SUBROUTINE HPROJ2(ID1,IDN,ISEL,UWFUNC,IFROM,ITOM,IVARX,IVARY)
*.==========>
*.           Fill 2-DIM histogram ID1 using variables number IVARX
*.           and IVARY from the N-tuple IDN.
*.
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
      COMMON/PAWIDN/IDNEVT,VIDN1,VIDN2,VIDN3,X(522)
      EXTERNAL UWFUNC
*.___________________________________________
*
      CALL HGNPAR(IDN,'HPROJ2')
      IF(LCIDN.LE.0)GO TO 99
      NDIM=IQ(LCIDN+2)
      IF(IVARX.LE.0.OR.IVARX.GT.NDIM)THEN
         CALL HBUG('Wrong parameters','HPROJ2',IDN)
         RETURN
      ENDIF
      IF(IVARY.LE.0.OR.IVARY.GT.NDIM)THEN
         CALL HBUG('Wrong parameters','HPROJ2',IDN)
         RETURN
      ENDIF
      ITO=MIN(ITOM,IQ(LCIDN+3))
*
      DO 10 IDNEVT=IFROM,ITO
         CALL HGNF(IDN,IDNEVT,X,IERROR)
         IF(IERROR.NE.0)GO TO 99
         VIDN1=X(IVARX)
         VIDN2=X(IVARY)
         IF(ISEL.EQ.0)THEN
            CALL HFILL(ID1,X(IVARX),X(IVARY),1.)
         ELSE
            W=UWFUNC(X,ISEL)
            IF(W.NE.0.)THEN
               CALL HFILL(ID1,X(IVARX),X(IVARY),W)
            ENDIF
         ENDIF
  10  CONTINUE
*
  99  END
