*
* $Id: hplnt.F,v 1.1.1.1 1996/01/19 10:50:09 mclareni Exp $
*
* $Log: hplnt.F,v $
* Revision 1.1.1.1  1996/01/19 10:50:09  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.06/02 29/08/89  11.06.59  by  O.Couet
* X dimension change from 127 to 515
*-- Author :
      SUBROUTINE HPLNT(IDN,ISEL,UWFUNC,IFROM,ITOM,IVARX,IVARY)
*.==========>
*.            DRAWS N-tuple IDN as a scatter-plot
*.            (Transformation NTHIST is selected)
*..=========>
#include "hbook/hcbook.inc"
#include "hplot/hpl2.inc"
#include "hplot/hpl9.inc"
      COMMON/PAWIDN/IDNEVT,X(515)
      COMMON/HPLCSC/U(64),V(64)
      EXTERNAL UWFUNC
*._____________________________
*
*----
*     Transformation NTHIST is selected
*----
      IF(NTWIN.NE.NTHIST)THEN
         CALL ISELNT(NTHIST)
         NTWIN=NTHIST
      ENDIF
*
      CALL HGNPAR(IDN,'HPLNT ')
      IF(LCIDN.LE.0)RETURN
      NDIM=IQ(LCIDN+2)
      IF(IVARX.LE.0.OR.IVARX.GT.NDIM)THEN
         CALL HBUG('Non existing option','HPLNT ',IDN)
         RETURN
      ENDIF
      IF(IVARY.LE.0.OR.IVARY.GT.NDIM)THEN
         CALL HBUG('Non existing option','HPLNT ',IDN)
         RETURN
      ENDIF
*
      NPOINT=0
      ITO=MIN(ITOM,IQ(LCIDN+3))
*
      DO 10 IDNEVT=IFROM,ITO
         CALL HGNF(IDN,IDNEVT,X,IERROR)
         IF(IERROR.NE.0)GO TO 20
         X1=X(IVARX)
         Y1=X(IVARY)
         IF(X1.LT.XMINI)GO TO 10
         IF(X1.GE.XMAXI)GO TO 10
         IF(Y1.LT.YMINI)GO TO 10
         IF(Y1.GE.YMAXI)GO TO 10
         IF(ISEL.NE.0)THEN
            W=UWFUNC(X,1)
            IF(W.LE.0.)GO TO 10
         ENDIF
         NPOINT=NPOINT+1
         U(NPOINT)=X(IVARX)
         V(NPOINT)=X(IVARY)
         IF(NPOINT.EQ.64)THEN
            CALL IPM(NPOINT,U,V)
             NPOINT=0
         ENDIF
  10  CONTINUE
  20  IF(NPOINT.NE.0)CALL IPM(NPOINT,U,V)
*
  99  END

