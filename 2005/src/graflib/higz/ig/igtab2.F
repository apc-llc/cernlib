*
* $Id: igtab2.F,v 1.1.1.1 1996/02/14 13:10:39 mclareni Exp $
*
* $Log: igtab2.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:39  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.22/08 24/02/95  17.12.10  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IGTAB2(IA,IB,F,T)
#include "higz/hipack.inc"
#include "higz/hihid.inc"
#include "higz/hicont.inc"
      DIMENSION F(3,4),T(4),IXADD(4),IYADD(4)
      REAL IGCELL
      DATA IXADD /0,1,1,0/
      DATA IYADD /0,0,1,1/
*
      IXT   = IA+IXFCHA(1)-1
      IYT   = IB+IYFCHA(1)-1
*
      XSTP2 = XSTP/2.
      YSTP2 = YSTP/2.
      IF(ILOGX.NE.0)THEN
         XLAB1L  = LOG10(XLAB1)
         XLAB2L  = LOG10(XLAB2)
         XVAL1L  = LOG10(XVAL1)
         XVAL2L  = LOG10(XVAL2)
      ELSE
         XLAB1L  = XLAB1
         XLAB2L  = XLAB2
         XVAL1L  = XVAL1
         XVAL2L  = XVAL2
      ENDIF
      IF(ILOGY.NE.0)THEN
         YLAB1L  = LOG10(YLAB1)
         YLAB2L  = LOG10(YLAB2)
         YVAL1L  = LOG10(YVAL1)
         YVAL2L  = LOG10(YVAL2)
      ELSE
         YLAB1L  = YLAB1
         YLAB2L  = YLAB2
         YVAL1L  = YVAL1
         YVAL2L  = YVAL2
      ENDIF
*
      DO 10 I=1,4
         IXA = IXADD(I)
         IYA = IYADD(I)
*
*              Compute the cell position in cartesian coordinates
*              and compute the LOG if necessary
*
         F(1,I) = XLAB1+XSTP2+FLOAT(IA-1+IXA)*XSTP
         F(2,I) = YLAB1+YSTP2+FLOAT(IB-1+IYA)*YSTP
         IF(ILOGX.NE.0)THEN
            F(1,I) = LOG10(F(1,I))
         ENDIF
         IF(ILOGY.NE.0)THEN
            F(2,I) = LOG10(F(2,I))
         ENDIF
*
*              Transform the cell position in the required coordinate system
*
         IF(ISYS.EQ.2)THEN
            F(1,I) = 360.*(F(1,I)-XVAL1L)/(XVAL2L-XVAL1L)
            F(2,I) = (F(2,I)-YLAB1L)/(YLAB2L-YLAB1L)
         ELSEIF(ISYS.EQ.3)THEN
            F(1,I) = 360.*(F(1,I)-XVAL1L)/(XVAL2L-XVAL1L)
         ELSEIF(ISYS.EQ.4)THEN
            F(1,I) = 360.*(F(1,I)-XVAL1L)/(XVAL2L-XVAL1L)
            F(2,I) = 180.*(F(2,I)-YVAL1L)/(YVAL2L-YVAL1L)
         ELSEIF(ISYS.EQ.5)THEN
            F(1,I) = 360.*(F(1,I)-XVAL1L)/(XVAL2L-XVAL1L)
            F(2,I) = (180.-2.*DANG)*(F(2,I)-YVAL1L)/(YVAL2L-YVAL1L)+DANG
         ENDIF
*
*              Get the content of the table. If the X index (ICX) is
*              greater than the X size of the table (NCX), that's mean
*              IGTABL tried to close the surface and in this case the
*              first channel should be used.
*
         ICX               = IXT+IXA
         IF(ICX.GT.NCX)ICX = 1
         IF(IPACK.NE.0)THEN
            F(3,I) = IGCELL(NCX,NCY,REFWRD(1),ICX,IYT+IYA,1)
         ELSE
            F(3,I) = IGCELL(NCX,NCY,REFWRD(IADRES(1)+1),ICX,IYT+IYA,1)
         ENDIF
*
*              The colors on the surface can represent the content or
*              the errors.
*
         IF (IOPTER.NE.0) THEN
            IF(IPACK.NE.0)THEN
               T(I) = IGCELL(NCX,NCY,REFWRD(1),ICX,IYT+IYA,2)
            ELSE
               T(I) = IGCELL(NCX,NCY,REFWRD(IADRES(1)+1),ICX,IYT+IYA,2)
            ENDIF
         ELSE
            T(I) = F(3,I)
         ENDIF
   10 CONTINUE
*
*              LOGZ is required...
*
      IF(IDRGR.EQ.3)THEN
         DO 20 I=1,4
            IF(ILOG.NE.0.AND.ZMAXST.GT.0.)THEN
               F(3,I) = LOG10(ZMAXST)
            ELSE
               F(3,I) = ZMAXST
            ENDIF
   20    CONTINUE
      ENDIF
*
      IF(ISYS.GE.3)THEN
         DO 30 I=1,4
            F(3,I) = ((1.-RINRAD)*((F(3,I)-ZMIN)/(ZMAX-ZMIN)))+RINRAD
   30    CONTINUE
      ENDIF
*
      END
