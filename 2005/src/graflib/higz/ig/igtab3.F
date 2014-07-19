*
* $Id: igtab3.F,v 1.1.1.1 1996/02/14 13:10:39 mclareni Exp $
*
* $Log: igtab3.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:39  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.22/08 23/02/95  11.56.08  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IGTAB3(IA,IB,NV,AB,VV,T)
#include "higz/hipack.inc"
#include "higz/hicont.inc"
#include "higz/hihid.inc"
      DIMENSION AB(2,4),VV(*),T(4,*)
      REAL IGCELL
*
      IXT = IA+IXFCHA(1)-1
      IYT = IB+IYFCHA(1)-1
*
*              Compute the cell position in cartesian coordinates
*              and compute the LOG if necessary
*
      AB(1,1) = XLAB1+(FLOAT(IA)-1.+RLEGBO)*XSTP
      AB(1,2) = AB(1,1)+XSTP*RLEGBW
      AB(2,1) = YLAB1+(FLOAT(IB)-1.+RLEGBO)*YSTP
      AB(2,3) = AB(2,1)+YSTP*RLEGBW
      IF(ILOGX.NE.0)THEN
         AB(1,1) = LOG10(AB(1,1))
         AB(1,2) = LOG10(AB(1,2))
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
         AB(2,1) = LOG10(AB(2,1))
         AB(2,3) = LOG10(AB(2,3))
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
*              Transform the cell position in the required coordinate system
*
      IF(ISYS.EQ.2)THEN
         AB(1,1) = 360.*(AB(1,1)-XVAL1L)/(XVAL2L-XVAL1L)
         AB(1,2) = 360.*(AB(1,2)-XVAL1L)/(XVAL2L-XVAL1L)
         AB(2,1) = (AB(2,1)-YLAB1L)/(YLAB2L-YLAB1L)
         AB(2,3) = (AB(2,3)-YLAB1L)/(YLAB2L-YLAB1L)
      ELSEIF(ISYS.EQ.3)THEN
         AB(1,1) = 360.*(AB(1,1)-XVAL1L)/(XVAL2L-XVAL1L)
         AB(1,2) = 360.*(AB(1,2)-XVAL1L)/(XVAL2L-XVAL1L)
      ELSEIF(ISYS.EQ.4)THEN
         AB(1,1) = 360.*(AB(1,1)-XVAL1L)/(XVAL2L-XVAL1L)
         AB(1,2) = 360.*(AB(1,2)-XVAL1L)/(XVAL2L-XVAL1L)
         AB(2,1) = 180.*(AB(2,1)-YVAL1L)/(YVAL2L-YVAL1L)
         AB(2,3) = 180.*(AB(2,3)-YVAL1L)/(YVAL2L-YVAL1L)
      ELSEIF(ISYS.EQ.5)THEN
         AB(1,1) = 360.*(AB(1,1)-XVAL1L)/(XVAL2L-XVAL1L)
         AB(1,2) = 360.*(AB(1,2)-XVAL1L)/(XVAL2L-XVAL1L)
         AB(2,1) = (180.-2.*DANG)*(AB(2,1)-YVAL1L)/(YVAL2L-YVAL1L)+DANG
         AB(2,3) = (180.-2.*DANG)*(AB(2,3)-YVAL1L)/(YVAL2L-YVAL1L)+DANG
      ENDIF
*
*              Complete the cell coordinates
*
      AB(2,2) = AB(2,1)
      AB(1,3) = AB(1,2)
      AB(1,4) = AB(1,1)
      AB(2,4) = AB(2,3)
*
*              Get the content of the table, and loop on the
*              stack if necessary.
*
      IL=ILOG
      ILOG=0
      VV(1)=ZMIN
      IF(IPACK.NE.0)THEN
         VV(2)=IGCELL(NCX,NCY,REFWRD(1),IXT,IYT,1)
      ELSE
         VV(2)=IGCELL(NCX,NCY,REFWRD(IADRES(1)+1),IXT,IYT,1)
      ENDIF
      IF(NIDS.NE.0)THEN
         DO 10 I=2,NIDS+1
            IXT=IA+IXFCHA(I)-1
            IYT=IB+IYFCHA(I)-1
            IF(IPACK.NE.0)THEN
               VV(I+1)=IGCELL(NCX,NCY,REFWRD(I),IXT,IYT,1)+VV(I)
            ELSE
               VV(I+1)=IGCELL(NCX,NCY,REFWRD(IADRES(I)+1),IXT,IYT,1)
     +                       +VV(I)
            ENDIF
            VV(I+1)=MAX(ZMIN,VV(I+1))
   10    CONTINUE
      ENDIF
*
      NV=NIDS+2
      ILOG=IL
      DO 20  I=2,NV
         IF(ILOG.NE.0)THEN
            VV(I)=MAX(ZMIN,LOG10(VV(I)))
         ELSE
            VV(I)=MAX(ZMIN,VV(I))
         ENDIF
   20 CONTINUE
*
      IF(ILOG.EQ.0)THEN
   30    I=3
   40    IF(I.LE.NV)THEN
            IF(VV(I).LT.VV(I-1))THEN
               VV(I-1)=VV(I)
               GOTO 30
            ENDIF
            I=I+1
            GOTO 40
         ENDIF
      ENDIF
*
*              For cylindrical, spherical and pseudo-rapidity, the content
*              is mapped onto the radius
*
      IF(ISYS.GE.3)THEN
         DO 50  I=1,NV
            VV(I)=((1.-RINRAD)*((VV(I)-ZMIN)/(ZMAX-ZMIN)))+RINRAD
   50    CONTINUE
      ENDIF
*
      DO 70 I=1,NV
         DO 60 J=1,4
            T(J,I)=VV(I)
   60    CONTINUE
   70 CONTINUE
*
      END
