*
* $Id: hvtob.F,v 1.1.1.1 1996/03/01 11:38:37 mclareni Exp $
*
* $Log: hvtob.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:37  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.07/08 15/08/95  17.24.50  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE HVTOB(ID,VAL,IBIN,IAXIS)
*.==========>
*.
*.   Convert an axis Value into a Bin number without setting it
*. to the min or max bin number of the histo.
*.
*. ID    : Histogram Id (iD or 2D)
*. VAL   : Axis value (input)
*. IBIN  : Bin number (output)
*. IAXIS : Axis number fo 2d histos only:
*.         IAXIS = 1 : X axis
*.         IAXIS = 2 : Y axis
*.
*..=========>
#include "hbook/hcbook.inc"
#include "hbook/hcbits.inc"
*.___________________________________________
*
      CALL HFIND(ID,'HVTOB')
      CALL HDCOFL
*
*              1D histograms
*
      IF (I1.NE.0) THEN
*
* Equidistant bining
*
         IF(I6.EQ.0)THEN
            DX    = FLOAT(IQ(LCID+KNCX))/(Q(LCID+KXMAX)-Q(LCID+KXMIN))
            IBIN  = INT((VAL-Q(LCID+KXMIN))*DX+1.)
         ELSE
*
* Non equidistant bining
*
            LBINS = LQ(LCID-2)
            DO 10 I=1,IQ(LCID+KNCX)
               IF (VAL.GE.Q(LBINS+I)) IBIN = I
   10       CONTINUE
         ENDIF
*
*              2D histograms
*
      ELSE
         IF (IAXIS.EQ.1) THEN
            DX   = FLOAT(IQ(LCID+KNCX))/(Q(LCID+KXMAX)-Q(LCID+KXMIN))
            IBIN = INT((VAL-Q(LCID+KXMIN))*DX+1.)
         ENDIF
         IF (IAXIS.EQ.2) THEN
            DY   = FLOAT(IQ(LCID+KNCY))/(Q(LCID+KYMAX)-Q(LCID+KYMIN))
            IBIN = INT((VAL-Q(LCID+KYMIN))*DY+1.)
         ENDIF
      ENDIF
*
      END
