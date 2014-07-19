*
* $Id: hfilpr.F,v 1.1.1.1 1996/01/16 17:07:36 mclareni Exp $
*
* $Log: hfilpr.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:36  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.23/01 20/02/95  09.40.34  by  Julian Bunn
*-- Author :
      SUBROUTINE HFILPR(IDD)
*.==========>
*.       Fills projections,slices,bands, of a scatter-plot
*.       from the 2-Dim contents
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcbits.inc"
#include "hbook/hcprin.inc"
*.___________________________________________
      CALL HFIND(IDD,'HFILPR')
      IF(LCID.LE.0)GO TO 99
      CALL HDCOFL
      IF(I230.EQ.0)GO TO 99
      CALL SBIT0(IQ(LCID),5)
      LSCAT=LCONT
      NBS=NB
      IDLAST=IDD
      NCX=IQ(LCID+KNCX)
      XMIN=Q(LCID+KXMIN)
      XMAX=Q(LCID+KXMAX)
      NCY=IQ(LCID+KNCY)
      YMIN=Q(LCID+KYMIN)
      YMAX=Q(LCID+KYMAX)
C
      DX=(XMAX-XMIN)/FLOAT(NCX)
      DY=(YMAX-YMIN)/FLOAT(NCY)
C
      Y=YMIN-0.5*DY
      DO 20 J=1,NCY
         Y=Y+DY
         X=XMIN-0.5*DX
         DO 10 I=1,NCX
            X=X+DX
            NB=NBS
            CONT=HCXY(I,J,1)
            CALL HFIDP(IDD,X,Y,CONT)
  10     CONTINUE
  20  CONTINUE
      IDLAST=0
C
  99  RETURN
      END
