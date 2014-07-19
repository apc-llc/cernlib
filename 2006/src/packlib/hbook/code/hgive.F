*
* $Id: hgive.F,v 1.1.1.1 1996/01/16 17:07:38 mclareni Exp $
*
* $Log: hgive.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:38  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.21/12 15/03/94  08.59.50  by  Rene Brun
*-- Author :
      SUBROUTINE HGIVE(IDD,CHTITL,NCX,XMIN,XMAX,NCY,YMIN,YMAX,
     +NWT,IDB)
*.==========>
*.           RETURN BOOKING PARAMETERS OF IDD
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcbits.inc"
#include "hbook/hcntpar.inc"
      CHARACTER*(*) CHTITL
*.___________________________________________
      NARG=10
      CALL NOARG(NARG)
      NCX=0
      IF(NARG.GT.5)NCY=0
      IF(NARG.GT.8)NWT=0
      IF(NARG.GT.9)IDB=0
      CALL HFIND(IDD,'HGIVE ')
*
*             IDD DOES NOT EXIST
*
      IF(LCID.LE.0)GO TO 99
*
      CALL HDCOFL
      IF(I4.NE.0)THEN
         IF (IQ(LCID-2) .NE. ZLINK) THEN
            NCX   = IQ(LCID+2)
            IWT   = IQ(LCID+9)+LCID
            NWTIT = IQ(LCID+8)
         ELSE
            NCX   = IQ(LCID+ZNDIM)
            IWT   = IQ(LCID+ZITIT1)+LCID
            NWTIT = IQ(LCID+ZNWTIT)
         ENDIF
         XMIN=0.
         XMAX=0.
         YMIN=0.
         YMAX=0.
      ELSE
         NCX=IQ(LCID+KNCX)
         XMIN=Q(LCID+KXMIN)
         XMAX=Q(LCID+KXMAX)
         IWT=LCID+KTIT1
         IF(I230.NE.0)THEN
            IF(NARG.GT.5)NCY=IQ(LCID+KNCY)
            IF(NARG.GT.6)YMIN=Q(LCID+KYMIN)
            IF(NARG.GT.7)YMAX=Q(LCID+KYMAX)
            IWT=LCID+KTIT2
         ENDIF
         NWTIT=IQ(LCID-1)-IWT+LCID+1
      ENDIF
      IF(NARG.GT.9)IDB=LCID
      IF(NARG.LT.9)GO TO 99
      NWT=NWTIT
      IF(NWT.EQ.0)GO TO 99
      NCH=LEN(CHTITL)
      NWCH=MIN(NCH,4*NWT)
      IF(NCH.GT.0)CHTITL=' '
      CALL UHTOC(IQ(IWT),4,CHTITL,NWCH)
*
  99  RETURN
      END
