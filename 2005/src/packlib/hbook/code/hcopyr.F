*
* $Id: hcopyr.F,v 1.1.1.1 1996/01/16 17:07:34 mclareni Exp $
*
* $Log: hcopyr.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:34  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :          15/06/95  13.26.31  by  Unknown
*-- Author : J.J.Bunn
      SUBROUTINE HCOPYR(ID1,ID2,CHTITL,IBINX1,IBINX2,IBINY1,IBINY2,
     +                  CHOPT)
*.==========>
*.           Copy Histogram ID1 to a new histogram ID2, only
*.           moving the bin contents from bins IBINX1 to IBINX2, IBINY1 to IBINY2
*.           and setting the title to CHTITL.
*.
*.           ID1        Existing histogram ID to be copied
*.           ID2        New histogram ID to receive copy
*.           CHTITL     New histogram title (or ' ' to receive same as ID1)
*.           IBINX1     Low bin for range in X
*.           IBINX2     High bin for range in X
*.           IBINY1     Low bin for range in Y (if 2D histo, ignored otherwise)
*.           IBINY2     High bin for range in Y (    "     ditto       "      )
*.           CHOPT      ' ' no errors will be copied,
*.                      'E' bin errors will be copied
*.
*..=========> ( J.J.Bunn )
      CHARACTER*(*) CHTITL,CHOPT
      CHARACTER*80 CHID2
      LOGICAL HEXIST,LERRORS
      EXTERNAL HEXIST
*.___________________________________________
      IF(ID2.EQ.ID1) THEN
         CALL HBUG('Identifiers must be different','HCOPYR',ID1)
         RETURN
      ELSE IF(ID2.EQ.0) THEN
         CALL HBUG('New histogram ID must not be 0','HCOPYR',ID1)
         RETURN
      ELSE IF(.NOT.HEXIST(ID1)) THEN
         CALL HBUG('Source histogram does not exist!','HCOPYR',ID1)
         RETURN
      ELSE IF(HEXIST(ID2)) THEN
         CALL HBUG('+Existing Histogram will be replaced','HCOPYR',ID2)
         CALL HDELET(ID2)
      ENDIF
C
C get the booking parameters of the source histogram
C
      CALL HGIVE(ID1,CHID2,NX,XMI,XMA,NY,YMI,YMA,NWT,LOCH)
C
C check that the bin range to be copied is correct
C
      IF(IBINX1.GT.IBINX2) THEN
         CALL HBUG('Bin range in X is bad','HCOPYR',ID1)
         RETURN
      ELSE IF(NY.GT.0.AND.(IBINY1.GT.IBINY2)) THEN
         CALL HBUG('Bin range in Y is bad','HCOPYR',ID1)
         RETURN
      ENDIF
C
C book the new histogram with a new title if necessary
C
      IF(LENOCC(CHTITL).NE.0) CHID2 = CHTITL
      BINWX = (XMA-XMI)/REAL(NX)
      NBINX = IBINX2-IBINX1+1
      IF(NY.GT.0) THEN
         BINWY = (YMA-YMI)/REAL(NY)
         NBINY = IBINY2-IBINY1+1
         CALL HIJXY(ID1,IBINX1,IBINY1,XMINEW,YMINEW)
         XMANEW = XMINEW + REAL(NBINX)*BINWX
         YMANEW = YMINEW + REAL(NBINY)*BINWY
         CALL HBOOK2(ID2,CHID2,NBINX,XMINEW,XMANEW,
     &               NBINY,YMINEW,YMANEW,0.)
      ELSE
         CALL HIX(ID1,IBINX1,XMINEW)
         XMANEW = XMINEW + REAL(NBINX)*BINWX
         CALL HBOOK1(ID2,CHID2,NBINX,XMINEW,XMANEW,0.)
      ENDIF
C
C check the booking was successful
C
      IF(.NOT.HEXIST(ID2)) THEN
         CALL HBUG('Failed to book new histogram','HCOPYR',ID2)
         RETURN
      ENDIF
C
C now copy the required range of bins into the new histogram
C
      LERRORS = INDEX(CHOPT,'E').NE.0 .OR. INDEX(CHOPT,'e').NE.0
      XPOS = XMI - BINWX*0.5
      IXNEW = 0
      DO 10 IX=1,NX
         XPOS = XPOS + BINWX
         IXNEW = IXNEW + 1
C check for 2D
         IF(NY.GT.0) THEN
            YPOS = YMI - BINWY*0.5
            IYNEW = 0
            DO 20 IY=1,NY
               YPOS = YPOS + BINWY
               IYNEW = IYNEW + 1
C
C fill the new histogram with the contents of this cell in the old histogram
C
                  CALL HFILL(ID2,XPOS,YPOS,HIJ(ID1,IX,IY))
C
C store the error on this cell, if requested
C
                  IF(LERRORS)
     &               CALL HPAKES(ID2,IXNEW,IYNEW,HIJE(ID1,IX,IY))
   20       CONTINUE
         ELSE
C
C fill the new histogram with the contents of this bin in the old histogram
C
               CALL HFILL(ID2,XPOS,0.,HI(ID1,IX))
C
C store the error on this bin, if requested
C
               IF(LERRORS) CALL HPAKES(ID2,IXNEW,0,HIE(ID1,IX))
         ENDIF
   10 CONTINUE
C
      END
