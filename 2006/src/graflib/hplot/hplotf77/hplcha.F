*
* $Id: hplcha.F,v 1.1.1.1 1996/01/19 10:49:58 mclareni Exp $
*
* $Log: hplcha.F,v $
* Revision 1.1.1.1  1996/01/19 10:49:58  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.20/00 26/04/95  09.26.27  by  O.Couet
*-- Author :
      SUBROUTINE HPLCHA(NT,XLOC,YLOC,IDH,XLOCH,YLOCH,ICX,ICY)
*.==========>
*.        To convert a point as returned by the HIGZ routine
*.        IRQLC in histogram coordinates and to return the
*.        value IDH of the latest plotted histogram in selection
*.        transformation NT.
*..=========>
#include "hbook/hcbits.inc"
#include "hplot/hpl0.inc"
#include "hplot/hpl1.inc"
#include "hplot/quest.inc"
      CHARACTER*8 CHPID(15)
      DIMENSION IPID(15)
      LOGICAL HEXIST
*._____________________________
*
      IDH       = 0
      ICX       = 0
      ICY       = 0
      IQUEST(3) = 0
*
*            Convert to histogram coordinates if LOG scales
*
      IW = NT/10
      IF(IW.LE.0.OR.IW.GT.100)RETURN
      IF(IPICT(IW,1).NE.0)THEN
         XLOCH = 10.**XLOC
      ELSE
         XLOCH = XLOC
      ENDIF
      IF(IPICT(IW,2).NE.0)THEN
         YLOCH = 10.**YLOC
      ELSE
         YLOCH = YLOC
      ENDIF
*
      IF(LOZFL)THEN
         CALL IGPICK(NT,XLOC,YLOC,NBLEV,CHPID,IPID,' ')
         IF(NBLEV.LE.1)GOTO 10
         IDH = IPID(1)
         IF(CHPID(1)(1:2).EQ.'1d')THEN
            IF(HEXIST(IDH))THEN
               CALL HXI(IDH,XLOCH,ICX)
            ELSE
               IDH = 0
            ENDIF
         ELSEIF(CHPID(1)(1:2).EQ.'2d')THEN
            IF(HEXIST(IDH))THEN
               CALL HXYIJ(IDH,XLOC,YLOC,ICX,ICY)
            ELSE
               IDH = 0
            ENDIF
         ELSEIF(CHPID(1)(1:6).EQ.'ntuple')THEN
            IQUEST(3) = 1
            ICX       = IPID(2)
         ENDIF
         GOTO 20
      ENDIF
*
*            Get IDH from NT
*
   10 IDH = IPICT(IW,3)
      IF(HEXIST(IDH))THEN
         CALL HFIND(IDH,'HPLCHA')
         CALL HDCOFL
         IF(I1.NE.0)THEN
            CALL HXI(IDH,XLOCH,ICX)
         ELSE
            CALL HXYIJ(IDH,XLOCH,YLOCH,ICX,ICY)
         ENDIF
      ELSE
         IDH=0
      ENDIF
*
   20 IQUEST(1) = 0
      IQUEST(2) = 0
      IF(IPICT(IW,1).NE.0)IQUEST(1) = 1
      IF(IPICT(IW,2).NE.0)IQUEST(2) = 1
*
      END
