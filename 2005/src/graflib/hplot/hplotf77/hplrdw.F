*
* $Id: hplrdw.F,v 1.1.1.1 1996/01/19 10:50:11 mclareni Exp $
*
* $Log: hplrdw.F,v $
* Revision 1.1.1.1  1996/01/19 10:50:11  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.20/00 13/04/95  11.13.43  by  O.Couet
*-- Author :
      SUBROUTINE HPLRDW(NT,LPT,XAX,YAX)
*.==========>
*.       Redraw histogram and axis in the same zone than the previous
*.       one, in case scale has changed for option "Update".
*.       parameter:
*.                   NT= number of transformation
*.                   LPT= pointer in ZEBRA structure.
*..=========>
#include "hplot/hpl1.inc"
#include "hplot/hpl2.inc"
#include "hbook/hcbook.inc"
#include "hplot/quest.inc"
      LOGICAL LBAR
      CHARACTER*8 CHOPTG
      DIMENSION XAX(3),YAX(3)
*._____________________________
*
      CALL IGQ('LWID',RLWID)
      CALL IGQ('FAIS',RFAIS)
      CALL IGQ('FASI',RFASI)
      CALL IGQ('FACI',RFACI)
      CALL IGQ('PLCI',RPLCI)
      CALL IGQ('BORD',RBORD)
*----
*    Select the right transformation (histogram transformation)
*----
      CALL ISELNT(NT)
      NTWIN=NT
      NTHIST=NTWIN
*----
*     Redraw histogram
*----
      IF(LOZFL)THEN
         NWNEED=2*IQ(LPT+6)+100
         CALL MZNEED(IXHIGZ,NWNEED,'G')
         IF(IQUEST(11).LT.0)THEN
            CALL HBUG('No space to store the picture','HPLRDW',0)
            LOZFL=.FALSE.
            CALL IGZSET('G')
         ENDIF
      ENDIF
      LIY=LPT+19
      LIX=LIY+IQ(LPT+6)
*
      IHTOLD=IHTYP
*
      LBAR=.FALSE.
      CALL UHTOC(IQ(LPT+9),4,CHOPTG,4)
      CALL UOPTC(CHOPTG,'B',IOPTG)
      IF(IOPTG.NE.0)LBAR=.TRUE.
      IF(LBAR)THEN
         RBARO=Q(LPT+10)
         RBARW=Q(LPT+11)
         CALL IGSET('BARO',RBARO)
         CALL IGSET('BARW',RBARW)
      ENDIF
      IF((LBAR).AND.(IHTYP.EQ.0))IHTYP=1000
*
      CALL HPLATT(1)
*
      NB          = IQ(LPT+6)
      IQUEST(81)  = LIX+1
      IQUEST(82)  = LIY+1
      CHOPTG(8:8) = 'Z'
      CALL IGHIST(NB,Q(LIX+1),Q(LIY+1),CHOPTG)
*----
*     Draw axes
*----
      CALL HPLAXI(XAX,YAX,1)
*
      IHTYP=IHTOLD
      CALL IGSET('BARO',ROFFS)
      CALL IGSET('BARW',RWDTH)
      CALL IGSET('LWID',RLWID)
      CALL IGSET('FAIS',RFAIS)
      CALL IGSET('FASI',RFASI)
      CALL IGSET('FACI',RFACI)
      CALL IGSET('PLCI',RPLCI)
      CALL IGSET('BORD',RBORD)
*
      END
