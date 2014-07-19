*
* $Id: hplbox.F,v 1.1.1.1 1996/01/19 10:49:58 mclareni Exp $
*
* $Log: hplbox.F,v $
* Revision 1.1.1.1  1996/01/19 10:49:58  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.20/00 13/04/95  14.14.32  by  O.Couet
*-- Author :
      SUBROUTINE HPLBOX(XSF,YSF,XEF,YEF,KICASE)
*.==========>
*.            draws a rectangular box on the picture
*.            (Transformation NTHIST or 1 is selected
*.            according to KICASE)
*..=========>
#include "hplot/hpl1.inc"
#include "hplot/hpl2.inc"
#include "hplot/hpl4.inc"
#include "hplot/hpl9.inc"
      LOGICAL LERR1
      CHARACTER*(*) KICASE
*._____________________________
*
      XS = XSF
      YS = YSF
      XE = XEF
      YE = YEF
*
      CALL IGQ('FAIS',RFAIS)
      CALL IGQ('FASI',RFASI)
      CALL IGQ('FACI',RFACI)
      CALL IGQ('PLCI',RPLCI)
      CALL IGQ('LWID',RLWID)
      CALL IGQ('BORD',RBORD)
      CALL ISCLIP(0)
*----
*     Ensure (XS,YS) is lower left hand point
*----
      IF (XS.GT.XE) THEN
         T  = XS
         XS = XE
         XE = T
      ENDIF
      IF (YS.GT.YE) THEN
         T  = YS
         YS = YE
         YE = T
      ENDIF
*----
*     Select the NT according to KICASE
*----
      LERR1 = .FALSE.
      LOCM  = .FALSE.
      IF (KICASE(1:1).EQ.'C') LOCM = .TRUE.
      IF (LOCM) THEN
         IF(NTWIN.NE.1)CALL ISELNT(1)
         NTWIN = 1
         GOTO 10
      ELSE
         IF (NTWIN.NE.NTHIST) CALL ISELNT(NTHIST)
         NTWIN = NTHIST
      ENDIF
*----
*     Check the validy of the arguments in case of LOG scale
*----
      IF (LOGXFL) THEN
         IF (XS.LE.0.) GOTO 20
         IF (XE.LE.0.) GOTO 20
         XS = LOG10(XS)
         XE = LOG10(XE)
      ENDIF
      YS = YS*FACTOR
      YE = YE*FACTOR
      IF (LOGYFL) THEN
         IF (YMINI.LE.0.0) GOTO 20
         IF (YS.LE.0.0)    GOTO 20
         IF (YE.LE.0.0)    GOTO 20
         YS = LOG10(YS)
         YE = LOG10(YE)
      ENDIF
*----
*     Check if the box is not outside the picture limits
*----
   10 IF (LOCM) THEN
         XS = MAX(XS,0.)
         XE = MIN(XE,XSIZ)
         YS = MAX(YS,0.)
         YE = MIN(YE,YSIZ)
      ELSE
         XS = MAX(XS,XMINI)
         XE = MIN(XE,XMAXI)
         YS = MAX(YS,YMINI)
         YE = MIN(YE,YMAXI)
      ENDIF
      IF ((XS.GT.XE).OR.(YS.GT.YE)) LERR1 = .TRUE.
*----
*     Drawing of the box
*----
      IF (IBTYP.NE.0) THEN
         IABTYP = IABS(IBTYP)
         IBFAIS = IABTYP/1000-1
         IF (IBFAIS.LT.0) IBFAIS = 3
         CALL ISFAIS(IBFAIS)
         IBFASI = MOD(IABTYP,1000)
         IF(IBTYP.LT.0) IBFASI = -IBFASI
         IF ((IBFASI.NE.0).AND.(IBFAIS.GE.2)) CALL ISFASI(IBFASI)
         IF (IBCOL.NE.0) CALL ISFACI(IBCOL)
      ELSE
         IBFAIS = 0
         CALL ISFAIS(0)
         CALL ISFACI(1)
      ENDIF
      IF (IBBCOL.NE.0) THEN
         CALL ISPLCI(IBBCOL)
      ELSE
         CALL ISPLCI(IBCOL)
      ENDIF
      CALL ISLWSC(FLOAT(IBWID))
      CALL IGSET('BORD',1.)
      IF (KICASE(3:3).EQ.'S'.AND.ISBCOL.NE.0) THEN
         DZ=0.2+(0.1/IXWIN)
         IF(IBFAIS.EQ.0) ISBOX = 0
         IF(IBFAIS.EQ.1) ISBOX = 1000+IBCOL
         IF(IBFAIS.GE.2) ISBOX = IBFASI
         CALL IGPAVE(XS,XE,YS,YE,DZ,ISBOX,1000+ISBCOL,'TR')
      ELSE
         CALL IGBOX(XS,XE,YS,YE)
      ENDIF
*
      GOTO 30
*
   20 CALL HBUG('Routine called with zero or negative argument with log
     +scale','HPLBOX',0)
   30 IF(LERR1) CALL HBUG('User has called a routine to plot outside the
     + picture limits','HPLBOX',0)
*
      CALL ISCLIP(1)
      CALL IGSET('FAIS',RFAIS)
      CALL IGSET('FASI',RFASI)
      CALL IGSET('FACI',RFACI)
      CALL IGSET('PLCI',RPLCI)
      CALL IGSET('LWID',RLWID)
      CALL IGSET('BORD',RBORD)
*
      END
