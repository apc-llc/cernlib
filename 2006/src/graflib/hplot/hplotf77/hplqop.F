*
* $Id: hplqop.F,v 1.1.1.1 1996/01/19 10:50:11 mclareni Exp $
*
* $Log: hplqop.F,v $
* Revision 1.1.1.1  1996/01/19 10:50:11  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.20/05 27/10/95  11.40.19  by  O.Couet
*-- Author :    O.Couet   25/10/95
      SUBROUTINE HPLQOP(CHOPT,R)
*
* Routine used by the PAW function $OPTION
*
#include "hplot/hpl1.inc"
#include "hplot/hpl3.inc"
#include "hbook/hcopt.inc"
      CHARACTER*(*) CHOPT
*
      R = 0.
*
      IF (INDEX(CHOPT(1:4),'LINX').NE.0) THEN
         IF (LOGX.EQ.0) R=1.
         RETURN
      ENDIF
      IF (INDEX(CHOPT(1:4),'LINY').NE.0) THEN
         IF (LOGY.EQ.0) R=1.
         RETURN
      ENDIF
      IF (INDEX(CHOPT(1:4),'LINZ').NE.0) THEN
         IF (LOGZ.EQ.0) R=1.
         RETURN
      ENDIF
      IF (INDEX(CHOPT(1:4),'LOGX').NE.0) THEN
         IF (LOGX.EQ.1) R=1.
         RETURN
      ENDIF
      IF (INDEX(CHOPT(1:4),'LOGY').NE.0) THEN
         IF (LOGY.EQ.1) R=1.
         RETURN
      ENDIF
      IF (INDEX(CHOPT(1:4),'LOGZ').NE.0) THEN
         IF (LOGZ.EQ.1) R=1.
         RETURN
      ENDIF
      IF (INDEX(CHOPT(1:4),'ZFL1').NE.0) THEN
         IF (LOZFL1) R=1.
         RETURN
      ENDIF
      IF (INDEX(CHOPT(1:3),'ZFL').NE.0) THEN
         IF (LOZFL) R=1.
         RETURN
      ENDIF
      IF (INDEX(CHOPT(1:4),'NZFL').NE.0) THEN
         IF (.NOT.LOZFL1.AND..NOT.LOZFL) R=1.
         RETURN
      ENDIF
      IF (INDEX(CHOPT(1:4),'HSTA').NE.0) THEN
         IF (ISTAF.EQ.1) R=1.
         RETURN
      ENDIF
      IF (INDEX(CHOPT(1:4),'HNST').NE.0) THEN
         IF (ISTAF.EQ.0) R=1.
         RETURN
      ENDIF
      IF (INDEX(CHOPT(1:4),'VERT').NE.0) THEN
         IF (LOVERT) R=1.
         RETURN
      ENDIF
      IF (INDEX(CHOPT(1:4),'HORI').NE.0) THEN
         IF (.NOT.LOVERT) R=1.
         RETURN
      ENDIF
      IF (INDEX(CHOPT(1:3),'EAH').NE.0) THEN
         IF (LOEAH) R=1.
         RETURN
      ENDIF
      IF (INDEX(CHOPT(1:4),'NEAH').NE.0) THEN
         IF (.NOT.LOEAH) R=1.
         RETURN
      ENDIF
      IF (INDEX(CHOPT(1:3),'CHA').NE.0) THEN
         IF (LOCHA) R=1.
         RETURN
      ENDIF
      IF (INDEX(CHOPT(1:4),'NCHA').NE.0) THEN
         IF (.NOT.LOCHA) R=1.
         RETURN
      ENDIF
      IF (INDEX(CHOPT(1:3),'AST').NE.0) THEN
         IF (LOASTK) R=1.
         RETURN
      ENDIF
      IF (INDEX(CHOPT(1:4),'NAST').NE.0) THEN
         IF (.NOT.LOASTK) R=1.
         RETURN
      ENDIF
      IF (INDEX(CHOPT(1:4),'SOFT').NE.0) THEN
         IF (LOSOFT) R=1.
         RETURN
      ENDIF
      IF (INDEX(CHOPT(1:4),'HARD').NE.0) THEN
         IF (.NOT.LOSOFT) R=1.
         RETURN
      ENDIF
      IF (INDEX(CHOPT(1:3),'SQR').NE.0) THEN
         IF (LOSQR) R=1.
         RETURN
      ENDIF
      IF (INDEX(CHOPT(1:4),'NSQR').NE.0) THEN
         IF (.NOT.LOSQR) R=1.
         RETURN
      ENDIF
      IF (INDEX(CHOPT(1:4),'UTIT').NE.0) THEN
         IF (LOUTIT) R=1.
         RETURN
      ENDIF
      IF (INDEX(CHOPT(1:4),'HTIT').NE.0) THEN
         IF (.NOT.LOUTIT) R=1.
         RETURN
      ENDIF
      IF (INDEX(CHOPT(1:3),'TAB').NE.0) THEN
         IF (LOTAB) R=1.
         RETURN
      ENDIF
      IF (INDEX(CHOPT(1:4),'NTAB').NE.0) THEN
         IF (.NOT.LOTAB) R=1.
         RETURN
      ENDIF
      IF (INDEX(CHOPT(1:3),'BOX').NE.0) THEN
         IF (LOBOX) R=1.
         RETURN
      ENDIF
      IF (INDEX(CHOPT(1:4),'NBOX').NE.0) THEN
         IF (.NOT.LOBOX) R=1.
         RETURN
      ENDIF
      IF (INDEX(CHOPT(1:3),'TIC').NE.0) THEN
         IF (LOTIC) R=1.
         RETURN
      ENDIF
      IF (INDEX(CHOPT(1:4),'NTIC').NE.0) THEN
         IF (.NOT.LOTIC) R=1.
         RETURN
      ENDIF
      IF (INDEX(CHOPT(1:3),'STA').NE.0) THEN
         IF (LOSTAT) R=1.
         RETURN
      ENDIF
      IF (INDEX(CHOPT(1:4),'NSTA').NE.0) THEN
         IF (.NOT.LOSTAT) R=1.
         RETURN
      ENDIF
      IF (INDEX(CHOPT(1:3),'FIT').NE.0) THEN
         IF (LOFIT) R=1.
         RETURN
      ENDIF
      IF (INDEX(CHOPT(1:4),'NFIT').NE.0) THEN
         IF (.NOT.LOFIT) R=1.
         RETURN
      ENDIF
      IF (INDEX(CHOPT(1:3),'PTO').NE.0) THEN
         IF (LOPTO) R=1.
         RETURN
      ENDIF
      IF (INDEX(CHOPT(1:4),'NPTO').NE.0) THEN
         IF (.NOT.LOPTO) R=1.
         RETURN
      ENDIF
      IF (INDEX(CHOPT(1:3),'BAR').NE.0) THEN
         IF (LOBAR) R=1.
         RETURN
      ENDIF
      IF (INDEX(CHOPT(1:4),'NBAR').NE.0) THEN
         IF (.NOT.LOBAR) R=1.
         RETURN
      ENDIF
      IF (INDEX(CHOPT(1:4),'DVXI').NE.0) THEN
         IF (LODVXI) R=1.
         RETURN
      ENDIF
      IF (INDEX(CHOPT(1:4),'DVXR').NE.0) THEN
         IF (.NOT.LODVXI) R=1.
         RETURN
      ENDIF
      IF (INDEX(CHOPT(1:4),'DVYI').NE.0) THEN
         IF (LODVYI) R=1.
         RETURN
      ENDIF
      IF (INDEX(CHOPT(1:4),'DVYR').NE.0) THEN
         IF (.NOT.LODVYI) R=1.
         RETURN
      ENDIF
      IF (INDEX(CHOPT(1:4),'GRID').NE.0) THEN
         IF (LOGRID) R=1.
         RETURN
      ENDIF
      IF (INDEX(CHOPT(1:4),'NGRI').NE.0) THEN
         IF (.NOT.LOGRID) R=1.
         RETURN
      ENDIF
      IF (INDEX(CHOPT(1:4),'DATE').NE.0) THEN
         IF (LODATE) R=1.
         RETURN
      ENDIF
      IF (INDEX(CHOPT(1:4),'NDAT').NE.0) THEN
         IF (.NOT.LODATE) R=1.
         RETURN
      ENDIF
      IF (INDEX(CHOPT(1:4),'FILE').NE.0) THEN
         IF (LOFILE) R=1.
         RETURN
      ENDIF
      IF (INDEX(CHOPT(1:4),'NFIL').NE.0) THEN
         IF (.NOT.LOFILE) R=1.
         RETURN
      ENDIF
      IF (INDEX(CHOPT(1:2),'A0').NE.0) THEN
         IF (IVSIZE.EQ.0) R=1.
         RETURN
      ENDIF
      IF (INDEX(CHOPT(1:2),'A1').NE.0) THEN
         IF (IVSIZE.EQ.1) R=1.
         RETURN
      ENDIF
      IF (INDEX(CHOPT(1:2),'A2').NE.0) THEN
         IF (IVSIZE.EQ.2) R=1.
         RETURN
      ENDIF
      IF (INDEX(CHOPT(1:2),'A3').NE.0) THEN
         IF (IVSIZE.EQ.3) R=1.
         RETURN
      ENDIF
      IF (INDEX(CHOPT(1:2),'A4').NE.0) THEN
         IF (IVSIZE.EQ.4) R=1.
         RETURN
      ENDIF
      IF (INDEX(CHOPT(1:2),'A5').NE.0) THEN
         IF (IVSIZE.EQ.5) R=1.
         RETURN
      ENDIF
      IF (INDEX(CHOPT(1:2),'A6').NE.0) THEN
         IF (IVSIZE.EQ.6) R=1.
         RETURN
      ENDIF
      IF (INDEX(CHOPT(1:1),'P').NE.0) THEN
         IF (IPGSIZ.EQ.1) R=1.
         RETURN
      ENDIF
      IF (INDEX(CHOPT(1:2),'*P').NE.0) THEN
         IF (IPGSIZ.EQ.1) R=1.
         RETURN
      ENDIF
      IF (INDEX(CHOPT(1:3),'**P').NE.0) THEN
         IF (IPGSIZ.EQ.2) R=1.
         RETURN
      ENDIF
      IF (INDEX(CHOPT(1:4),'***P').NE.0) THEN
         IF (IPGSIZ.EQ.3) R=1.
         RETURN
      ENDIF
      IF (INDEX(CHOPT(1:4),'NOPG').NE.0) THEN
         IF (IPGSIZ.EQ.0) R=1.
         RETURN
      ENDIF
*
      END
