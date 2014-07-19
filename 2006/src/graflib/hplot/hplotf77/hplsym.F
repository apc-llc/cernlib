*
* $Id: hplsym.F,v 1.2 2001/12/07 15:19:56 couet Exp $
*
* $Log: hplsym.F,v $
* Revision 1.2  2001/12/07 15:19:56  couet
* - use higz include files from the higz include file directory.
*   option Z is now working in hplaer.F
*   option Z has been implemented in hplsym.F
*
* Revision 1.1.1.1  1996/01/19 10:50:11  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.20/00 19/04/95  10.52.25  by  O.Couet
*-- Author :
      SUBROUTINE HPLSYM(XU,YU,N,JISYM,USIZE,CHOPT)
*.==========>
*.
*.   Draws symbols (or a point) on the picture at the point defined by XU(N) 
*. and YU(N) according to CHOPT:
*.
*.   ' ' Transformation NTHIST is selected
*.   'C' Transformation 1 is selected
*.   'Z' The vectors XU,YU,DXU1,DXU2,DYU1 and DYU2 are taken into PAWC at the   
*.       adresses IQUEST(81) and IQUEST(82)
*.
*..=========>
#include "hplot/hpl1.inc"
#include "hplot/hpl2.inc"
#include "hplot/hpl4.inc"
#include "hplot/hpl6.inc"
#include "hbook/hcbook.inc"
#include "hplot/quest.inc"
#include "higz/hilink.inc"
      EQUIVALENCE (LXU,LSTACK(1)),(LYU,LSTACK(2))
      DIMENSION XU(N),YU(N)
      DIMENSION KSYM(12),KOLD(12)
      CHARACTER*(*) CHOPT
      DATA KOLD/840,841,842,843,850,851,852,853,2261,2262,2263,2264/
      DATA KSYM/ 20, 21, 22, 23, 24, 25, 26, 27,  28,  29,  30,  31/
*._____________________________
*
      LOCM = .FALSE.
      IF (USIZE.GT.0.0) THEN
         SYMSIZ = USIZE
      ELSE
         SYMSIZ = TVSIZ(1)
      ENDIF
      KISYM = JISYM
      DO 10 I=1,12
         IF (KISYM.EQ.KOLD(I)) KISYM = KSYM(I)
   10 CONTINUE
      IF (KISYM.LE.0) KISYM = 1
*----
*      If necessary the adresses of the vectors are saved in
*      a link area.
*----
      IF(INDEX(CHOPT,'Z').NE.0)THEN
         LXU   = IQUEST(81)
         LYU   = IQUEST(82)
         IOPTZ = 1
      ELSE
         IOPTZ = 0
      ENDIF
*----
*     Sets markers attributes
*----
      CALL IGQ('MSCF',SMSF)
      CALL IGQ('MTYP',RMTYP)
      CALL ISMKSC((SYMSIZ*XYTWN)/0.009)
      CALL ISMK(KISYM)
*----
*     Selects cm transformation if necessary
*----
      IF (INDEX(CHOPT,'C').NE.0) LOCM = .TRUE.
      IF (LOCM) THEN
         IF(NTWIN.NE.1)CALL ISELNT(1)
         NTWIN = 1
      ELSE
         IF(NTWIN.NE.NTHIST)CALL ISELNT(NTHIST)
         NTWIN = NTHIST
      ENDIF
*----
*     loop round coordinates
*----
      DO 20 K=1,N
         IF (IOPTZ.NE.0) THEN
            XP = Q(LXU+K-1)
            YP = Q(LYU+K-1)
         ELSE
            XP = XU(K)
            YP = YU(K)
         ENDIF
         IF (.NOT.LOCM) THEN
            IF (LOGXFL) THEN
               IF(XP.LE.0.)GO TO 20
               XP = LOG10(XP)
            ENDIF
            IF (LOGYFL) THEN
               IF(YP.LE.0)GO TO 20
               YP = LOG10(YP)
            ENDIF
         ENDIF
         CALL IPM(1,XP,YP)
   20 CONTINUE
*
      CALL IGSET('MSCF',SMSF)
      CALL IGSET('MTYP',RMTYP)
*
      END
