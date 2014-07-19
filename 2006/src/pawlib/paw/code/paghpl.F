*
* $Id: paghpl.F,v 1.3 2001/12/07 15:47:54 couet Exp $
*
* $Log: paghpl.F,v $
* Revision 1.3  2001/12/07 15:47:54  couet
* - Option Z is now used to call the routines HPLAER and HPLSYM.
*
* Revision 1.2  1998/12/10 10:49:46  couet
* - New commands 3DNULL 3DPLINE and 3DPMARKER
*
* Revision 1.1.1.1  1996/03/01 11:38:39  mclareni
* Paw
*
*
#include "paw/pilot.h"
      SUBROUTINE PAGHPL
*
*            /GRAPHICS/HPLOT
*
#include "paw/quest.inc"
#include "hbook/hcbook.inc"
#include "paw/pcpatl.inc"
#include "paw/pcchar.inc"
      CHARACTER*8 CHOPT
*.______________________________________
*
      CALL KUPATL(CHPATL,NPAR)
*.______________________________________
*
*             SYMBOLS
*
      IF(CHPATL.EQ.'SYMBOLS')THEN
         CALL KUGETV(CHTITL,LX1,LX2)
         CALL KUGETV(CHTITL,LY1,LY2)
         IF(LX1.EQ.0)GO TO 999
         IF(LY1.EQ.0)GO TO 999
         NCX=LX2-LX1+1
         NCY=LY2-LY1+1
         IF(NCX.LE.0)GO TO 999
         IF(NCY.LE.0)GO TO 999
         CALL KUGETI(N)
         IF(N.GT.NCX)N=NCX
         IF(N.GT.NCY)N=NCY
         CALL KUGETI(ISYMB)
         CALL KUGETR(SSIZE)
         IQUEST(81) = LX1
         IQUEST(82) = LY1
         CALL HPLSYM(Q(LX1),Q(LY1),N,ISYMB,SSIZE,'Z')
         GO TO 999
      ENDIF
*.______________________________________
*
*             ERRORS
*
      IF(CHPATL.EQ.'ERRORS')THEN
         CALL KUGETV(CHTITL,LX1,LX2)
         CALL KUGETV(CHTITL,LY1,LY2)
         CALL KUGETV(CHTITL,LEX1,LEX2)
         CALL KUGETV(CHTITL,LEY1,LEY2)
         IF(LX1.EQ.0)GO TO 999
         IF(LY1.EQ.0)GO TO 999
         IF(LEX1.EQ.0)GO TO 999
         IF(LEY1.EQ.0)GO TO 999
         NCX=LX2-LX1+1
         NCY=LY2-LY1+1
         IF(NCX.LE.0)GO TO 999
         IF(NCY.LE.0)GO TO 999
         NECX=LEX2-LEX1+1
         NECY=LEY2-LEY1+1
         IF(NECX.LE.0)GO TO 999
         IF(NECY.LE.0)GO TO 999
         IF(NECX.LT.NCX)NCX=NECX
         IF(NECY.LT.NCY)NCY=NECY
         CALL KUGETI(N)
         IF(N.GT.NCX)N=NCX
         IF(N.GT.NCY)N=NCY
         CALL KUGETI(ISYMB)
         CALL KUGETR(SSIZE)
         CALL KUGETC(CHOPT,NCH)
         IQUEST(81) = LX1
         IQUEST(82) = LY1
         IQUEST(83) = LEX1
         IQUEST(84) = LEX1
         IQUEST(85) = LEY1
         IQUEST(86) = LEY1
         CHOPT(8:8) = 'Z'
         CALL HPLERR(Q(LX1),Q(LY1),Q(LEX1),Q(LEY1),N,CHOPT,ISYMB,SSIZE)
         GO TO 999
      ENDIF
*.______________________________________
*
*             AERRORS
*
      IF(CHPATL.EQ.'AERRORS')THEN
         CALL KUGETV(CHTITL,LX1,LX2)
         CALL KUGETV(CHTITL,LY1,LY2)
         CALL KUGETV(CHTITL,LEXL1,LEXL2)
         CALL KUGETV(CHTITL,LEXU1,LEXU2)
         CALL KUGETV(CHTITL,LEYL1,LEYL2)
         CALL KUGETV(CHTITL,LEYU1,LEYU2)
         IF(LX1.EQ.0)GO TO 999
         IF(LY1.EQ.0)GO TO 999
         IF(LEXL1.EQ.0)GO TO 999
         IF(LEXU1.EQ.0)GO TO 999
         IF(LEYL1.EQ.0)GO TO 999
         IF(LEYU1.EQ.0)GO TO 999
         NCX=LX2-LX1+1
         NCY=LY2-LY1+1
         IF(NCX.LE.0)GO TO 999
         IF(NCY.LE.0)GO TO 999
         NECXL=LEXL2-LEXL1+1
         NECXU=LEXU2-LEXU1+1
         NECYL=LEYL2-LEYL1+1
         NECYU=LEYU2-LEYU1+1
         IF(NECXL.LE.0)GO TO 999
         IF(NECXU.LE.0)GO TO 999
         IF(NECYL.LE.0)GO TO 999
         IF(NECYU.LE.0)GO TO 999
         IF(NECXL.LT.NCX)NCX=NECXL
         IF(NECXU.LT.NCX)NCX=NECXU
         IF(NECYL.LT.NCY)NCY=NECYL
         IF(NECYU.LT.NCY)NCY=NECYU
         CALL KUGETI(N)
         IF(N.GT.NCX)N=NCX
         IF(N.GT.NCY)N=NCY
         CALL KUGETI(ISYMB)
         CALL KUGETR(SSIZE)
         CALL KUGETC(CHOPT,NCH)
         IQUEST(81) = LX1
         IQUEST(82) = LY1
         IQUEST(83) = LEXL1
         IQUEST(84) = LEXU1
         IQUEST(85) = LEYL1
         IQUEST(86) = LEYU1
         CHOPT(8:8) = 'Z'
         CALL HPLAER(Q(LX1),Q(LY1),Q(LEXL1),Q(LEXU1),Q(LEYL1),Q(LEYU1)
     +,              N,CHOPT,ISYMB,SSIZE)
         GO TO 999
      ENDIF
*.______________________________________
*
*             KEY
*
      IF(CHPATL.EQ.'KEY')THEN
         CALL KUGETR(X)
         CALL KUGETR(Y)
         CALL KUGETI(ISYMB)
         CALL KUGETS(CHTITL,NCH)
         CALL KUGETR(DX)
         CALL KUGETC(CHOPT,NCH)
         CALL HPLTOC(X,Y,XCM,YCM,NT)
         CALL HPLSEL(1)
         CALL HPLNOT(XCM,YCM,DX,ISYMB,CHTITL,CHOPT)
         CALL HPLSEL(NT)
         GO TO 999
      ENDIF
*.______________________________________
*
*             TICKS
*
      IF(CHPATL.EQ.'TICKS')THEN
         CALL KUGETC(CHOPT,NCH)
         CALL KUGETR(XW)
         CALL KUGETR(YW)
         CALL HPLWIR(CHOPT,XW,YW,'TICK')
         GO TO 999
      ENDIF
*.______________________________________
*
*             ATITLE
*
      IF(CHPATL.EQ.'ATITLE')THEN
         CALL KUGETS(CHTITL,NCH)
         CALL KUGETS(CHTEMP,NCH)
         CALL KUGETS(CHFILE,NCH)
         CALL KUGETI(IALGN)
         CALL KUGETS(CHOPT,NCH)
         CALL HPLLGD(CHTITL,CHTEMP,CHFILE,IALGN,CHOPT)
         GO TO 999
      ENDIF
*.______________________________________
*
*             GRID
*
      IF(CHPATL.EQ.'GRID')THEN
         CALL PAGRID
         GO TO 999
      ENDIF
*.______________________________________
*
*             NULL
*
      IF(CHPATL.EQ.'NULL')THEN
         IF(NPAR.EQ.0)THEN
            CALL IZSAVA
            CALL HPLNUL
            CALL IZSETA
         ELSE
            CALL KUGETR(XMIN)
            CALL KUGETR(XMAX)
            CALL KUGETR(YMIN)
            CALL KUGETR(YMAX)
            CALL KUGETC(CHOPT,NCH)
            CALL HPLFRA(XMIN,XMAX,YMIN,YMAX,CHOPT)
         ENDIF
         CALL PAUTIT(' ')
      ENDIF
*.______________________________________
*
*             3DNULL
*
      IF(CHPATL.EQ.'3DNULL')THEN
         CALL KUGETR(XMIN)
         CALL KUGETR(XMAX)
         CALL KUGETR(YMIN)
         CALL KUGETR(YMAX)
         CALL KUGETR(ZMIN)
         CALL KUGETR(ZMAX)
         CALL KUGETR(THETA)
         CALL KUGETR(PHI)
         CALL KUGETC(CHOPT,NCH)
         CALL HPLFR3(XMIN,XMAX,
     +               YMIN,YMAX,
     +               ZMIN,ZMAX,
     +               THETA,PHI,
     +               CHOPT)
         CALL PAUTIT(' ')
      ENDIF
*
  999 END
