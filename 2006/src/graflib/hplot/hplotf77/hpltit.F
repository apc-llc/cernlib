*
* $Id: hpltit.F,v 1.1.1.1 1996/01/19 10:50:11 mclareni Exp $
*
* $Log: hpltit.F,v $
* Revision 1.1.1.1  1996/01/19 10:50:11  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.20/00 19/04/95  10.26.37  by  O.Couet
*-- Author :
      SUBROUTINE HPLTIT(CHHTIT)
*.==========>
*. Draws histogram title or user supplied title
*. (Cm are used for call to HPLTXT)
*..=========>
#include "hbook/hcbook.inc"
#include "hbook/hcbits.inc"
#include "hplot/hpl2.inc"
#include "hplot/hpl4.inc"
#include "hplot/hpl6.inc"
#include "hplot/hplstr.inc"
      CHARACTER*(*) CHHTIT
      DIMENSION ITIT(2)
      DIMENSION RTXFP(2)
*._____________________________
*
      TVTIT = TVSIZ(3)
      CALL IGQ('TXFP',RTXFP)
*----
*       Set histogram Title font and precision
*----
      NTWOLD = NTWIN
      KFONT  = IABS(IHFONT(1))
      ITXFON = KFONT/10
      ITXPRE = MOD(KFONT,10)
      IF(IHFONT(1).LT.0)ITXFON = -ITXFON
      CALL ISTXFP(ITXFON,ITXPRE)
*
      IF(CHHTIT.NE.' ')THEN
         CHTEMP = CHHTIT
         GOTO 10
      ENDIF
*-----
*       If CHHTIT = ' ', the histogram title is taken from the
*       histogram data structure. The maximum length of the title
*       is ISTRSZ characters (HPLOT limitation)
*-----
      J1              = KTIT1
      IF(I230.NE.0)J1 = KTIT2
      NCHR            = (IQ(LCID-1)-J1+1)*4
      IF(NCHR.EQ.0)GOTO 999
      NCHR            = MIN(NCHR,ISTRSZ)
      CHTEMP          = ' '
      CALL UHTOC(IQ(LCID+J1),4,CHTEMP,NCHR)
*----
*       Automatic calculation of character size if
*       TVSIZ(3) (TSIZ) equal to 0.
*       (0.28 is a maximum value, unless the user sets
*       it with SET TSIZ).
*----
   10 NBCHAR = LENOCC(CHTEMP)
      IF(NBCHAR.EQ.0)GOTO 999
      IF(TVSIZ(3).EQ.0.)THEN
         TVSIZ(3) = (XHIGH-XLOW)/NBCHAR
         IF(TVSIZ(3).GT.0.28)TVSIZ(3) = 0.28
      ENDIF
*----
*       Decode the title to find if some axis title are required.
*       The histogram title can have the format:
*       'histo_title ; X_axis_title ; Y_axis_title ; Z_axis_title'
*       The position of each ';' is returned in ITIT.
*----
      J       = 0
      ITIT(1) = 0
      ITIT(2) = 0
      DO 20 I=1,NBCHAR
         IF(CHTEMP(I:I).EQ.';')THEN
            J       = J+1
            IF(J.EQ.3)GOTO 30
            ITIT(J) = MAX(I-1,0)
         ENDIF
   20 CONTINUE
   30 IF (J.EQ.0) THEN
         ITIT(1) = NBCHAR
         ITIT(2) = NBCHAR
      ELSEIF (J.LT.2) THEN
         IF (NBCHAR.GT.ITIT(J)+1) THEN
            ITIT(J+1) = NBCHAR
         ENDIF
      ENDIF
*
      IF (ITIT(1).GE.1) THEN
         XK = 0.5*(XHIGH+XLOW)
         YK = YLOW-YHTIT-TVSIZ(3)
         CALL HPLTXT(XK,YK,CHTEMP(1:ITIT(1)),3,1,2)
      ENDIF
      IF (ITIT(2).GE.ITIT(1)+2) THEN
         CALL HPLAX(CHTEMP(ITIT(1)+2:ITIT(2)),' ')
      ENDIF
      IF (NBCHAR .GE.ITIT(2)+2) THEN
         CALL HPLAX(' ',CHTEMP(ITIT(2)+2:NBCHAR))
      ENDIF
*
      TVSIZ(3) = TVTIT
*----
*       Reset NT
*----
      IF(NTWIN.NE.NTWOLD)THEN
         NTWIN = NTWOLD
         CALL ISELNT(NTWOLD)
      ENDIF
*
  999 CALL ISTXFP(INT(RTXFP(1)),INT(RTXFP(2)))
      END
