*
* $Id: pavect.F,v 1.3 2002/01/15 16:08:04 couet Exp $
*
* $Log: pavect.F,v $
* Revision 1.3  2002/01/15 16:08:04  couet
* - When a vector of length 1 was created, for example as the result of
*   a vec/read command, the plot command didn't recognize it as a vector.
*   Commands such as vec/hfill or vec/plot returned an "unknown vector"
*   error.
*
* Revision 1.2  1997/03/17 10:00:13  couet
* - in v/plot x when the option S is used, the limit of the histogram are taken
* from the current plot.
*
* Revision 1.1.1.1  1996/03/01 11:38:42  mclareni
* Paw
*
*
#include "paw/pilot.h"
      SUBROUTINE PAVECT
*
#include "hbook/hcbook.inc"
#include "hbook/hcmail.inc"
#include "paw/pawcom.inc"
#include "paw/pcpatl.inc"
#include "paw/pcchar.inc"
#include "paw/quest.inc"
#include "paw/pcwk.inc"
#include "paw/pcbuff.inc"
*
*           /VECTOR
*           Operations with vectors
*
      REAL RVAL(4)
      EQUIVALENCE (RVAL(1),PAWBUF(1))
*
      CHARACTER*32 VNAME
      LOGICAL HEXIST
*
      CALL KUPATL(CHPATL,NPAR)
*.______________________________________
*
*              VECTOR/PLOT
*
      IF(CHPATL.EQ.'PLOT')THEN
         CALL KUGETC(CHTITL,NCH)
         IF(NCH.EQ.0)GO TO 999
         CALL PAGETI(ID)
         IF(ID.EQ.12345)THEN
            IF(HEXIST(ID))CALL HDELET(ID)
         ENDIF
         CALL KUGETC(CHTEMP,NCH2)
         IPC=0
         DO 10 I=2,NCH-1
            IF(CHTITL(I:I).EQ.'%')THEN
               IPC=I
               GO TO 20
            ENDIF
   10    CONTINUE
*
*              Case: V/PLOT X
*
   20    IF(IPC.EQ.0)THEN

            CALL KUVECT(CHTITL,LX1,LX2)
            NCX = LX2-LX1+1
            IF (NCX.LE.0) GOTO 90
            LFIX = 0
            CALL HPLSET('?TFON',CFON)
            ICFON = INT(CFON/10.)
            IF (CFON.EQ.2..OR.ICFON.EQ.1) CALL IGTCCH(CHTITL)
            IF(INDEX(CHTEMP,'S').NE.0)THEN
               CALL IGQWK(1,'NTWN',RVAL)
               XMIN = RVAL(1)
               XMAX = RVAL(2)
            ELSE
               XMIN = VMIN(Q(LX1),NCX)
               XMAX = VMAX(Q(LX1),NCX)
               DX   = (XMAX-XMIN)/40.
               IF (DX.EQ.0.) DX = 1.
               XMIN = XMIN-DX
               XMAX = XMAX+DX
            ENDIF
*
* Create and fill a 1D histo
*
            CALL HBOOK1(ID,CHTITL,100,XMIN,XMAX,0.)
            DO 30 I=LX1,LX2
               CALL HF1(ID,Q(I),1.)
   30       CONTINUE
*
* plot or print the 1D histo
*
            IF (IWK.NE.0) CALL HPLOT(ID,CHTEMP,' ',0)
            IF (IWK.EQ.0.OR.IWK.EQ.-2) CALL HPRINT(ID)
         ELSE
*
*              Case: V/PLOT X%Y
*
            VNAME = CHTITL(1:IPC-1)
            CALL KUVECT(VNAME,LY1,LY2)
            IF (LY1.LE.0) GO TO 90
            VNAME = CHTITL(IPC+1:NCH)
            CALL KUVECT(VNAME,LX1,LX2)
            IF (LX1.LE.0) GO TO 90
            LQ(LCDIR-40) = LX1
            LQ(LCDIR-41) = LY1
            NCX          = LX2-LX1+1
            NCY          = LY2-LY1+1
            NCHA         = MIN(NCX,NCY)
            XMIN         = VMIN(Q(LX1),NCHA)
            XMAX         = VMAX(Q(LX1),NCHA)
            DX           = (XMAX-XMIN)/40.
            IF (DX.EQ.0.) DX = 1.
            YMIN         = VMIN(Q(LY1),NCHA)
            YMAX         = VMAX(Q(LY1),NCHA)
            DY           = (YMAX-YMIN)/40.
            IF (DY.EQ.0.) DY=1.
            CALL HPLSET('?TFON',CFON)
            ICFON = INT(CFON/10.)
            IF (CFON.EQ.2..OR.ICFON.EQ.1) CALL IGTCCH(CHTITL)
*
* Create a 2D histo
*
            CALL HBOOK2(ID,CHTITL,40,XMIN-DX,XMAX+DX,
     +                            40,YMIN-DY,YMAX+DY,0.)
*
* Draw the vectors X and Y
*
            IF(IWK.NE.0)THEN
               CALL HPLOT(ID,CHTEMP,' ',0)
               CALL PAHLOG(LOGX,LOGY,LOGZ)
               IF(LOGX.NE.0.OR.LOGY.NE.0)CHTEMP(32:32) = 'G'
               IF(LOGX.NE.0)CHTEMP(31:31)              = 'X'
               IF(LOGY.NE.0)CHTEMP(30:30)              = 'Y'
               CHTEMP(1:1)   = 'P'
               IQUEST(81)    = LQ(LCDIR-40)
               IQUEST(82)    = LQ(LCDIR-41)
               CHTEMP(29:29) = 'Z'
               CALL IGRAPH(NCHA,Q(LX1),Q(LY1),CHTEMP)
            ENDIF
*
* Fill the 2D histo
*
            IF (ID.NE.12345.OR.IWK.EQ.0) THEN
               DO 40 I=1,NCHA
                  CALL HF2(ID,Q(LX1+I-1),Q(LY1+I-1),1.)
   40          CONTINUE
            ENDIF
            IF (IWK.EQ.0) CALL HPRINT(ID)
         ENDIF
*
         CALL PAUTIT(' ')
         GO TO 999
      ENDIF
*.______________________________________
*
*              VECTOR/HFILL
*
      IF(CHPATL.EQ.'HFILL')THEN
         CALL KUGETC(CHTITL,NCH)
         CALL PAGETI(ID)
         IPC=0
         DO 50 I=2,NCH-1
            IF(CHTITL(I:I).EQ.'%')THEN
               IPC=I
               GO TO 60
            ENDIF
   50    CONTINUE
   60    IF(IPC.EQ.0)THEN
            CALL KUVECT(CHTITL,LX1,LX2)
            NP    = LX2 - LX1 + 1
            IF (NP.LE.0)GO TO 90
            LFIX=0
            DO 70 I=LX1,LX2
               CALL HF1(ID,Q(I),1.)
   70       CONTINUE
         ELSE
            VNAME=CHTITL(1:IPC-1)
            CALL KUVECT(VNAME,LY1,LY2)
            IF(LY1.LE.0)GO TO 90
            VNAME=CHTITL(IPC+1:NCH)
            CALL KUVECT(VNAME,LX1,LX2)
            IF(LX1.LE.0)GO TO 90
            NCX=LX2-LX1+1
            NCY=LY2-LY1+1
            NCH=MIN(NCX,NCY)
            DO 80 I=1,NCH
               CALL HFILL(ID,Q(LX1+I-1),Q(LY1+I-1),1.)
   80       CONTINUE
         ENDIF
         GO TO 999
      ENDIF
*.______________________________________
*
*              VECTOR/DRAW
*
      IF(CHPATL.EQ.'DRAW')THEN
         CALL KUGETV(CHTITL,LX1,LX2)
         CALL PAGETI(ID)
         CALL KUGETC(CHTEMP,NCH)
         NCX    = LX2 - LX1 + 1
         IF (NCX.LE.1)GO TO 999
         XMIN=VMIN(Q(LX1),NCX)
         XMAX=VMAX(Q(LX1),NCX)
         IF(ID.EQ.12345)THEN
            IF(HEXIST(ID))CALL HDELET(ID)
         ENDIF
         LFIX=0
         NCH=LENOCC(CHTITL)
         CALL HPLSET('?TFON',CFON)
         ICFON = INT(CFON/10.)
         IF(CFON.EQ.2..OR.ICFON.EQ.1)CALL IGTCCH(CHTITL)
         CALL HBOOK1(ID,CHTITL,NCX,1.,FLOAT(NCX+1),0.)
         CALL HPAK(ID,Q(LX1))
         IF(XMIN.EQ.XMAX)THEN
            IF(XMIN.LT.0.)THEN
               CALL HMAXIM(ID,0.)
               CALL HMINIM(ID,1.1*XMIN)
            ELSE
               CALL HMINIM(ID,0.)
               CALL HMAXIM(ID,1.1*XMAX)
            ENDIF
         ENDIF
         IF(IWK.NE.0)CALL HPLOT(ID,CHTEMP,' ',0)
         IF(IWK.EQ.0.OR.IWK.EQ.-2)CALL HPRINT(ID)
         IF(ID.EQ.12345)CALL HDELET(ID)
         CALL PAUTIT(' ')
         GO TO 999
      ENDIF
*
*             Error
*
   90 CHMAIL='Unknown vector ==> '//CHTITL
      CHTITL=CHMAIL
      CALL HBUG(CHTITL,'PAVECT',0)
*
  999 END
