*
* $Id: hplone.F,v 1.2 1998/06/02 09:14:23 couet Exp $
*
* $Log: hplone.F,v $
* Revision 1.2  1998/06/02 09:14:23  couet
* - if FCOL was > 1000 the axes were redrawn even if option A was used.
*
* Revision 1.1.1.1  1996/01/19 10:50:10  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*-- Author : O.Couet
      SUBROUTINE HPLONE(CHOPT,IERROR)
*.==========>
*. HPLOT basic routine to draw 1-dim projection.
*. Transformation NTHIST is selected when calling HPLSWN.
*..=========>
#include "hbook/hcbits.inc"
#include "hbook/hcbook.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcprin.inc"
#include "hplot/hpl1.inc"
#include "hplot/hpl2.inc"
#include "hplot/hpl3.inc"
#include "hplot/hpl9.inc"
#include "hplot/hpl13.inc"
#include "hplot/hplstr.inc"
      CHARACTER*(*) CHOPT
      INTEGER HPLHIP
      DIMENSION XAX(3),YAX(3)
*._____________________________
*
*----
*     If error bars required or
*     if HSTAR has been called (function)
*     then option "K", "Update" and "+" are not available.
*-----
      IF((I11.NE.0).OR.(I12.NE.0).OR.(I28.NE.0))THEN
         IF(IOPTK.NE.0)THEN
            CALL HBUG('Option "K" not available ','HPLOT ',IDZK)
            RETURN
         ENDIF
         IF(IOPTU.NE.0)THEN
            CALL HBUG('Option "Update" not available ','HPLOT ',IDZB)
            RETURN
         ENDIF
         IF(IOPTP.NE.0)THEN
            CALL HBUG('Option "+" not available ','HPLOT ',IDZK)
            RETURN
         ENDIF
      ENDIF
*----
*     If option "Update" or "+" is selected we set LOGXFL and LOGYFL
*     according to ZEBRA structure otherwise we determine them.
*     If LHPLIP=0 it means that we cannot find histogram IDZB (option "U")
*     or IDZK (option "+") in the structure (not created with option "K"
*     if windowing, or scratched by another histogram if not windowing).
*----
      IF((IOPTU.NE.0).OR.(IOPTP.NE.0))THEN
         IF(IOPTU.NE.0)THEN
            LHPLIP=HPLHIP(IDZB,'I')
            IF(LHPLIP.EQ.0)THEN
               CALL HBUG('Histogram is not in memory','HPLOT',IDZB)
               RETURN
            ENDIF
         ELSE IF(IOPTP.NE.0)THEN
            LHPLIP=HPLHIP(IDZK,'I')
            IF(LHPLIP.EQ.0)THEN
               CALL HBUG('No histogram in memory','HPLOT',IDZK)
               RETURN
            ENDIF
         ENDIF
         IF(IQ(LHPLIP+3).NE.0)THEN
            LOGYFL=.TRUE.
         ELSE
            LOGYFL=.FALSE.
         ENDIF
         IF(IQ(LHPLIP+4).NE.0)THEN
            LOGXFL=.TRUE.
         ELSE
            LOGXFL=.FALSE.
         ENDIF
      ELSE
*----
*      The defaults is LINX and LINY. If IDOPT LOGY has been applied
*      on a histogram (I26.NE.0) the option LOGY is forced.
*----
         LOGXFL=.FALSE.
         IF(LOGX.NE.0)LOGXFL=.TRUE.
         LOGYFL=.FALSE.
         IF(LOGY.NE.0)LOGYFL=.TRUE.
         IF(I26.NE.0)LOGYFL=.TRUE.
      ENDIF
*----
*       Histogram coordinates
*       abcsissa (XMINI,XMAXI)
*       ordonnee (YMINI,YMAXI)
*----
      NCX=IQ(LPRX)
      NB=IQ(LCONT+KNBIT)
      IF(I6.EQ.0)THEN
         XXSIZE=(Q(LPRX+2)-Q(LPRX+1))/FLOAT(NCX)
         XX0=Q(LPRX+1)
         IF(LOGXFL.AND.XX0.LE.0.)THEN
            XX0=0.1*XXSIZE
         ENDIF
      ENDIF
*
      CALL HPLC(IERROR)
*----
*      Case "LOGYFL and YMINI=YMAXI" or "YMAXI<0"
*----
      IF(IERROR.NE.0)RETURN
*----
*      Picture surround (if new page) and page number (if requested).
*      Histogram surround (if not option "Same" nor "Update").
*      If scale has changed (option "Update") all histogram "Live" are
*      completly redraw with axis.
*----
      LO3DPL = .FALSE.
      CALL HPLSWN
      IF(I1.NE.0)THEN
         CALL IGPID(1,'1d',ID,' ')
      ELSE
         CALL IGPID(1,'2d',ID,' ')
      ENDIF
      IF(LOSCAL)RETURN
*
      CALL HPLDES(ID)
*----
*     do not draw histogram if error bars required
*     unless 'eah' option has been selected
*-----
      IF((.NOT.LOEAH.AND.I34.EQ.1).OR.IOPTE.NE.0) GOTO 10
*-----
*     if HSTAR has been called (I28.EQ.1) draw function
*-----
      IF(I28.NE.0)THEN
         CALL HPLF(1)
      ELSE
         IF(IOPTH.NE.0)CALL HPLH
      ENDIF
*----
*     test for error bars or option E
*----
   10 CONTINUE
      CHTEMP = ' '
      IF(IOPTE.NE.0)THEN
         CHTEMP        = CHOPT
         CHTEMP(16:16) = 'H'
         CALL HPLAER(0.,0.,0.,0.,0.,0.,0,CHTEMP,0,0)
      ENDIF
      IF(I34.NE.0.AND.IOPTE.EQ.0)THEN
         IF(IOPTH.EQ.2)THEN
            CALL HPLH
         ELSE
            IF(IOPTF.LT.2)THEN
               CHTEMP        = CHOPT
               CHTEMP(16:16) = 'H'
               CALL HPLAER(0.,0.,0.,0.,0.,0.,0,CHTEMP,0,0)
            ENDIF
         ENDIF
      ENDIF
*----
*     test for function
*---
      IF(I12.NE.0)THEN
         IF(IOPTF.NE.0)CALL HPLF(3)
      ENDIF
*----
*     draw axes if required.
*----
      IF (IOPTA.EQ.0) THEN
         IF ((.NOT.LOSAME).AND.(IOPTU.EQ.0).AND.(IOPTP.EQ.0)) THEN
            XAX(1)=XMINI
            XAX(2)=XMINI
            XAX(3)=XMAXI
            YAX(1)=YMINI
            YAX(2)=YMAXI
            YAX(3)=YMINI
            CALL HPLAXI(XAX,YAX,1)
         ENDIF
         IF (LOSAME.AND.RHCOL.GT.1000..OR.IFTYP.GT.1000) THEN
            XAX(1)=XWMINI
            XAX(2)=XWMINI
            XAX(3)=XWMAXI
            YAX(1)=YWMINI
            YAX(2)=YWMAXI
            YAX(3)=YWMINI
            CALL HPLAXI(XAX,YAX,2)
         ENDIF
      ENDIF
*
      CALL HPLU(ID,1)
*
      END
