*
* $Id: pafunc.F,v 1.10 2000/01/18 09:59:58 couet Exp $
*
* $Log: pafunc.F,v $
* Revision 1.10  2000/01/18 09:59:58  couet
* - bug fixed in fun/plot:
*
*   The parameters used to describe color and filling of the f/plot
*   apparance change if it is a first plot or a superposition
*   To get the proper results one need to type:
*
*      set HCOL 1002
*      f/plot f1(x) 0 100
*      set FCOL 1005
*      f/plot f2(x) 0 100 S
*
* Revision 1.9  1997/04/24 07:06:25  couet
* - remove the CALL CLTOU before CSEXPR. This call is now done in CSEXPR itself
*
* Revision 1.8  1997/04/23 15:01:08  couet
* - a # was added at the end of the string given as parameter to csexpr. This
* was useless because csexpr add the # itself. These caused errors like:
*
*  CS-TR-ERR: routine _001, line    0
*  FUNCTION _001(X,Y,Z) _001=X**2# ^ #
*   syntax error
*
* Revision 1.7  1997/03/14 11:14:09  couet
* - CSDPRO was called too early in FUN/PLOT. This produced a bug in the function
* plotting with LOGX.
*
* Revision 1.6  1997/03/11 16:17:41  couet
* - Version 2.09/02, CLTOU was missing in FUN/DRAW
*
* Revision 1.5  1996/10/23 14:19:03  couet
* - paexpr putted back in fun/dr
*
* Revision 1.4  1996/10/23 14:15:52  couet
* -  PAEXPR removed from FUN/DR
*
* Revision 1.3  1996/10/21 16:31:59  couet
* - New functions to compile comis program before calling csexpr
*
* Revision 1.2  1996/05/28 12:12:49  couet
* - Free the space used in the COMIS memory after a FUNC/PLOT FUN1 etc ...
*
* Revision 1.1.1.1  1996/03/01 11:38:39  mclareni
* Paw
*
*
#include "paw/pilot.h"
      SUBROUTINE PAFUNC
*
*            /FUNCTION
*
#include "hbook/hcbook.inc"
#include "paw/pawcom.inc"
#include "paw/pawnpu.inc"
#include "paw/pcpatl.inc"
#include "paw/pcchar.inc"
#include "paw/pawcfu.inc"
#include "paw/pcbuff.inc"
*
      REAL XP(20),YP(20)
      EQUIVALENCE (XP(1),PAWBUF(1)),(YP(1),PAWBUF(1003))
      CHARACTER*8 CHOPT
      EXTERNAL PAWFU1,PAWFU2,PAWFU3,IHDF01
      LOGICAL HEXIST
      DIMENSION IOPT(6)
      EQUIVALENCE (IOPT(1),IOPTC),(IOPT(2),IOPTS),(IOPT(3),IOPTPL)
      EQUIVALENCE (IOPT(4),IOPTL),(IOPT(5),IOPTP),(IOPT(6),IOPTM)
*
      CALL KUPATL(CHPATL,NPAR)
*
      IF(CHPATL.EQ.'FUN1')THEN
         CALL PAGETI (ID)
         CALL KUGETF (CHTITL,NCH)
         CALL KUGETI (NCX)
         CALL KUGETR (XLOW)
         CALL KUGETR (XUP)
         CALL KUGETC (CHOPT,NCH2)
         CHTEMP = CHTITL
         CALL PAWCS
         CALL PAWFCA (CHTEMP,NCH,JADF,0)
         JADFF=JADF
         IF (JADF.LE.0) THEN
            CALL PAEXPR(IERR)
            IF (IERR.NE.0) GOTO 999
            IF (NCH.GT.NCHAR-1) THEN
               PRINT*, '*** String is too long'
               GOTO 999
            ENDIF
            CALL CSEXPR (CHTEMP,JADF)
            IF (JADF.LE.0) GOTO 999
         ENDIF
         CALL HBFUN1 (ID,CHTITL,NCX,XLOW,XUP,PAWFU1)
         IF(JADFF.EQ.0.AND.JADF.NE.0)CALL CSDPRO(JADF)
         IF (CHOPT(1:1).NE.' ') THEN
            CALL HDCOFL
            CALL PAPLOT(ID,CHOPT,'HIST',0,0,0,0,0,0)
         ENDIF
         CALL PAUTIT(' ')
         GO TO 999
      ENDIF
*
      IF(CHPATL.EQ.'FUN2')THEN
         CALL PAGETI (ID)
         CALL KUGETF (CHTITL,NCH)
         CALL KUGETI (NCX)
         CALL KUGETR (XLOW)
         CALL KUGETR (XUP)
         CALL KUGETI (NCY)
         CALL KUGETR (YLOW)
         CALL KUGETR (YUP)
         CALL KUGETC (CHOPT,NCH2)
         CHTEMP = CHTITL
         CALL PAWCS
         CALL PAWFCA (CHTEMP,NCH,JADF,0)
         JADFF=JADF
         IF (JADF.LE.0) THEN
            CALL PAEXPR(IERR)
            IF (IERR.NE.0) GOTO 999
            IF (NCH.GT.NCHAR-1) THEN
               PRINT*, '*** String is too long'
               GOTO 999
            ENDIF
            CALL CSEXPR (CHTEMP,JADF)
            IF (JADF.LE.0) GOTO 999
         ENDIF
         CALL HBFUN2(ID,CHTITL,NCX,XLOW,XUP,NCY,YLOW,YUP,PAWFU2)
         IF(JADFF.EQ.0.AND.JADF.NE.0)CALL CSDPRO(JADF)
         IF (CHOPT.NE.' ') THEN
            CALL HDCOFL
            CALL PAPLOT(ID,CHOPT,'HIST',0,0,0,0,0,0)
         ENDIF
         CALL PAUTIT(' ')
         GO TO 999
      ENDIF
*
      IF(CHPATL.EQ.'POINTS')THEN
         CALL KUGETI(NPFUNX)
         CALL KUGETI(NPFUNY)
         CALL KUGETI(NPFUNZ)
         IF(NPAR.NE.0)THEN
            NPFUNC=NPFUNX
         ELSE
            NPFUNC=100
         ENDIF
         GO TO 999
      ENDIF
*
      IF(CHPATL.EQ.'RANGE')THEN
         CALL KUGETR(RANGX1)
         CALL KUGETR(RANGX2)
         IF(RANGX2.LT.RANGX1)THEN
            TEMP=RANGX2
            RANGX2=RANGX1
            RANGX1=TEMP
         ENDIF
         CALL KUGETR(RANGY1)
         CALL KUGETR(RANGY2)
         IF(RANGY2.LT.RANGY1)THEN
            TEMP=RANGY2
            RANGY2=RANGY1
            RANGY1=TEMP
         ENDIF
         CALL KUGETR(RANGZ1)
         CALL KUGETR(RANGZ2)
         IF(RANGZ2.LT.RANGZ1)THEN
            TEMP=RANGZ2
            RANGZ2=RANGZ1
            RANGZ1=TEMP
         ENDIF
         GO TO 999
      ENDIF
*
      IF(CHPATL.EQ.'ANGLE')THEN
         CALL KUGETR(ANGLE1)
         CALL KUGETR(ANGLE2)
         GO TO 999
      ENDIF
*
      IF(CHPATL.EQ.'DRAW')THEN
         CALL KUGETF(CHTITL,NCH)
         CALL KUGETC(CHOPT,NCH2)
         CALL PAWCS
         CHTEMP=CHTITL
         CALL PAWFCA(CHTEMP,NCH,JADF,0)
         JADFF=JADF
         IF(JADF.LE.0)THEN
            CALL PAEXPR(IERR)
            IF (IERR.NE.0) GOTO 999
            IF(NCH.GT.NCHAR-3)THEN
               PRINT*, '*** String is too long'
               GO TO 999
            ELSE
               IEQ=INDEX(CHTITL,'=')
               IF(IEQ.NE.0)THEN
                  DO 10 I=NCH,IEQ+1,-1
                     CHOPT(1:1)=CHTEMP(I:I)
                     CHTEMP(I+1:I+1)=CHOPT(1:1)
   10             CONTINUE
                  CHTEMP(IEQ:IEQ+1)='-('
                  CHTEMP(NCH+2:NCH+2)=')'
               ENDIF
            ENDIF
            CALL CSEXPR(CHTEMP,JADF)
            IF(JADF.LE.0)GO TO 999
         ENDIF
         XP(1)=RANGX1
         XP(2)=RANGY1
         XP(3)=RANGZ1
         YP(1)=RANGX2
         YP(2)=RANGY2
         YP(3)=RANGZ2
         CALL HPLFR3(RANGX1,RANGX2,RANGY1,RANGY2,RANGZ1,RANGZ2
     +,              ANGLE1,ANGLE2,'BWG')
         CALL IHIMPF(PAWFU3,XP,YP,NPFUNX,NPFUNY,NPFUNZ,IHDF01,'BF')
         IF(JADFF.EQ.0.AND.JADF.NE.0)CALL CSDPRO(JADF)
         CALL HPLFR3(RANGX1,RANGX2,RANGY1,RANGY2,RANGZ1,RANGZ2
     +,              ANGLE1,ANGLE2,'FG')
         CALL PAUTIT(CHTITL)
         GO TO 999
      ENDIF
*
      IF(CHPATL.EQ.'PLOT')THEN
         CALL KUGETF(CHTITL,NCH)
         CALL KUGETR(XLOW)
         CALL KUGETR(XUP)
         IF(XUP.LT.XLOW)THEN
            TEMP = XUP
            XUP  = XLOW
            XLOW = TEMP
         ENDIF
         IF (XLOW.EQ.XUP) XUP = XLOW+1.
         CALL KUGETC(CHOPT,NCH2)
         CALL UOPTC(CHOPT,'CS+LP*',IOPT)
         CHTEMP = CHTITL
         CALL PAWCS
         CALL PAWFCA (CHTEMP,NCH,JADF,0)
         JADFF=JADF
         IF (JADF.LE.0) THEN
            CALL PAEXPR(IERR)
            IF (IERR.NE.0) GOTO 999
            IF (NCH.GT.NCHAR-1) THEN
               PRINT*, '*** String is too long'
               GOTO 999
            ENDIF
            CALL CSEXPR (CHTEMP,JADF)
            IF (JADF.LE.0) GOTO 999
         ENDIF
         CALL PAHLOG(LOGX,LOGY,LOGZ)
         IDF=12345
         IF(HEXIST(IDF))CALL HDELET(IDF)
         CALL HBFUN1(IDF,CHTITL,NPFUNC,XLOW,XUP,PAWFU1)
         IF(INDEX(CHOPT,'HIST').EQ.0)THEN
            IOPALL = IOPTC+IOPTPL+IOPTL+IOPTP+IOPTM
            IF (IOPALL.EQ.0..AND.IOPTS.NE.0) THEN
               LCHOPT = LENOCC(CHOPT)+1
               CHOPT(LCHOPT:LCHOPT) = 'C'
            ENDIF
         ENDIF
         CALL HIDOPT(IDF,'CONT')
         IF(LOGX.EQ.0)THEN
            CALL HPLOT(IDF,CHOPT,' ',0)
         ELSE
            IF(NCH2.EQ.0)CHOPT='C'
            IF(XUP.LE.0.)GO TO 999
            IF(XLOW.LE.0.)THEN
               IF(XUP.GE.100.)THEN
                  XLOW=1.
               ELSE
                  XLOW=XUP/1000.
               ENDIF
            ENDIF
            XL = LOG10(XLOW)
            DX = (LOG10(XUP)-XL)/NPFUNC
            DO 20 I=1,NPFUNC+1
               XP(I) = XL+(I-1)*DX
               XTRUE = 10.**XP(I)
               YTRUE = PAWFU1(XTRUE)
               YP(I) = 0.
               IF (LOGY.NE.0) THEN
                  IF (YTRUE.GT.0.) YP(I) = LOG10(YTRUE)
               ELSE
                  YP(I) = YTRUE
               ENDIF
   20       CONTINUE
            YYMIN = VMIN(YP,NPFUNC+1)
            YYMAX = VMAX(YP,NPFUNC+1)
            DYY = 0.05*(YYMAX-YYMIN)
            IF (LOGY.NE.0) THEN
               YMIN = 10.**(YYMIN-DYY)
               YMAX = 10.**(YYMAX+DYY)
            ELSE
               YMIN = YYMIN-DYY
               YMAX = YYMAX+DYY
               IF (YMIN.LT.0..AND.YYMIN.GE.0.) YMIN = 0.
               IF (YMAX.GT.0..AND.YYMAX.LE.0.) YMAX = 0.
            ENDIF
            CALL HRESET(IDF,' ')
            CALL HMAXIM(IDF,YMAX)
            CALL HMINIM(IDF,YMIN)
            IF(IOPTS.EQ.0)THEN
               CALL HPLOT(IDF,' ',' ',0)
            ELSE
               I=INDEX(CHOPT,'S')
               CHOPT(I:I)=' '
            ENDIF
            CALL IGRAPH(NPFUNC+1,XP,YP,CHOPT)
         ENDIF
         IF(JADFF.EQ.0.AND.JADF.NE.0)CALL CSDPRO(JADF)
         CALL PAUTIT(' ')
      ENDIF
*
  999 END
