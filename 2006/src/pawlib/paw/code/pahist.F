*
* $Id: pahist.F,v 1.1.1.1 1996/03/01 11:38:40 mclareni Exp $
*
* $Log: pahist.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:40  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.07/06 03/07/95  17.57.19  by  O.Couet
*-- Author :    Rene Brun   03/01/89
      SUBROUTINE PAHIST
*
*            /HISTOGRAM
*
#include "hbook/hcbook.inc"
#include "hbook/hcbits.inc"
#include "paw/pawcom.inc"
#include "paw/pcpatl.inc"
#include "paw/pcchar.inc"
#include "paw/pcrang.inc"
#include "paw/pchist.inc"
#include "paw/pcaddr.inc"
#include "paw/pawnpu.inc"
#include "paw/quest.inc"
      CHARACTER*32 CHOPT
      CHARACTER*32 CHVECT
      DIMENSION PAR(50)
      CHARACTER*32 CHPATH(20)
      LOGICAL LOANGL
*.______________________________________
*
      CALL KUPATL(CHPATL,NPAR)
*.______________________________________
*
*             /HISTOGRAM/FILE
*
      IF(CHPATL.EQ.'FILE')THEN
         CALL PAWROP('HBOOK')
         GO TO 999
      ENDIF
*.______________________________________
*
*             /HISTOGRAM/LIST
*
      IF(CHPATL.EQ.'LIST')THEN
         CALL KUALFA
         CALL KUGETC(CHOPT,NCH)
         CALL HLDIR(' ',CHOPT)
         GO TO 999
      ENDIF
*.______________________________________
*
*             /HISTOGRAM/DELETE
*
      IF(CHPATL.EQ.'DELETE')THEN
         CALL PAGETI(ID)
         CALL HDELET(ID)
         GO TO 999
      ENDIF
*.______________________________________
*
*             /HISTOGRAM/PROJECT
*
      IF(CHPATL.EQ.'PROJECT')THEN
         CALL KUGETC(CHID,N)
         CALL HGETID(CHID)
         IF(LCID.LE.0)GO TO 999
         CALL HFILPR(ID)
         CALL HSETCD
         GO TO 999
      ENDIF
*.______________________________________
*
*             /HISTOGRAM/COPY
*
      IF(CHPATL.EQ.'COPY')THEN
         CALL KUGETC(CHID,N)
         CALL HGETID(CHID)
         IF(LCID.LE.0)GO TO 999
         IDC=ID
         IF(IDC.EQ.0)THEN
            CALL HSETCD
            CALL HBUG('Option ID=0 currently not supported','HCOPY',0)
            GO TO 999
         ENDIF
         CALL PAGETI(ID2)
         CALL KUGETS(CHTITL,NCH)
         IF(LFIX.EQ.0)THEN
            IF (ICRANG.NE.0) THEN
               CALL HCOPYR(IDC,ID2,CHTITL,IBX1,IBX2,IBY1,IBY2,' ')
            ELSE
               CALL HCOPY(IDC,ID2,CHTITL)
            ENDIF
         ELSE
            IF(NCH.EQ.0)CHTITL=CHID
            CALL HCOPYP(ID2,CHTITL)
         ENDIF
         IF(KHRIN.NE.0)CALL HDELET(IDC)
         CALL HSETCD
         GO TO 999
      ENDIF
*.______________________________________
*
*             /HISTOGRAM/PLOT
*
      IF(CHPATL.EQ.'PLOT')THEN
         CHOPT=' '
         IF(NPAR.NE.0)THEN
            CALL KUGETC(CHID,N)
            I=INDEX(CHID,'+')
            IF(I.GE.2)THEN
               CHTEMP=CHID(1:I-1)
               CALL HGETID(CHTEMP)
               LOANGL=.FALSE.
               IF(I1.NE.0)THEN
                  THETA=1.
                  PHI=-1.
               ELSE
                  THETA=ANGLE1
                  PHI=ANGLE2
               ENDIF
               CHPATL='LEGO'
               GOTO 40
            ENDIF
            IF(CHID.EQ.'*')CHID='0'
            CALL HGETID(CHID)
            IF(ID.NE.0.AND.LCID.LE.0)GO TO 999
            CALL KUGETC(CHOPT,NCH)
         ENDIF
*
         CALL PAPLOT(ID,CHOPT,CHCASE,NUM,ICRANG,ICX1,ICX2,ICY1,ICY2)
*
         IF(NPAR.NE.0) THEN
            IF (JMHIST .NE. 0) THEN
               CALL HCDIR(HPATH,'R')
               CALL JUMPST(JMHIST)
               IF (I1 .NE. 0) THEN
                  IT = 1
               ELSEIF (I230 .NE. 0) THEN
                  IT = 2
               ENDIF
               CALL JUMPX2(ID, IT)
            ENDIF
            CALL HSETCD
         ENDIF
         CALL PAUTIT(' ')
         GO TO 999
      ENDIF
*.______________________________________
*
*             /HISTOGRAM/ZOOM
*

      IF(CHPATL.EQ.'ZOOM')THEN
         IF(NPAR.EQ.0)THEN
            IDHS=0
            ICMINS=1
            ICMAXS=1
   10       CALL HPLOC(-1,NTLOC1,XLOC1,YLOC1,IDH1,ICX1,ICY1,ISTAT)
            IF(ISTAT.EQ.0)GO TO 999
            CALL HPLOC(-1,NTLOC2,XLOC2,YLOC2,IDH2,ICX2,ICY2,ISTAT)
            IF(ISTAT.EQ.0)GO TO 999
            IF(IDH1.EQ.0.AND.IDH2.EQ.0)GO TO 10
            IF(IDH1.EQ.0.AND.IDH2.NE.0)THEN
               IDH=IDH2
               ICMIN=ICMINS
               IF(IDH.NE.IDHS)ICMIN=1
               ICMAX=ICX2
            ELSEIF(IDH2.EQ.0.AND.IDH1.NE.0)THEN
               IDH=IDH1
               ICMIN=ICX1
               ICMAX=ICMAXS
               IF(IDH.NE.IDHS)ICMAX=9999
            ELSE
               IF(IDH1.NE.IDH2)GO TO 10
               IDH=IDH1
               ICMIN=ICX1
               ICMAX=ICX2
            ENDIF
            ID=IDH
            IF(ICMIN.LT.ICMAX)THEN
               CALL HPLZOM(IDH,' ',ICMIN,ICMAX)
               ICMINS=ICMIN
               ICMAXS=ICMAX
            ELSE
               CALL HPLZOM(IDH,' ',ICMAX,ICMIN)
               ICMINS=ICMAX
               ICMAXS=ICMIN
            ENDIF
            IDHS=IDH
            CALL PAUTIT(' ')
            GO TO 10
         ENDIF
*        ... NPAR.NE.0
         CALL KUGETC(CHID,N)
         CALL HGETID(CHID)
         IF(ID.NE.0.AND.LCID.LE.0)GO TO 999
         CALL KUGETC(CHOPT,NCH)
         CALL KUGETI(ICMIN)
         CALL KUGETI(ICMAX)
         CALL HPLZOM(ID,CHOPT,ICMIN,ICMAX)
         CALL PAUTIT(' ')
         CALL HSETCD
         GO TO 999
      ENDIF
*.______________________________________
*
*             /HISTOGRAM/MANY_PLOTS
*
      IF(CHPATL.EQ.'MANY_PLOTS')THEN
         NBHIST = 0
         VMALL  = -1.E+30
         CALL KUGETC(CHPATH(1),N)
         DO 20 I=1,10
            CALL KUGETL(CHPATH(I),N)
            IF(N.LE.0)GOTO 21
            NBHIST = NBHIST+1
            CALL HGETID(CHPATH(I))
            IF(ID.NE.0.AND.LCID.LE.0)GO TO 999
            VM=HMAX(ID)
            IF(VM.GT.VMALL)THEN
               VMALL=VM
            ENDIF
            CALL HSETCD
   20    CONTINUE
   21    CHID=CHPATH(1)
         CALL HGETID(CHID)
         ID1=ID
         CALL HMAXIM(ID,1.1*VMALL)
         CALL HPLOT(ID,'    ',CHCASE,NUM)
         DO 30 I=2,NBHIST
            CHID=CHPATH(I)
            CALL HGETID(CHID)
            CALL HPLOT(ID,'S',CHCASE,NUM)
            CALL HSETCD
   30    CONTINUE
         CALL HIDOPT(ID1,'AUTO')
         GO TO 999
      ENDIF
*.______________________________________
*
*             /HISTOGRAM/LEGO
*
      LOANGL=.TRUE.
   40 IF(CHPATL.EQ.'LEGO')THEN
         IF(LOANGL)THEN
            THETA=30.
            PHI=30.
         ENDIF
         IF(NPAR.NE.0.)THEN
            IF(.NOT.LOANGL)GOTO 50
            CALL KUGETC(CHID,N)
   50       I=INDEX(CHID,'+')
            IF(I.GE.2)THEN
               CHTEMP=CHID(1:I-1)
               CALL HGETID(CHTEMP)
               IF(ID.NE.0)THEN
                  PAR(1)=0.
                  PAR(2)=0.
                  PAR(3)=0.
                  PAR(4)=0.
                  PAR(5)=-ICX1
                  IQUEST(60)=ICX2
                  PAR(6)=-ICY1
                  IQUEST(61)=ICY2
                  CALL HPLTAB(ID,6,PAR,'LEGO+')
               ENDIF
               CHTEMP=CHID
               N=LENOCC(CHTEMP)
               CHID=CHTEMP(I+1:N)
               GOTO 50
            ENDIF
            CALL HGETID(CHID)
            IF(ID.NE.0.AND.LCID.LE.0)GO TO 999
            IF(LOANGL)THEN
               CALL KUGETR(THETA)
               CALL KUGETR(PHI)
            ENDIF
            CALL KUGETC(CHOPT,NCH)
         ENDIF
         PAR(1)=THETA
         PAR(2)=PHI
         PAR(3)=0.
         PAR(4)=0.
         PAR(5)=-ICX1
         IQUEST(60)=ICX2
         PAR(6)=-ICY1
         IQUEST(61)=ICY2
         IF(INDEX(CHOPT,'LEGO').EQ.0)THEN
            IF(INDEX(CHOPT,'1').NE.0)THEN
               CHOPT='LEGO1'
            ELSEIF(INDEX(CHOPT,'2').NE.0)THEN
               CHOPT='LEGO2'
            ELSE
               CHOPT='LEGO'
            ENDIF
         ELSEIF(LENOCC(CHOPT).GT.5)THEN
            PAR(1)=ANGLE1
            PAR(2)=ANGLE2
         ENDIF
         CALL HPLTAB(ID,6,PAR,CHOPT)
         IF(NPAR.NE.0)CALL HSETCD
         CALL PAUTIT(' ')
         GO TO 999
      ENDIF
*.______________________________________
*
*             /HISTOGRAM/SURFACE
*
      IF(CHPATL.EQ.'SURFACE')THEN
         THETA=30.
         PHI=30.
         IF(NPAR.NE.0)THEN
            CALL KUGETC(CHID,N)
            CALL HGETID(CHID)
            IF(ID.NE.0.AND.LCID.LE.0)GO TO 999
            CALL KUGETR(THETA)
            CALL KUGETR(PHI)
            CALL KUGETC(CHOPT,NCH)
         ENDIF
         PAR(1)=THETA
         PAR(2)=PHI
         PAR(3)=0.
         PAR(4)=0.
         PAR(5)=-ICX1
         IQUEST(60)=ICX2
         PAR(6)=-ICY1
         IQUEST(61)=ICY2
         IF(INDEX(CHOPT,'1').NE.0)THEN
            CHOPT='SURF1'
         ELSEIF(INDEX(CHOPT,'2').NE.0)THEN
            CHOPT='SURF2'
         ELSEIF(INDEX(CHOPT,'3').NE.0)THEN
            CHOPT='SURF3'
         ELSEIF(INDEX(CHOPT,'4').NE.0)THEN
            CHOPT='SURF4'
         ELSE
            CHOPT='SURF'
         ENDIF
         CALL HPLTAB(ID,6,PAR,CHOPT)
         IF(NPAR.NE.0)CALL HSETCD
         CALL PAUTIT(' ')
         GO TO 999
      ENDIF
*.______________________________________
*
*             /HISTOGRAM/CONTOUR
*
      IF(CHPATL.EQ.'CONTOUR')THEN
         IRES=10
         IMODE=1
         IF(NPAR.NE.0)THEN
            CALL KUGETC(CHID,N)
            CALL HGETID(CHID)
            IF(ID.NE.0.AND.LCID.LE.0)GO TO 999
            CALL KUGETI(IRES)
            CALL KUGETC(CHOPT,NCH)
            IF(INDEX(CHOPT,'0').NE.0)IMODE=0
            IF(INDEX(CHOPT,'2').NE.0)IMODE=2
            IF(INDEX(CHOPT,'3').NE.0)IMODE=3
         ENDIF
         PAR(1)=IRES
         PAR(2)=IMODE
         PAR(3)=0.
         PAR(4)=0.
         PAR(5)=-ICX1
         IQUEST(60)=ICX2
         PAR(6)=-ICY1
         IQUEST(61)=ICY2
         NP=6
         IF(NPAR.EQ.4)THEN
            CALL KUGETC(CHVECT,NLEN)
            CALL KUVECT(CHVECT,LX1,LX2)
            IF(LX1.NE.0)THEN
               NP=MIN(NP+LX2-LX1+1,50)
               NP2=MIN(NP,44)
               DO 60 I=1,NP2
                  PAR(6+I)=Q(LX1+I-1)
   60          CONTINUE
            ENDIF
         ENDIF
         IF(INDEX(CHOPT,'S').NE.0)THEN
            CHOPT='CONTS'
         ELSE
            CHOPT='CONT'
         ENDIF
         CALL HPLTAB(ID,NP,PAR,CHOPT)
         IF(NPAR.NE.0)CALL HSETCD
         CALL PAUTIT(' ')
      ENDIF
*
  999 END
