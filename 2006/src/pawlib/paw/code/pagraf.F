*
* $Id: pagraf.F,v 1.3 1999/01/19 14:22:15 couet Exp $
*
* $Log: pagraf.F,v $
* Revision 1.3  1999/01/19 14:22:15  couet
* - command METAFILE: - unused parameter CHMETA removed
*                     - help updated
*
* Revision 1.2  1998/04/09 10:09:15  couet
* - New option in VLOCATE:
*       Q  Do not print the number of points entered
*
* Revision 1.1.1.1  1996/03/01 11:38:40  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.07/00 26/04/95  16.46.10  by  O.Couet
*-- Author :    Rene Brun   03/01/89
      SUBROUTINE PAGRAF
*
#include "hbook/hcbook.inc"
#include "paw/pawlun.inc"
#include "paw/pcpatl.inc"
#include "paw/quest.inc"
#include "paw/pcchar.inc"
#include "paw/pcbuff.inc"
#include "paw/pcwk.inc"
#include "paw/paloc.inc"
#include "paw/pchtag.inc"
*
*           /GRAPHICS
*
      CHARACTER*32 VECX,VECY
      CHARACTER*4  CHOPT
      CHARACTER*32 CHLOC(7)
*
      PARAMETER (MAXP=1002)
      REAL XP(MAXP),YP(MAXP)
      EQUIVALENCE (XP(1),PAWBUF(1)),(YP(1),PAWBUF(1003))
*
      DIMENSION RLOW(NCHTAG),RHIGH(NCHTAG)
      EQUIVALENCE (RLOW(1),PAWBUF(1)),(RHIGH(1),PAWBUF(NCHTAG+1))
*
      LOGICAL HEXIST,IGM100,LOWIP,LOLOGX,LOLOGY
C.
C.    ------------------------------------------------------------------
C.
      CALL KUPATL(CHPATL,NPAR)
C
C           SVP
C
      IF(CHPATL.EQ.'SVP')THEN
         CALL KUGETI(NT)
         CALL KUGETR(X1)
         CALL KUGETR(X2)
         CALL KUGETR(Y1)
         CALL KUGETR(Y2)
         CALL ISVP(NT,X1,X2,Y1,Y2)
         GO TO 999
      ENDIF
C
C           SWN
C
      IF(CHPATL.EQ.'SWN')THEN
         CALL KUGETI(NT)
         CALL KUGETR(X1)
         CALL KUGETR(X2)
         CALL KUGETR(Y1)
         CALL KUGETR(Y2)
         CALL HPLWN(NT,X1,X2,Y1,Y2)
         GO TO 999
      ENDIF
C
C           SELNT
C
      IF(CHPATL.EQ.'SELNT')THEN
         CALL KUGETI(NT)
         CALL HPLSEL(NT)
         GO TO 999
      ENDIF
C.
C             SIZE
C
      IF(CHPATL.EQ.'SIZE')THEN
         CALL KUGETR(XSIZE)
         CALL KUGETR(YSIZE)
         CALL HPLSET('XSIZ',XSIZE)
         CALL HPLSET('YSIZ',YSIZE)
         GO TO 999
      ENDIF
C
C             ZONE
C
      IF(CHPATL.EQ.'ZONE')THEN
         CALL KUGETI(NX)
         CALL KUGETI(NY)
         CALL KUGETI(IFIRST)
         CALL KUGETC(CHOPT,NCH)
         CALL HPLZON(NX,NY,IFIRST,CHOPT)
         GO TO 999
      ENDIF
C
C             SET
C
      IF(CHPATL.EQ.'SET')THEN
         CALL KUGETC(CHOPT,NCH)
         CALL KUGETR(VAL)
         IF (NPAR.EQ.1) THEN
            IF (CHOPT.EQ.'PMCI') VAL = 1.
            IF (CHOPT.EQ.'FACI') VAL = 1.
            IF (CHOPT.EQ.'PLCI') VAL = 1.
            IF (CHOPT.EQ.'TXCI') VAL = 1.
            IF (CHOPT.EQ.'LTYP') VAL = 1.
            IF (CHOPT.EQ.'FASI') VAL = 1.
            IF (CHOPT.EQ.'LWID') VAL = 1.
            IF (CHOPT.EQ.'MTYP') VAL = 1.
            IF (CHOPT.EQ.'CHHE') VAL = 0.28
         ENDIF
         CALL HPLSET(CHOPT,VAL)
         GO TO 999
      ENDIF
C
C             OPTION
C
      IF(CHPATL.EQ.'OPTION')THEN
         CALL KUGETC(CHOPT,NCH)
         CALL HPLOPT(CHOPT,1)
         GO TO 999
      ENDIF
C
C             NEXT
C
      IF(CHPATL.EQ.'NEXT')THEN
         CALL HPLNEW
         GO TO 999
      ENDIF
C
C             CLR
C
      IF(CHPATL.EQ.'CLR')THEN
         CALL ICLRWK(0,1)
         GO TO 999
      ENDIF
C
C             METAFILE
C
      IF(CHPATL.EQ.'METAFILE')THEN
         CALL KUGETI(I)
         LUN=IABS(I)
         CALL KUGETI(METAFL)
         IF(IGM100(METAFL))THEN
            IF(LUN.LT.100)THEN
               IF(I.LT.0)THEN
                  I=I-100
               ELSEIF(I.GT.0)THEN
                  I=I+100
               ENDIF
            ELSEIF(LUN.GT.100)THEN
               LUN=LUN-100
            ENDIF
         ENDIF
         IF(LUN.NE.0)THEN
            CALL PALUNF(LUN,2,IFREE)
            IF(IFREE.EQ.0)GO TO 999
         ENDIF
         LUNIT(LUN)=8
         IF(IWK.LE.0)THEN
            IF(I.GT.0)THEN
               IWK=-2
            ELSEIF(I.LT.0)THEN
               IWK=-1
            ELSE
               IWK=0
            ENDIF
         ENDIF
         CALL IGMETA(I,METAFL)
         GO TO 999
      ENDIF
C
C             LOCATE
C
      IF(CHPATL.EQ.'LOCATE')THEN
         CALL KUGETC(CHTEMP,NCHTMP)
         CALL IZCTOI(CHTEMP,NTPRI)
         IF(IQUEST(1).NE.0)THEN
#if defined(CERNLIB_COMIS)
            CALL CLTOU(CHTEMP)
            IF(CHTEMP.EQ.'RESET')THEN
               JADLOC = 0
            ELSE
               CALL PAWCS
               CALL PAWFCA(CHTEMP,NCHTMP,JADLOC,1)
            ENDIF
#endif
            IF(IWK.EQ.999)GOTO 999
            NTPRI = -1
         ELSE
            JADLOC = 0
            IF(IWK.EQ.999)THEN
               PRINT*,'*** In Paw++ NTPRI must be a routine name'
               GOTO 999
            ENDIF
         ENDIF
         CALL KUGETC(CHOPT,NCH)
         CALL KUGETI(IWKID)
*
         IF(NTPRI.GE.0)THEN
            IF(NTPRI.NE.1)CALL ISELNT(1)
            CALL ISELNT(NTPRI)
         ELSE
            CALL HPLGIV(XL,YL,XH,YH)
            NTHIST=IQUEST(12)
            CALL ISELNT(1)
            DO 10 I=10,NTHIST,10
               CALL ISELNT(I)
   10       CONTINUE
         ENDIF
         IF(INDEX(CHOPT,'+').NE.0.OR.JADLOC.NE.0)THEN
            LCDNR=11
         ELSE
            LCDNR=21
         ENDIF
*
         IOPTI=INDEX(CHOPT,'I')
         IOPTS=INDEX(CHOPT,'S')
         IF(IOPTS.NE.0)LCDNR=-LCDNR
         IF(IOPTI.NE.0)THEN
            LCDNR=11
            CALL IGZSET('S')
            CALL IGZSET('G')
            CALL IGSET('DRMD',2.)
            CALL IGSET('BORD',0.)
            CALL IGSET('FAIS',1.)
            CALL IGSET('FACI',2.)
            ICX2=0
            ICXF=0
            SIGM=0.
            SIGMN=0.
            IDHC=0
            LOWIP=.FALSE.
         ENDIF
*
         CHOPT(4:4)='P'
         NLOC=0
         IITYP=IGIWTY(1)
   20    CALL IRQLC(IWKID,LCDNR,ISTAT,NT,X,Y)
         XLOC=X
         YLOC=Y
         CALL HPLCHA(NT,X,Y,IDH,XLOC,YLOC,ICX,ICY)
         IF(IQUEST(1).NE.0)THEN
            LOLOGX=.TRUE.
         ELSE
            LOLOGX=.FALSE.
         ENDIF
         IF(IQUEST(2).NE.0)THEN
            LOLOGY=.TRUE.
         ELSE
            LOLOGY=.FALSE.
         ENDIF
#if defined(CERNLIB_COMIS)
         IF(JADLOC.NE.0)THEN
            IQUEST(80) = ISTAT
            CALL CSJCAL(JADLOC,0,X,X,X,X,X,X,X,X,X,X)
         ENDIF
#endif
         IF(IOPTS.NE.0.AND.NT.EQ.0)NT=-1
         IF(IITYP.GT.10)NLOC=NLOC+1
         CALL KUALFA
         IF(NLOC.LE.20.AND.ISTAT.NE.0)THEN
*
* Ntuples
*
            IF(IQUEST(3).NE.0)THEN
               NVAR=NCHTAG-2
               CALL HGIVEN(IDH,CHTAG(1),NVAR,CHTAG(3),RLOW,RHIGH)
               CALL HGNF(IDH,ICX,XP(3),IERROR)
               IF(IERROR.NE.0)GOTO 20
               CHTAG(1)='Ntuple ID'
               CHTAG(2)='Event Number'
               MAXLEN=0
               DO 30 I=1,NVAR
                  ILEN=LENOCC(CHTAG(I))
                  IF(ILEN.GT.MAXLEN)MAXLEN=ILEN
   30          CONTINUE
               MAXLEN=MAXLEN+3
               CALL IZITOC(IDH,CHTAG(1)(MAXLEN:))
               CALL IZITOC(ICX,CHTAG(2)(MAXLEN:))
               DO 40 I=3,NVAR+2
                  CALL IZRTOC(XP(I),CHTAG(I)(MAXLEN:))
   40          CONTINUE
               CALL IGMESS(NVAR+2,CHTAG,'LOCATE',CHOPT)
               GOTO 20
            ENDIF
*
* Histograms
*
            CONT=0.
            IF(HEXIST(IDH))THEN
               IF(ICX.NE.0.AND.ICY.NE.0)THEN
                  CONT=HIJ(IDH,ICX,ICY)
               ELSE
                  CONT=HI(IDH,ICX)
               ENDIF
               CALL HGSTAT(IDH,XP)
            ENDIF
*
            IF(IOPTI.NE.0.AND.HEXIST(IDH))THEN
               IF(LCDNR.GT.0)THEN
                  IF(NT.GT.1)THEN
                     ICXF=ICX
                     ICX2=ICX
                     CALL ISELNT(NT)
                     CALL IGQWK(1,'NTWN',YP)
                     IF(.NOT.LOLOGY)YP(3)=MAX(YP(3),0.)
                     CALL HIX(IDH,ICX,X1)
                     CALL HIX(IDH,ICX+1,X2)
                     SIGMN=CONT*(X2-X1)
                     SIGM=CONT
                     TCONT=CONT
                     IF(LOLOGY)THEN
                        IF(CONT.LE.0.)THEN
                           TCONT=0.
                        ELSE
                           TCONT=LOG10(CONT)
                        ENDIF
                     ENDIF
                     IF(LOLOGX)THEN
                        IF(X1.GT.0.)X1=LOG10(X1)
                        IF(X2.GT.0.)X2=LOG10(X2)
                     ENDIF
                     CALL IGBOX(X1,X2,YP(3),TCONT)
                     LCDNR=-11
                     IDHC=IDH
                     WRITE(CHLOC(1),'('' Histogram   '',I8)')IDHC
                     WRITE(CHLOC(2),'('' From bin    '',I8)')ICXF
                     WRITE(CHLOC(3),'('' To bin      '',I8)')ICX
                     WRITE(CHLOC(4),'('' Integration '',G12.4)')SIGM
                     WRITE(CHLOC(5),'('' Int/ALLCHA  '',G12.4)')SIGM
     +                                                         /XP(2)
                     WRITE(CHLOC(6),'('' Math. Int.  '',G12.4)')SIGMN
                     CALL IGSET('DRMD',1.)
                     CALL IGMESS(6,CHLOC,'LOCATE',CHOPT)
                     CALL IGSET('DRMD',2.)
                  ENDIF
               ELSE
                  IF(NT.GT.1)THEN
                     IF(ICX2.NE.ICX)THEN
                        LOWIP=.FALSE.
                        IF((ICXF-ICX2)*(ICXF-ICX).LT.0)ICX=ICXF
                        IS=0
                        RP=1.
                        IF(ICX.GT.ICX2)THEN
                           INCR=1
                           IF(ICX.LE.ICXF)THEN
                              IS=-1
                              RP=-1.
                           ENDIF
                        ELSE
                           INCR=-1
                           IF(ICX.GE.ICXF)THEN
                              IS=1
                              RP=-1.
                           ENDIF
                        ENDIF
                        DO 50 I=ICX2+INCR,ICX,INCR
                           CONT=HI (IDHC,I+IS)
                           CALL HIX(IDHC,I+IS,X1)
                           CALL HIX(IDHC,I+1+IS,X2)
                           SIGMN=SIGMN+RP*(CONT*(X2-X1))
                           SIGM=SIGM+RP*CONT
                           TCONT=CONT
                           IF(LOLOGY)THEN
                              IF(CONT.LE.0.)THEN
                                 TCONT=0.
                              ELSE
                                 TCONT=LOG10(CONT)
                              ENDIF
                           ENDIF
                           IF(LOLOGX)THEN
                              IF(X1.GT.0.)X1=LOG10(X1)
                              IF(X2.GT.0.)X2=LOG10(X2)
                           ENDIF
                           CALL IGBOX(X1,X2,YP(3),TCONT)
   50                   CONTINUE
                        WRITE(CHLOC(1),'('' Histogram   '',I8)')
     +                  IDHC
                        WRITE(CHLOC(2),'('' From bin    '',I8)')
     +                  ICXF
                        WRITE(CHLOC(3),'('' To bin      '',I8)')
     +                  ICX
                        WRITE(CHLOC(4),'('' Integration '',G12.4)')
     +                  SIGM
                        WRITE(CHLOC(5),'('' Int/ALLCHA  '',G12.4)')
     +                  SIGM/XP(2)
                        WRITE(CHLOC(6),'('' Math. Int.  '',G12.4)')
     +                  SIGMN
                        CALL IGSET('DRMD',1.)
                        CALL IGMESS(6,CHLOC,'LOCATE',CHOPT)
                        CALL IGSET('DRMD',2.)
                        ICX2=ICX
                     ENDIF
                  ENDIF
                  IF(NT.LE.1.OR.ISTAT.EQ.1)THEN
                     LOWIP=.TRUE.
                     INCR=1
                     IF(ICX2.GT.ICXF)INCR=-1
                     DO 60 I=ICX2,ICXF,INCR
                        CONT=HI (IDHC,I)
                        CALL HIX(IDHC,I,X1)
                        CALL HIX(IDHC,I+1,X2)
                        TCONT=CONT
                        IF(LOLOGY)THEN
                           IF(CONT.LE.0.)THEN
                              TCONT=0.
                           ELSE
                              TCONT=LOG10(CONT)
                           ENDIF
                        ENDIF
                        IF(LOLOGX)THEN
                           IF(X1.GT.0.)X1=LOG10(X1)
                           IF(X2.GT.0.)X2=LOG10(X2)
                        ENDIF
                        CALL IGBOX(X1,X2,YP(3),TCONT)
   60                CONTINUE
                     CALL IGMESS(1,' ',' ','D')
                     LCDNR=11
                     SIGM=0.
                     SIGMN=0.
                  ENDIF
               ENDIF
*
            ELSE IF(IOPTI.EQ.0) THEN
               CHLOC(1) = 'Histogram ID'
               CHLOC(2) = 'X channel'
               CHLOC(3) = 'Y channel'
               CHLOC(4) = 'NT'
               CHLOC(5) = 'X'
               CHLOC(6) = 'Y'
               CHLOC(7) = 'Z'
               CALL IZITOC(IDH ,CHLOC(1)(15:))
               CALL IZITOC(ICX ,CHLOC(2)(15:))
               CALL IZITOC(ICY ,CHLOC(3)(15:))
               CALL IZITOC(NT  ,CHLOC(4)(15:))
               CALL IZRTOC(XLOC,CHLOC(5)(15:))
               CALL IZRTOC(YLOC,CHLOC(6)(15:))
               CALL IZRTOC(CONT,CHLOC(7)(15:))
               CALL IGMESS(7,CHLOC,'LOCATE',CHOPT)
            ENDIF
#if defined(CERNLIB_IBM)
            IF(IOPTS.EQ.0.AND.IOPTI.EQ.0)THEN
               IF(IWK.LE.10)GO TO 20
               CALL KUPROC('Type CR to continue or Quit',CHOPT,NCH)
               IF(NCH.NE.0.AND.CHOPT(1:1).EQ.'Q')GO TO 999
            ENDIF
#endif
            GO TO 20
         ENDIF
         CALL IGMESS(1,' ',' ','C')
         IF(IOPTI.NE.0.AND.HEXIST(IDH))THEN
            IF(.NOT.LOWIP)THEN
               INCR=1
               IF(ICX2.GT.ICXF)INCR=-1
               DO 70 I=ICX2,ICXF,INCR
                  CONT=HI (IDHC,I)
                  CALL HIX(IDHC,I,X1)
                  CALL HIX(IDHC,I+1,X2)
                  TCONT=CONT
                  IF(LOLOGY)THEN
                     IF(CONT.LE.0.)THEN
                        TCONT=0.
                     ELSE
                        TCONT=LOG10(CONT)
                     ENDIF
                  ENDIF
                  IF(LOLOGX)THEN
                     IF(X1.GT.0.)X1=LOG10(X1)
                     IF(X2.GT.0.)X2=LOG10(X2)
                  ENDIF
                  CALL IGBOX(X1,X2,YP(3),TCONT)
   70          CONTINUE
            ENDIF
            CALL IGSET('DRMD',1.)
            CALL IGZSET('R')
         ENDIF
         GO TO 999
      ENDIF
C
C             VLOCATE
C
      IF(CHPATL.EQ.'VLOCATE')THEN

         CALL KUGETC(VECX,NCHNMX)
         CALL KUGETC(VECY,NCHNMY)
         CALL KUGETC(CHOPT,NCH)
         CALL KUGETI(NTPRI)
         CALL KUGETI(IWKID)

         NLOC=MAXP
         CALL PAWLOC(NLOC,XP,YP,NTPRI,IWKID,CHOPT)

         IF (NLOC.EQ.0) GO TO 999

         IF (INDEX(CHOPT,'Q').EQ.0) THEN
            CALL KUALFA
            PRINT *,NLOC,' points have been hit'
         ENDIF

         CALL KUVEC(VECX,XP,NLOC,'W-')
         IF (IQUEST(1).NE.0) GO TO 90

         CALL KUVEC(VECY,YP,NLOC,'W-')
         IF (IQUEST(1).NE.0) GO TO 90

      ENDIF
C
C             HMOVE
C
      IF(CHPATL.EQ.'HMOVE')THEN
   80    CALL PAHMOV(ISTAT)
#if defined(CERNLIB_APOLLO)
         IF(ISTAT.NE.0)GO TO 80
#endif
#if !defined(CERNLIB_APOLLO)
         GO TO 999
#endif
      ENDIF
*
      GO TO 999
*
   90 CALL KUALFA
      PRINT *,'Illegal syntax in vector name'
*
  999 END
