*
* $Id: pamloc.F,v 1.2 1996/09/11 14:59:49 couet Exp $
*
* $Log: pamloc.F,v $
* Revision 1.2  1996/09/11 14:59:49  couet
* - Hgetnt and Hgetn2 (old qp routines) are now replaced by hntld
*
* Revision 1.1.1.1  1996/03/01 11:39:06  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.05/19 20/09/94  20.32.58  by  Fons Rademakers
*-- Author :    O.Couet   22/10/92
      SUBROUTINE PAMLOC(IX,IY,NBI,IWIN)
*.===========>
*.
*.   Call back routine for the graphics window
*.
*..==========> (O.Couet)
#include "paw/pcbuff.inc"
#include "paw/quest.inc"
#include "paw/pntold.inc"
#include "paw/pawcfu.inc"
#include "paw/paloc.inc"
      PARAMETER (MGUIDL=199)
      COMMON /KCGUID/ GUID(MGUIDL)
      CHARACTER*80 GUID
      CHARACTER*80 CHMESS(MGUIDL)
      EQUIVALENCE (CHMESS(1),GUID(1))
      DIMENSION RLOW(MGUIDL),RHIGH(MGUIDL)
      EQUIVALENCE (RLOW(1),PAWBUF(1)),(RHIGH(1),PAWBUF(MGUIDL+1))
      DIMENSION RVAL(20)
      EQUIVALENCE (RVAL(1),PAWBUF(1))
*
      PARAMETER (MAXP=1002)
      REAL XP(MAXP)
      EQUIVALENCE (XP(1),PAWBUF(1))
*
* DRAG1  = If TRUE the dragging mode is on with button 1
* DRAG2  = If TRUE the dragging mode is on with button 2
* IDH    = Current histogram ID
* IDHI   = Histogram currently integrated
* IHTYPE = Object type
* ICX    = Current X channel number
* ICY    = Current Y channel number
* ICX1   = First channel number for the integration
* ICX2   = Last channel number for the integration
* STINT  = .TRUE. if the integration of a 1D histo start
* YBOT   = Y bottom value to draw integration boxes
* NT     = Current normalization tranformation
* NTI    = Current integration normalization tranformation
* LOLOGX = TRUE if the histogram is log scale on X
* LOLOGY = TRUE if the histogram is log scale on Y
* NVARMX = Maximal number of variables to be displayed in a Ntuple
* MXEWW  = Maximale echo window width
*
      PARAMETER (NVARMX=15)
***
      SAVE XVP1,XVP2,YVP1,YVP2,XLOCS,YLOCS,NTS,R2BUF,NTI
***
      SAVE IDH,IDHI,IHTYPE,ICX1,ICX2,YBOT,ICX,ICY,NT,MXEWW
      SAVE SIGM,SIGMN
      LOGICAL  HEXIST,DRAG1,DRAG2,LOLOGX,LOLOGY,STINT
      SAVE    DRAG1,DRAG2,LOLOGX,LOLOGY,STINT
      DIMENSION IVARS(NVARMX)
      CHARACTER*32 CH32
      REAL*8       DVAL
      DATA DRAG1,DRAG2   /.FALSE.,.FALSE./
      DATA LOLOGX,LOLOGY /.FALSE.,.FALSE./
      DATA STINT /.TRUE./
      DATA MXEWW /0/
*.______________________________________
*
      NB = NBI
      IF(NB.EQ.0.AND..NOT.(DRAG1.OR.DRAG2))RETURN
      CALL IGZSET('S')
      CALL IGZSET('G')
*.______________________________________
*
*              Enter window
*
      IF(NB.EQ.999)THEN
         DRAG1 = .FALSE.
         DRAG2 = .FALSE.
         GOTO 70
      ENDIF
*.______________________________________
*
*              Leave window
*
      IF(NB.EQ.-999)THEN
         CALL KMXMES(0,0,0,0,' ',' ','C')
         GOTO 70
      ENDIF
*.______________________________________
*
*              Compute the Picked object
*
* IHTYPE = 0 : No histogram picked
* IHTYPE = 1 : A 1d histogram is picked
* IHTYPE = 2 : A 2d histogram is picked
* IHTYPE = 3 : A ntuple is picked
*
      X      = FLOAT(IX)
      Y      = FLOAT(IY)
      IHTYPE = 0
      CALL IZPICT(' ','QO')
      CALL IRQLC(1,99,ISTAT,NT,X,Y)
      XLOC = X
      YLOC = Y
      CALL HPLCHA(NT,X,Y,IDH,XLOC,YLOC,ICX,ICY)
*
      IF(IQUEST(1).NE.0)THEN
         LOLOGX = .TRUE.
      ELSE
         LOLOGX = .FALSE.
      ENDIF
      IF(IQUEST(2).NE.0)THEN
         LOLOGY = .TRUE.
      ELSE
         LOLOGY = .FALSE.
      ENDIF
*
      IF(IDH.NE.0)THEN
         IF(IQUEST(3).NE.0)THEN
            IHTYPE = 3
         ELSEIF(ICX.NE.0.AND.ICY.NE.0)THEN
            IHTYPE = 2
            CONT   = HIJ(IDH,ICX,ICY)
         ELSE
            IHTYPE = 1
            CONT   = HI(IDH,ICX)
         ENDIF
      ENDIF
*
#if defined(CERNLIB_COMIS)
      IQUEST(80) = NB
      IQUEST(83) = IX
      IQUEST(84) = IY
      IF(DRAG1)THEN
         IQUEST(81) = 1
      ELSE
         IQUEST(81) = 0
      ENDIF
      IF(DRAG2)THEN
         IQUEST(82) = 1
      ELSE
         IQUEST(82) = 0
      ENDIF
      IF(JADLOC.NE.0)CALL CSJCAL(JADLOC,0,X,X,X,X,X,X,X,X,X,X)
#endif
*.______________________________________
*
*              Left Button Press
*
   10 IF(NB.EQ.1)THEN
         DRAG1 = .TRUE.
         STINT = .TRUE.
         ICX1  = ICX
         ICX2  = ICX
*
         CHMESS(1) = 'NT'
         CHMESS(2) = 'X'
         CHMESS(3) = 'Y'
         NLINE     = 3
         IF(IHTYPE.EQ.1)NLINE = 6
         IF(IHTYPE.EQ.2)THEN
            NLINE     = 7
            CHMESS(6) = 'Y channel'
         ENDIF
         IF(IHTYPE.EQ.1.OR.IHTYPE.EQ.2)THEN
            CHMESS(4)     = 'Histogram ID'
            CHMESS(5)     = 'X channel'
            CHMESS(NLINE) = 'Content'
         ENDIF
         IF(IHTYPE.EQ.3)THEN
            CHMESS(100) = ' '
            NVAR        = NVARMX
            CALL IZITOC(IDH,CH32)
            CALL HNTLD(CH32)
            CALL HGIVEN(IDH,CHMESS(100),NVAR,CHMESS(6),RLOW,RHIGH)
            IF (NTOLD) THEN
               CALL HGNF(IDH,ICX,XP(6),IERROR)
               IF(IERROR.NE.0)GOTO 70
            ELSE
               LOOP = MIN(NVAR,NVARMX)
               DO 20 I=6,6+LOOP-1
                  CALL PADVAR(CHMESS(I),IVARS(I-5),IERROR)
   20          CONTINUE
*
*-- make sure that we don't accidentally use dynamic memory by
*-- setting the dyn. memory offsets to 0
*
               CALL VZERO(IOFFST, MAXCLL)
*
               CALL HGNTV(IDH, VAR, NVART, ICX, IERROR)
               IF(IERROR.NE.0)GOTO 70
            ENDIF
            CHMESS(4)   = ' '
            CHMESS(4)   = 'Ntuple ID'
            CHMESS(5)   = 'Event Number'
            NLINE       = 5+MIN(NVAR,NVARMX)
         ENDIF
*
         MAXLEN = 0
         DO 30 I=1,NLINE
            ILEN                     = LENOCC(CHMESS(I))
            IF(ILEN.GT.MAXLEN)MAXLEN = ILEN
   30    CONTINUE
         MAXLEN = MAX(MAXLEN+3,MXEWW)
         MXEWW  = MAXLEN
*
         CALL IZITOC(NT  ,CHMESS(1)(MAXLEN:))
         CALL IZRTOC(XLOC,CHMESS(2)(MAXLEN:))
         CALL IZRTOC(YLOC,CHMESS(3)(MAXLEN:))
         IF(IHTYPE.EQ.2)THEN
            CALL IZITOC(ICY ,CHMESS(6)(MAXLEN:))
         ENDIF
         IF(IHTYPE.EQ.1.OR.IHTYPE.EQ.2)THEN
            CALL IZITOC(IDH ,CHMESS(4)(MAXLEN:))
            CALL IZITOC(ICX ,CHMESS(5)(MAXLEN:))
            CALL IZRTOC(CONT,CHMESS(NLINE)(MAXLEN:))
         ENDIF
         IF(IHTYPE.EQ.3)THEN
            CALL IZITOC(IDH ,CHMESS(4)(MAXLEN:))
            CALL IZITOC(ICX ,CHMESS(5)(MAXLEN:))
            DO 40 I=6,NLINE
               IF (NTOLD) THEN
                  CALL IZRTOC(XP(I),CHMESS(I)(MAXLEN:))
               ELSE
                  I2 = I-5
                  CALL PNTVAL(ICX, IVARS(I2), DVAL, IVAL, CH32, W)
                  J  = IVARS(I2)
                  IF (ITYPE(J) .EQ. 1) THEN
                     CALL IZRTOC(W, CHMESS(I)(MAXLEN:))
                  ELSEIF (ITYPE(J) .EQ. 2) THEN
                     CALL IZITOC(IVAL, CHMESS(I)(MAXLEN:))
                  ELSEIF (ITYPE(J) .EQ. 3) THEN
*
*-- Only write in hex format when bit 32 is used of unsigned int
*
                     IF (JBIT(IVAL,32) .EQ. 1) THEN
                        CALL PHXTOC(IVAL, CHMESS(I)(MAXLEN:))
                     ELSE
                        CALL IZITOC(IVAL, CHMESS(I)(MAXLEN:))
                     ENDIF
                  ELSEIF (ITYPE(J) .EQ. 4) THEN
                     IF (IVAL .EQ. 1) THEN
                        CHMESS(I)(MAXLEN:) = '.TRUE.'
                     ELSE
                        CHMESS(I)(MAXLEN:) = '.FALSE.'
                     ENDIF
                  ELSEIF (ITYPE(J) .EQ. 5) THEN
                     CHMESS(I)(MAXLEN:) = CH32
                  ENDIF
               ENDIF
   40       CONTINUE
            IF(NLINE-5.LT.NVAR)THEN
               NLINE         = NLINE+1
               CHMESS(NLINE) = 'etc ...'
            ENDIF
         ENDIF
*
         CALL IGQWK(IWIN,'MXDS',RVAL(20))
         CALL KMXMES(IQUEST(10),IQUEST(11)
     +,              30,NLINE,CHMESS,'Paw++ Locate','PL')
         GOTO 70
      ENDIF
*.______________________________________
*
*              Left Button Release
*
      IF(NB.EQ.-1)THEN
         DRAG1 = .FALSE.
*
         IF(.NOT.STINT)THEN
            INCR=1
            IF(ICX2.GT.ICX1)INCR=-1
            DO 50 I=ICX2,ICX1,INCR
               CONT = HI(IDHI,I)
               CALL HIX(IDHI,I,X1)
               CALL HIX(IDHI,I+1,X2)
               TCONT = CONT
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
               CALL IGBOX(X1,X2,YBOT,TCONT)
   50       CONTINUE
            CALL IGSET('DRMD',1.)
            CALL IGSET('2BUF',10.*IWIN+R2BUF)
            IDHI  = 0
            NTI   = 0
         ENDIF
         GOTO 70
      ENDIF
*.______________________________________
*
*              Middle Button Press
*
      IF(NB.EQ.2)THEN
         DRAG2 = .TRUE.
         CALL ISELNT(NT)
         CALL IGQWK(1,'NTVP',RVAL)
         NTS   = NT
         XVP1  = RVAL(1)
         XVP2  = RVAL(2)
         YVP1  = RVAL(3)
         YVP2  = RVAL(4)
         XLOCS = RQUEST(11)
         YLOCS = RQUEST(12)
         GOTO 70
      ENDIF
*.______________________________________
*
*              Middle Button Release
*
      IF(NB.EQ.-2)THEN
         DRAG2 = .FALSE.
         XSHFT = RQUEST(11)-XLOCS
         YSHFT = RQUEST(12)-YLOCS
         CALL IGZSET('GZ')
         CALL ISVP(NTS,MAX(0.,XVP1+XSHFT),MIN(1.,XVP2+XSHFT)
     +,                MAX(0.,YVP1+YSHFT),MIN(1.,YVP2+YSHFT))
         CALL ISELNT(NTS)
         CALL IZPICT(' ','D')
         CALL IGZSET('R')
         CALL IUWK(0,0)
         GOTO 70
      ENDIF
*.______________________________________
*
*              Dragging is on with button 1
*
      IF(DRAG1)THEN
         IF(IHTYPE.NE.1)THEN
            IF(STINT)THEN
               NB=1
               GOTO 10
            ELSE
               GOTO 70
            ENDIF
         ENDIF
*
* 1D histos integration
*
         IF(STINT)THEN
            IF(.NOT.HEXIST(IDH))GOTO 70
            CALL ISELNT(NT)
            CALL IGQWK(1,'NTWN',XP)
            YBOT = XP(3)
            IF(.NOT.LOLOGY)YBOT = MAX(YBOT,0.)
            CALL IGSET('DRMD',2.)
            CALL IGSET('BORD',0.)
            CALL IGSET('FAIS',1.)
            CALL IGSET('FACI',2.)
            CALL IGQWK(IWIN,'2BUF',R2BUF)
            CALL IGSET('2BUF',10.*IWIN)
            IF(ICX1.LE.0)ICX1 = 1
            CONT1 = HI(IDH,ICX1)
            CALL HIX(IDH,ICX1,X1)
            CALL HIX(IDH,ICX1+1,X2)
            CALL HGSTAT(IDH,XP(11))
            SIGMN  = CONT1*(X2-X1)
            SIGM   = CONT1
            TCONT1 = CONT1
               IF(LOLOGY)THEN
                  IF(CONT1.LE.0.)THEN
                     TCONT1=0.
                  ELSE
                     TCONT1=LOG10(CONT1)
                  ENDIF
               ENDIF
               IF(LOLOGX)THEN
                  IF(X1.GT.0.)X1=LOG10(X1)
                  IF(X2.GT.0.)X2=LOG10(X2)
               ENDIF
            CALL IGBOX(X1,X2,YBOT,TCONT1)
            STINT = .FALSE.
            IDHI  = IDH
            NTI   = NT
            WRITE(CHMESS(1),'(''Histogram ID '',I8)') IDHI
            WRITE(CHMESS(2),'(''From bin     '',I8)') ICX1
         ELSE
            IF(NT.NE.NTI.OR.IDH.NE.IDHI)GOTO 70
         ENDIF
         IF(ICX2.NE.ICX)THEN
            IF((ICX1-ICX2)*(ICX1-ICX).LT.0)ICX = ICX1
            IS = 0
            RP = 1.
            IF(ICX.GT.ICX2)THEN
               INCR = 1
               IF(ICX.LE.ICX1)THEN
                  IS = -1
                  RP = -1.
               ENDIF
            ELSE
               INCR = -1
               IF(ICX.GE.ICX1)THEN
                  IS = 1
                  RP = -1.
               ENDIF
            ENDIF
            DO 60 I=ICX2+INCR,ICX,INCR
               CONT = HI(IDHI,I+IS)
               CALL HIX(IDHI,I+IS,X1)
               CALL HIX(IDHI,I+1+IS,X2)
               SIGMN = SIGMN+RP*(CONT*(X2-X1))
               SIGM  = SIGM+RP*CONT
               TCONT = CONT
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
               CALL IGBOX(X1,X2,YBOT,TCONT)
   60       CONTINUE
            ICX2 = ICX
*
            WRITE(CHMESS(3),'(''To bin       '',I8)') ICX
            WRITE(CHMESS(4),'(''Integration  '',G12.4)') SIGM
            WRITE(CHMESS(5),'(''Int/ALLCHA   '',G12.4)') SIGM/XP(12)
            WRITE(CHMESS(6),'(''Math. Int.   '',G12.4)') SIGMN
            CALL IGQWK(IWIN,'MXDS',RVAL(20))
            CALL IGSET('DRMD',1.)
            CALL KMXMES(IQUEST(10),IQUEST(11)
     +,                 30,6,CHMESS,'Paw++ Locate','PL')
            CALL IGSET('DRMD',2.)
         ENDIF
*
      ENDIF
*.______________________________________
*
*              Dragging is on with button 2
*
      IF(DRAG2)THEN
      ENDIF
*
   70 CONTINUE
      CALL IGZSET('R')
      END
