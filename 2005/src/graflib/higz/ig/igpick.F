*
* $Id: igpick.F,v 1.1.1.1 1996/02/14 13:10:37 mclareni Exp $
*
* $Log: igpick.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:37  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.23/03 01/09/95  17.03.58  by  O.Couet
*-- Author :    O.Couet   22/05/92
      SUBROUTINE IGPICK(NT,XLOC,YLOC,NBLEV,CHPID,IPID,CHOPT)
*.===========>
*.
*.  See the help in the HIGZ manual
*.
*..==========> (O.Couet)
#include "higz/hipaw.inc"
#include "higz/hicode.inc"
#include "higz/hipick.inc"
      PARAMETER (PI=3.14159265,DEGRAD=PI/180.)
      CHARACTER*(*) CHOPT
      CHARACTER*(*) CHPID(*)
      CHARACTER*80 TX_STR
      DIMENSION TX_PX(4),TX_PY(4)
      DIMENSION IPID(*)
      LOGICAL LOPICK,LOLVL
*.______________________________________
*
      IF(LPICD.LE.0)THEN
         IF(LPICT.GT.0)THEN
            LPICD = LPICT
            LDNT0 = LHNT0
            LDI   = LHI
            LDF   = LHF
            LDC   = LHC
         ELSE
            NBLEV = 0
            RETURN
         ENDIF
      ENDIF
*
*              Compute in LNDECO the bank adress of NT
*
      LN     = LQ(LPICD-1)
      LNDECO = 0
      DO 111 I=1,IQ(LPICD+8)
         IF(LN.EQ.0)GOTO 222
         IF(IQ(LN+1).EQ.NT)THEN
            LNDECO = LN
            GOTO 222
         ENDIF
         LN=LQ(LN)
  111 CONTINUE
      RETURN
  222 CONTINUE
*
      LIDECO     = LDI
      LFDECO     = LDF
      LCDECO     = LDC
*
      IAW        = IQ(LNDECO+2)
      DX         = (Q(LFDECO+IAW+1)-Q(LFDECO+IAW))/200.
      DY         = (Q(LFDECO+IAW+3)-Q(LFDECO+IAW+2))/200.
*
      IREPTR     = IQ(LNDECO+4)
      NBLEV      = 0
      ICLEV      = 999
      LOPICK     = .FALSE.
      LOLVL      = .FALSE.
      CHLVL      = ' '
      IPILVL     = 0
*
* IQUEST(60) --> Adress
* IQUEST(61) --> Type
*
      IQUEST(60) = 0
      IQUEST(61) = 0
*
*              Scan the LNDECO bank
*
    1 IREPTR=IREPTR-1
      IF(IREPTR.EQ.9)RETURN
      IADRI=MOD(IQ(LNDECO+IREPTR),1000000)
*
      GOTO (1,1,30,40,1
     +,     60,70,80,90,100,110,120,130,140,150,160,170,180,1
     +,     200,210,220,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
     +,     1,1,1
     +,     1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
     +,     1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
     +      ),INT(IQ(LNDECO+IREPTR)/1000000)
*
*              Primitive ID with NAME
*
   30 IF(.NOT.LOPICK)GOTO 1
      IF(IQ(LIDECO+IADRI).GE.ICLEV)GOTO 1
      ICLEV = IQ(LIDECO+IADRI)
      IF(ICLEV.GT.NBLEV)NBLEV=ICLEV
      IPID(ICLEV) = IQ(LIDECO+IADRI+1)
      CALL UHTOC(IQ(LCDECO+IQ(LIDECO+IADRI+2)),4,CHLVL2,LEVLEN)
      CHPID(ICLEV) = CHLVL2(1:LEN(CHPID(ICLEV)))
      IF(ICLEV.EQ.1)THEN
         IF(LOLVL)THEN
            NBLEV        = NBLEV+1
            IPID(NBLEV)  = IPILVL
            CHPID(NBLEV) = CHLVL(1:LEN(CHPID(NBLEV)))
         ENDIF
         RETURN
      ENDIF
      GOTO 1
*
*              Primitive ID without NAME
*
   40 IF(.NOT.LOPICK)GOTO 1
      IF(IQ(LIDECO+IADRI).GE.ICLEV)GOTO 1
      ICLEV = IQ(LIDECO+IADRI)
      IF(ICLEV.GT.NBLEV)NBLEV=ICLEV
      IPID(ICLEV) = IQ(LIDECO+IADRI+1)
      IF(ICLEV.EQ.1)THEN
         IF(LOLVL)THEN
            NBLEV        = NBLEV+1
            IPID(NBLEV)  = IPILVL
            CHPID(NBLEV) = CHLVL(1:LEN(CHPID(NBLEV)))
         ENDIF
         RETURN
      ENDIF
      GOTO 1
*
*              Histograms
*
   60 IF(LOPICK)GOTO 1
      IADRF  = IQ(LIDECO+IADRI)
      NBINS  = IQ(LIDECO+IADRI+1)
      ICHOPT = IQ(LIDECO+IADRI+2)
*
* Non equidistant bins
*
      IF(JBIT(ICHOPT,8).NE.0)THEN
      ELSE
*
* Equidistant bins
*
         X1    = Q(LFDECO+IADRF)
         X2    = Q(LFDECO+IADRF+1)
         XLOCH = XLOC
         YLOCH = YLOC
         IF(JBIT(ICHOPT,12).NE.0)THEN
            IF(JBIT(ICHOPT,13).NE.0)XLOCH = 10**XLOC
            IF(JBIT(ICHOPT,14).NE.0)YLOCH = 10**YLOC
         ENDIF
         IF(XLOCH.GE.X1.AND.XLOCH.LE.X2)THEN
            N      = NBINS*((XLOCH-X1)/(X2-X1))
            IF(YLOCH.LE.Q(LFDECO+IADRF+2+N))THEN
               LOLVL  = .TRUE.
               IPILVL = N+1
               CHLVL  = 'BIN'
               GOTO 500
            ENDIF
         ENDIF
      ENDIF
      GOTO 1
*
*              Polymarker with one point
*
   70 IF(LOPICK)GOTO 1
      GOTO 1
*
*              Polyline with two points
*
   80 IF(LOPICK)GOTO 1
      IP = IGPKPL(XLOC,YLOC,2,Q(LFDECO+IADRI),Q(LFDECO+IADRI+2),DX,DY)
      IF(IP.NE.0)GOTO 500
      GOTO 1
*
*              Polyline
*
   90 IF(LOPICK)GOTO 1
      IADRF = IQ(LIDECO+IADRI)
      N  = IQ(LIDECO+IADRI+1)
      IP = IGPKPL(XLOC,YLOC,N,Q(LFDECO+IADRF),Q(LFDECO+IADRF+N),DX,DY)
      IF(IP.NE.0)GOTO 500
      GOTO 1
*
*              Polymarker
*
  100 IF(LOPICK)GOTO 1
      IADRF = IQ(LIDECO+IADRI)
      N  = IQ(LIDECO+IADRI+1)
      IP = IGPKPM(XLOC,YLOC,N,Q(LFDECO+IADRF),Q(LFDECO+IADRF+N),DX,DY)
      IF(IP.NE.0)THEN
         IF(INT(IQ(LNDECO+IREPTR+1)/1000000).EQ.IMIDCO)THEN
            IADRID=MOD(IQ(LNDECO+IREPTR+1),1000000)
            ICLEV = IQ(LIDECO+IADRID)
            IF(ICLEV.GT.NBLEV)NBLEV=ICLEV
            IPID(ICLEV) = IQ(LIDECO+IADRID+IP)
            CHPID(ICLEV)='POINT'
         ENDIF
         GOTO 500
      ENDIF
      GOTO 1
*
*              Fill area
*
  110 IF(LOPICK)GOTO 1
      IADRF = IQ(LIDECO+IADRI)
      N     = IQ(LIDECO+IADRI+1)
      IP    = IGPKFA(XLOC,YLOC,N,Q(LFDECO+IADRF),Q(LFDECO+IADRF+N))
      IF(IP.NE.0)GOTO 500
      GOTO 1
*
*              Text (ITX)
*
  120 IF(LOPICK)GOTO 1
      ITAIND = 0
      TX_HGT = -99999.
      ITX_AL = -99999
      TX_WD  = 0.
  121 ITAIND = ITAIND + 1
      IF(ITAIND.EQ.9)THEN
* Could not locate all the text attributes !
         GOTO 1
      ENDIF

      IF( (INT(IQ(LNDECO+IREPTR-ITAIND)/1000000).EQ.ICHHCO)
     +    .AND.(TX_HGT.EQ.-99999.))
     +    TX_HGT = Q(LFDECO+ MOD(IQ(LNDECO+IREPTR-ITAIND),1000000))

      IF( (INT(IQ(LNDECO+IREPTR-ITAIND)/1000000).EQ.ITXACO)
     +    .AND.(ITX_AL.EQ.-99999))
     +    ITX_AL = MOD(IQ(LNDECO+IREPTR-ITAIND),100000)

      IF((ITX_AL.EQ.-99999).OR.(TX_HGT.EQ.-99999.))GOTO 121
      IADRC  = IQ(LIDECO+IADRI+1)
      ITX_CN = IQ(LIDECO+IADRI+2)
      CALL UHTOC(IQ(LCDECO+IADRC),4,TX_STR,ITX_CN)
      TX_STR = TX_STR(1:ITX_CN)
      TX_WD  = ITX_CN * TX_HGT
      XPOS   = Q(LFDECO+IQ(LIDECO+IADRI))
      YPOS   = Q(LFDECO+IQ(LIDECO+IADRI)+1)

      IF(ITX_AL.LT.20)XMIN = XPOS
      IF((ITX_AL.GE.20).AND.(ITX_AL.LT.30))XMIN = XPOS - (TX_WD/2.)
      IF(ITX_AL.GE.30)XMIN = XPOS - TX_WD
      ITX_AL = MOD(ITX_AL,10)
      IF(ITX_AL.EQ.0)YMIN = YPOS
      IF((ITX_AL.EQ.1).OR.(ITX_AL.EQ.2))YMIN = YPOS - TX_HGT
      IF(ITX_AL.EQ.3)YMIN = YPOS - (TX_HGT/2.)
      XMAX = XMIN + TX_WD
      YMAX = YMIN + TX_HGT
      I = IREPTR
  122 I = I - 1
      IF( (INT(IQ(LNDECO+I)/1000000).EQ.3).OR.(I.EQ.9))THEN
* Could not find angle attribute for a text primitive!
         GOTO 1
      ENDIF
      IF(INT(IQ(LNDECO+I)/1000000).NE.IANGCO)GOTO 122
      I        = MOD(IQ(LNDECO+I),1000000)
      TX_ANG   = Q(LFDECO+I)
      TX_PX(1) = XMIN
      TX_PY(1) = YMIN
      TX_PX(2) = XMAX
      TX_PY(2) = YMIN
      TX_PX(3) = XMAX
      TX_PY(3) = YMAX
      TX_PX(4) = XMIN
      TX_PY(4) = YMAX
      DO 123 I = 1,4
         TX_PX(I) = (-XPOS) + TX_PX(I)
         TX_PY(I) = (-YPOS) + TX_PY(I)
         XTEMP = COS(DEGRAD*TX_ANG) * TX_PX(I) + (-SIN(DEGRAD*TX_ANG))
     +           * TX_PY(I)
         YTEMP = SIN(DEGRAD*TX_ANG) * TX_PX(I) + COS(DEGRAD*TX_ANG) *
     +           TX_PY(I)
         TX_PY(I) = YTEMP
         TX_PX(I) = XTEMP
         TX_PY(I) = TX_PY(I) + YPOS
         TX_PX(I) = TX_PX(I) + XPOS
  123 CONTINUE
      IP = 0
      IP = IGPKFA(XLOC,YLOC,4,TX_PX(1),TX_PY(1))
      IF(IP.NE.0)THEN
         RQUEST(70) = TX_PX(1)
         RQUEST(71) = TX_PX(2)
         RQUEST(72) = TX_PX(3)
         RQUEST(73) = TX_PX(4)
         RQUEST(74) = TX_PY(1)
         RQUEST(75) = TX_PY(2)
         RQUEST(76) = TX_PY(3)
         RQUEST(77) = TX_PY(4)
         GOTO 500
      ENDIF
      GOTO 1
*
*              Boxe
*
  130 IF(LOPICK)GOTO 1
      IF(XLOC.GE.Q(LFDECO+IADRI)  .AND.
     +   XLOC.LE.Q(LFDECO+IADRI+1).AND.
     +   YLOC.GE.Q(LFDECO+IADRI+2).AND.
     +   YLOC.LE.Q(LFDECO+IADRI+3))GOTO 500
      GOTO 1
*
*              Frame box
*
  140 IF(LOPICK)GOTO 1
      IF( ((XLOC.GE.Q(LFDECO+IADRI))    .AND.
     +     (XLOC.LE.Q(LFDECO+IADRI+1))  .AND.
     +     (YLOC.GE.Q(LFDECO+IADRI+2))  .AND.
     +     (YLOC.LE.Q(LFDECO+IADRI+3))) .AND.

     +     ((XLOC.LE.Q(LFDECO+IADRI+4)) .OR.
     +     (XLOC.GE.Q(LFDECO+IADRI+5))  .OR.
     +     (YLOC.LE.Q(LFDECO+IADRI+6))  .OR.
     +     (YLOC.GE.Q(LFDECO+IADRI+7))) ) GOTO 500
      GOTO 1
*
*              Arc
*
  150 IF(LOPICK)GOTO 1
      SCALX  = XLOC - Q(LFDECO+IADRI)
      SCALY  = YLOC - Q(LFDECO+IADRI+1)
      PMAG   = SQRT(SCALX**2 + SCALY**2)
      ANGLOC = ACOS( SCALX / PMAG)
      ANGLOC = (180.0/PI)*ANGLOC
      IF(SCALY.LT.0.0)ANGLOC = 360. - ANGLOC

      IF((Q(LFDECO+IADRI+2).EQ.Q(LFDECO+IADRI+3)).AND.
     +(ABS(PMAG-Q(LFDECO+IADRI+2)).LT. 0.05))
     +PMAG = Q(LFDECO+IADRI+2)

      IF(((PMAG.GE.Q(LFDECO+IADRI+2)).AND.
     +    (PMAG.LE.Q(LFDECO+IADRI+3))).OR.
     +   ((PMAG.LE.Q(LFDECO+IADRI+2)).AND.
     +    (PMAG.GE.Q(LFDECO+IADRI+3))))THEN
         PHMIN = Q(LFDECO+IADRI+4)
         PHMAX = Q(LFDECO+IADRI+5)
         IF(PHMAX.GT.PHMIN)THEN
            IF( (ANGLOC.GE.PHMIN).AND.(ANGLOC.LE.PHMAX))GOTO 500
         ELSE
            IF(((ANGLOC.GE.0.).AND.(ANGLOC.LE.PHMAX)).OR.
     +         ((ANGLOC.LE.360.).AND.(ANGLOC.GT.PHMIN)))GOTO 500
         ENDIF
      ENDIF
      GOTO 1
*
*              Axis
*
  160 IF(LOPICK)GOTO 1
      IADRF=IQ(LIDECO+IADRI)
      IP=IGPKPL(XLOC,YLOC,2,Q(LFDECO+IADRF),Q(LFDECO+IADRF+2)
     +,         1.5*DX,1.5*DY)
      IF(IP.NE.0)GOTO 500
      GOTO 1
*
*              Software characters
*
  170 IF(LOPICK)GOTO 1
      IADRF=IQ(LIDECO+IADRI)
      IADRC  = IQ(LIDECO+IADRI+1)
      ITX_CN = IQ(LIDECO+IADRI+2)
      CALL UHTOC(IQ(LCDECO+IADRC),4,TX_STR,ITX_CN)
      TX_STR = TX_STR(1:ITX_CN)
      TX_HGT = Q(LFDECO+IADRF+2)
      TX_ANG = Q(LFDECO+IADRF+3)
      CALL IGTEXT(0.,0.,TX_STR,TX_HGT,TX_WD,'S')
      IGTOP = 1
      DO 171 I=1,3
         IF(JBIT(IQ(LIDECO+IADRI+3),I).NE.0)
     +   IGTOP = I
 171  CONTINUE

      IF(IGTOP.EQ.1)THEN
         ALOFF = 0.
      ELSEIF(IGTOP.EQ.2)THEN
         ALOFF = TX_WD / 2.
      ELSE
         ALOFF = TX_WD
      ENDIF
      XPOS = Q(LFDECO+IADRF)
      YPOS = Q(LFDECO+IADRF+1)
      TX_PY(1) = Q(LFDECO+IADRF+1)
      TX_PY(2) = Q(LFDECO+IADRF+1)
      TX_PY(3) = Q(LFDECO+IADRF+1)+TX_HGT
      TX_PY(4) = Q(LFDECO+IADRF+1)+TX_HGT
      TX_PX(1) = Q(LFDECO+IADRF) - ALOFF
      TX_PX(2) = Q(LFDECO+IADRF) + TX_WD - ALOFF
      TX_PX(3) = Q(LFDECO+IADRF) + TX_WD - ALOFF
      TX_PX(4) = Q(LFDECO+IADRF) - ALOFF
*
      DO 172 I = 1,4
         TX_PX(I) = (-XPOS) + TX_PX(I)
         TX_PY(I) = (-YPOS) + TX_PY(I)
         XTEMP = COS(DEGRAD*TX_ANG) * TX_PX(I) + (-SIN(DEGRAD*TX_ANG))
     +           * TX_PY(I)
         YTEMP = SIN(DEGRAD*TX_ANG) * TX_PX(I) + COS(DEGRAD*TX_ANG) *
     +           TX_PY(I)
         TX_PY(I) = YTEMP
         TX_PX(I) = XTEMP
         TX_PY(I) = TX_PY(I) + YPOS
         TX_PX(I) = TX_PX(I) + XPOS
 172  CONTINUE
      IP = 0
      IP = IGPKFA(XLOC,YLOC,4,TX_PX(1),TX_PY(1))
      IF(IP.NE.0)THEN
         RQUEST(70) = TX_PX(1)
         RQUEST(71) = TX_PX(2)
         RQUEST(72) = TX_PX(3)
         RQUEST(73) = TX_PX(4)
         RQUEST(74) = TX_PY(1)
         RQUEST(75) = TX_PY(2)
         RQUEST(76) = TX_PY(3)
         RQUEST(77) = TX_PY(4)
         GOTO 500
      ENDIF
      GOTO 1
*
*              Multiline
*
  180 IF(LOPICK)GOTO 1
      GOTO 1
*
*              Table
*
  200 IF(LOPICK)GOTO 1
      IF(XLOC.GE.Q(LFDECO+IAW)  .AND.
     +   XLOC.LE.Q(LFDECO+IAW+1).AND.
     +   YLOC.GE.Q(LFDECO+IAW+2).AND.
     +   YLOC.LE.Q(LFDECO+IAW+3))GOTO 500
      GOTO 1
*
*              Graph
*
  210 IF(LOPICK)GOTO 1
      IADRF  = IQ(LIDECO+IADRI)
      N      = IQ(LIDECO+IADRI+1)
      ICHOPT = IQ(LIDECO+IADRI+2)
      IF(JBIT(ICHOPT,12).NE.0)THEN
         IP = IGPKFA(XLOC,YLOC,N,Q(LFDECO+IADRF),Q(LFDECO+IADRF+N))
      ELSE
         IP = IGPKPL(XLOC,YLOC,N,Q(LFDECO+IADRF),Q(LFDECO+IADRF+N)
     +,              DX,DY)
      ENDIF
      IF(IP.NE.0)GOTO 500
      GOTO 1
*
*              Pave
*
  220 IF(LOPICK)GOTO 1
      IADRF = IQ(LIDECO+IADRI)
      IF(XLOC.GE.Q(LFDECO+IADRF)  .AND.
     +   XLOC.LE.Q(LFDECO+IADRF+1).AND.
     +   YLOC.GE.Q(LFDECO+IADRF+2).AND.
     +   YLOC.LE.Q(LFDECO+IADRF+3))GOTO 500
      GOTO 1
*
  500 LOPICK     = .TRUE.
      IQUEST(60) = IREPTR
      IQUEST(61) = INT(IQ(LNDECO+IREPTR)/1000000)
      IQUEST(62) = IADRI
      IQUEST(63) = IADRF
      IQUEST(64) = NBLEV
      RQUEST(80) = XLOC
      RQUEST(81) = YLOC
      GOTO 1
*
      END
