*
* $Id: irqlc.F,v 1.4 1998/12/01 15:48:36 couet Exp $
*
* $Log: irqlc.F,v $
* Revision 1.4  1998/12/01 15:48:36  couet
* - Clean up: commented lines of code removed
*
* Revision 1.3  1998/01/28 14:34:47  couet
* - GGDM driver removed
*
* Revision 1.2  1996/09/25 14:58:50  couet
* - GPR driver removed
*
* Revision 1.1.1.1  1996/02/14 13:10:56  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.23/01 08/05/95  11.47.40  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IRQLC(IWKIDI,LCN,ISTATI,ITNRI,XLOC,YLOC)
      ENTRY      GRQLC(IWKIDI,LCN,ISTATI,ITNRI,XLOC,YLOC)
*.===========>
*.
*.   This routine returns the graphic cursor position in WC space in
*. XLOC and YLOC. The same position in the NDC space is available in
*. RQUEST(11) and RQUEST(12). The normalization tranformation number
*. is returned in TNR. After a call to IRQLC the window coordinates
*. are available in RQUEST(20), RQUEST(21), RQUEST(22), and RQUEST(23).
*. The viewport coordinates are available in RQUEST(30), RQUEST(31),
*. RQUEST(32), and  RQUEST(33). This routine computed the XLOC and YLOC
*. value according to value of the REDIT factor, but the value of
*. RQUEST(11) and RQUEST(12) do not care about the value of REDIT. The
*. REDIT factor is usefull for the graphics editor and the menu mode of
*. KUIP.
*.   The values of XLOC and YLOC are computed with the parameters stored
*. in the NT storage (HINT) if ZEBRA is not used, or if there no
*. displayed picture. If an HIGZ picture is displayed, only the NT
*. existing in this picture are used to compute the values of XLOC and
*. YLOC.
*.   This routine can work in two modes :
*.    - The REQUEST mode where a "Button_Press" event is necesary to get
*.      the cursor position.
*.    - The SAMPLE mode where only a move of the mouse is enough to get
*.      the cursor position.
*.
*. _Input parameters:
*.
*.  INTEGER IWKIDI : Workstation identifier .
*.  INTEGER LCN    : Input device number .
*.                   If LCN>10 then LCN=10*IPET+LNDNR
*.
*.                 LCN=10 tracking cross
*.                 LCN=20 cross-hair
*.                 LCN=30 rubber circle
*.                 LCN=40 rubber band
*.                 LCN=50 rubber rectangle
*.                 LCN=99 the screen coordinates are taken
*.                        in XLOC and YLOC (X11 only).
*.
*.                 LCN>0  request mode
*.                 LCN<0  sample mode
*.
*. _Output parameters:
*.
*.  INTEGER ISTATI : Return status: 0=BREAK, 1=OK .
*.  INTEGER ITNRI  : Normalization transformation number .
*.  REAL XLOC YLOC : Cursor position in WC space .
*.
*..==========> (O.Couet)
#if defined(CERNLIB_KERNEL)
#include "higz/hikern.inc"
#endif
#if defined(CERNLIB_ZEBRA)
#include "higz/hipaw.inc"
#endif
#if !defined(CERNLIB_ZEBRA)
#include "higz/hiques.inc"
#endif
#if (!defined(CERNLIB_NTC))&&(!defined(CERNLIB_X11))
#include "higz/hint.inc"
#endif
#include "higz/hiloc.inc"
#include "higz/hiatt.inc"
#if defined(CERNLIB_MAIL)
#include "higz/himail.inc"
#endif
#if defined(CERNLIB_MAIL)
#include "higz/hiflag.inc"
#endif
      LOGICAL SAMPLE
#if defined(CERNLIB_GL)
      SAVE IXLOCP,IYLOCP
#endif
*.______________________________________
*
      IWKIND    = IGIWIN(IWKIDI)
      IQUEST(1) = 0
      IF(IWKIND.EQ.0)THEN
         CALL IGERR('Workstation is not open','IRQLC')
         RETURN
      ENDIF
      IF(IWTYL(IWKIND).LE.0)THEN
* No locator for Metafile.
         CALL IGERR('No request locator on metafile','IRQLC')
         RETURN
      ENDIF
#if defined(CERNLIB_MAIL)
*
      IF(MFLAG)THEN
         WRITE (CHMAIL,'(3I3)') 555,IWKIDI,LCN
         CALL IMWRIT(1)
         READ (5,'(2I5,2E16.7)') ISTATI,ITNRI,XLOC,YLOC
         CALL IMWRIT(5)
         RETURN
      ENDIF
#endif
*
      IF (LCN.GE.0) THEN
         SAMPLE = .FALSE.
      ELSE
         SAMPLE = .TRUE.
      ENDIF
      IPET = 1
      IF(ABS(LCN).GE.10)THEN
         IPET = ABS(LCN)/10
      ENDIF
#if defined(CERNLIB_MSDOS)
      ISAVE = IWKNB
      IWKNB = IWKIND
#endif
#if defined(CERNLIB_FALCO)
*
      IF(IWTYL(IWKIND).EQ.7878.OR.IWTYL(IWKIND).EQ.7879)THEN
* Here we use space as affirmative reply and any other char as NO
         IF (ISTATI.EQ.ICHAR(' ')) THEN
            ISTATI=1
         ELSE
            ISTATI=0
         ENDIF
         GOTO 90
      ENDIF
#endif
#if (defined(CERNLIB_GL))&&(!defined(CERNLIB_MSDOS))
*
      CALL ISCLIP(0)
      CALL QDEVIC(LEFTMO)
      CALL QDEVIC(RIGHTM)
      CALL QDEVIC(MOUSEX)
      CALL QDEVIC(MOUSEY)
      CALL LOGICO(LOXOR)
      CALL GETORI(IXORI,IYORI)
      CALL COLOR(1)
      CALL QRESET
      IXLOCP=IXLOCO
      IYLOCP=IYLOCO
   40 CONTINUE
      CALL WINSET(IWINID(IWKIND))
      CALL CURSOF
      IF(LOINS)THEN
         IF(IPET.EQ.1)THEN
            CALL MOVE2I(IXLOC,IYLOC-10)
            CALL DRAW2I(IXLOC,IYLOC+10)
            CALL MOVE2I(IXLOC-10,IYLOC)
            CALL DRAW2I(IXLOC+10,IYLOC)
         ENDIF
         IF(IPET.EQ.2)THEN
            CALL MOVE2I(IXLOC,0)
            CALL DRAW2I(IXLOC,INT(YWKSIZ(IWKIND)))
            CALL MOVE2I(0,IYLOC)
            CALL DRAW2I(INT(XWKSIZ(IWKIND)),IYLOC)
         ENDIF
         IF(IPET.EQ.3)THEN
            RX=FLOAT(IXLOC-IXLOCP)
            RY=FLOAT(IYLOC-IYLOCP)
            IRAD=INT(SQRT(RX*RX+RY*RY))
            CALL CIRCI(IXLOCP,IYLOCP,IRAD)
         ENDIF
         IF(IPET.EQ.4.OR.IPET.EQ.3)THEN
            CALL MOVE2I(IXLOCP,IYLOCP)
            CALL DRAW2I(IXLOC,IYLOC)
         ENDIF
         IF(IPET.EQ.5)THEN
            CALL SBOXI(IXLOCP,IYLOCP,IXLOC,IYLOC)
         ENDIF
      ENDIF
      IXLOCO=IXLOC
      IYLOCO=IYLOC
      IDEV=QREAD(IVAL)
      IF(IDEV.EQ.MOUSEX.OR.IDEV.EQ.MOUSEY.OR.LOINS)THEN
         IF(IDEV.EQ.MOUSEX)THEN
            IXLOC=IVAL-IXORI
            RQUEST(11)=FLOAT(IXLOC)/WKMAX(IWKIND)
            GOTO 50
         ENDIF
         IF(IDEV.EQ.MOUSEY)THEN
            IYLOC=IVAL-IYORI
            RQUEST(12)=FLOAT(IYLOC)/WKMAX(IWKIND)
            GOTO 50
         ENDIF
   50    IF(LOINS)THEN
            IF(IPET.EQ.1)THEN
               CALL MOVE2I(IXLOCO,IYLOCO-10)
               CALL DRAW2I(IXLOCO,IYLOCO+10)
               CALL MOVE2I(IXLOCO-10,IYLOCO)
               CALL DRAW2I(IXLOCO+10,IYLOCO)
            ENDIF
            IF(IPET.EQ.2)THEN
               CALL MOVE2I(IXLOCO,0)
               CALL DRAW2I(IXLOCO,INT(YWKSIZ(IWKIND)))
               CALL MOVE2I(0,IYLOCO)
               CALL DRAW2I(INT(XWKSIZ(IWKIND)),IYLOCO)
            ENDIF
            IF(IPET.EQ.3)THEN
               RX=FLOAT(IXLOCO-IXLOCP)
               RY=FLOAT(IYLOCO-IYLOCP)
               IRAD=INT(SQRT(RX*RX+RY*RY))
               CALL CIRCI(IXLOCP,IYLOCP,IRAD)
            ENDIF
            IF(IPET.EQ.4.OR.IPET.EQ.3)THEN
               CALL MOVE2I(IXLOCP,IYLOCP)
               CALL DRAW2I(IXLOCO,IYLOCO)
            ENDIF
            IF(IPET.EQ.5)THEN
               CALL SBOXI(IXLOCP,IYLOCP,IXLOCO,IYLOCO)
            ENDIF
         ENDIF
         LOINS=.TRUE.
         GOTO 60
      ENDIF
   60 IF(IDEV.EQ.LEFTMO)THEN
         IDEV=QREAD(IVAL)
         ISTATI=1
         GOTO 70
      ENDIF
      IF(IDEV.EQ.RIGHTM)THEN
         IDEV=QREAD(IVAL)
         ISTATI=0
         GOTO 70
      ENDIF
      IF(IDEV.EQ.INPTCH)THEN
         LOINS=.NOT.LOINS
         GOTO 40
      ENDIF
      GOTO 40
   70 CALL UNQDEV(LEFTMO)
      CALL UNQDEV(RIGHTM)
      CALL UNQDEV(MOUSEX)
      CALL UNQDEV(MOUSEY)
      CALL LOGICO(LOSRC)
      CALL COLOR(IPLCI+IOFCOL)
      CALL CURSON
      LOINS=.FALSE.
#endif
#if defined(CERNLIB_MSDOS)
      RQUEST(11)=OLDXP
      RQUEST(12)=OLDYP
*
      IXLOCO = NINT(RQUEST(11)*WKMAX(IWKIND))
      IYLOCO = NINT(YWKSIZ(IWKIND)-RQUEST(12)*WKMAX(IWKIND))
#endif
#if (defined(CERNLIB_MSDOS))&&(defined(CERNLIB_NDP))
*
      CALL ISCLIP(0)
      IER = SET_XOR(1)
      CALL COLOR(6)
      IF (MOUSE) CALL SET_MOUSE(IXLOCO,IYLOCO)
      IXLOCP=IXLOCO
      IYLOCP=IYLOCO
   40 CONTINUE
      CALL WINSET(IWINID(IWKIND))
      CALL CURSOF
      IF(LOINS)THEN
         IF(IPET.EQ.1)THEN
            CALL MOVE2I(IXLOC,IYLOC-10)
            CALL DRAW2I(IXLOC,IYLOC+10)
            CALL MOVE2I(IXLOC-10,IYLOC)
            CALL DRAW2I(IXLOC+10,IYLOC)
         ENDIF
         IF(IPET.EQ.2)THEN
            CALL MOVE2I(IXLOC,0)
            CALL DRAW2I(IXLOC,INT(YWKSIZ(IWKIND)))
            CALL MOVE2I(0,IYLOC)
            CALL DRAW2I(INT(XWKSIZ(IWKIND)),IYLOC)
         ENDIF
         IF(IPET.EQ.3)THEN
            RX=FLOAT(IXLOC-IXLOCP)
            RY=FLOAT(IYLOC-IYLOCP)
            IRAD=NINT(SQRT(RX*RX+RY*RY))
            CALL CIRCI(IXLOCP,IYLOCP,IRAD)
         ENDIF
         IF(IPET.EQ.4.OR.IPET.EQ.3)THEN
            CALL MOVE2I(IXLOCP,IYLOCP)
            CALL DRAW2I(IXLOC,IYLOC)
         ENDIF
         IF(IPET.EQ.5)THEN
            CALL SBOXI(IXLOCP,IYLOCP,IXLOC,IYLOC)
         ENDIF
      ENDIF
      IXLOCO=IXLOC
      IYLOCO=IYLOC
      ISTATI=ICRAWL(IXLOC,IYLOC)
      IF(ISTATI.GT. 0 .OR. LOINS)THEN
         IF(ISTATI .EQ. 10 .or. ISTATI .EQ. 11)THEN
            RQUEST(11)=FLOAT(IXLOC)/WKMAX(IWKIND)
         ENDIF
         IF(ISTATI .EQ. 1 .or. ISTATI .EQ. 11)THEN
            RQUEST(12)=(YWKSIZ(IWKIND)-FLOAT(IYLOC))/WKMAX(IWKIND)
         ENDIF
   50    IF(LOINS)THEN
            IF(IPET.EQ.1)THEN
               CALL MOVE2I(IXLOCO,IYLOCO-10)
               CALL DRAW2I(IXLOCO,IYLOCO+10)
               CALL MOVE2I(IXLOCO-10,IYLOCO)
               CALL DRAW2I(IXLOCO+10,IYLOCO)
            ENDIF
            IF(IPET.EQ.2)THEN
               CALL MOVE2I(IXLOCO,0)
               CALL DRAW2I(IXLOCO,INT(YWKSIZ(IWKIND)))
               CALL MOVE2I(0,IYLOCO)
               CALL DRAW2I(INT(XWKSIZ(IWKIND)),IYLOCO)
            ENDIF
            IF(IPET.EQ.3)THEN
               RX=FLOAT(IXLOCO-IXLOCP)
               RY=FLOAT(IYLOCO-IYLOCP)
               IRAD=NINT(SQRT(RX*RX+RY*RY))
               CALL CIRCI(IXLOCP,IYLOCP,IRAD)
            ENDIF
            IF(IPET.EQ.4.OR.IPET.EQ.3)THEN
               CALL MOVE2I(IXLOCP,IYLOCP)
               CALL DRAW2I(IXLOCO,IYLOCO)
            ENDIF
            IF(IPET.EQ.5)THEN
               CALL SBOXI(IXLOCP,IYLOCP,IXLOCO,IYLOCO)
            ENDIF
         ENDIF
         LOINS=.TRUE.
         GOTO 60
      ENDIF
   60 IF(ISTATI .EQ. 0)THEN
         ISTATI=1
         GOTO 70
      ENDIF
      IF(ISTATI .EQ. -1)THEN
         ISTATI=0
         GOTO 70
      ENDIF
      IF (.NOT. SAMPLE) GOTO 40
      ISTATI = 1
   70 CONTINUE
      IER =  SET_XOR(0)
      CALL COLOR(IPLCI+IOFCOL)
      LOINS=.FALSE.
      IWKNB = ISAVE
#endif
#if (defined(CERNLIB_MSDOS))&&(!defined(CERNLIB_NDP))
      IIX= IXLOCO
      IIY= IYLOCO
#endif
#if (defined(CERNLIB_X11)||defined(CERNLIB_MSDOS))&&(!defined(CERNLIB_NDP))
*
      IF(LCN.EQ.99)THEN
         IIX=INT(XLOC)
         IIY=INT(YLOC)
         ISTATI=99
         GOTO 80
      ENDIF
      CALL IXSELWI(IWINID(IWKIND))
      IF(SAMPLE)THEN
         ISTATI=IXREQLO(1,IPET,IIX,IIY)
      ELSE
         ISTATI=IXREQLO(0,IPET,IIX,IIY)
      ENDIF
      IF(ISTATI.EQ.3)ISTATI=0
   80 RQUEST(11)=FLOAT(IIX)/WKMAX(IWKIND)
      RQUEST(12)=(YWKSIZ(IWKIND)-FLOAT(IIY))/WKMAX(IWKIND)
#endif
#if defined(CERNLIB_MACMPW)
*
      IF(SAMPLE)THEN
         ISTATI=IMREQLO(1,IPET,IIX,IIY)
      ELSE
         ISTATI=IMREQLO(0,IPET,IIX,IIY)
      ENDIF
      IF(ISTATI.EQ.3)ISTATI=0
      RQUEST(11)=FLOAT(IIX)/WKMAX(IWKIND)
      RQUEST(12)=(YWKSIZ(IWKIND)-FLOAT(IIY))/WKMAX(IWKIND)
#endif
*
   90 IF(ISTATI.EQ.0.AND..NOT.SAMPLE)THEN
         RQUEST(11)=OLDXP
         RQUEST(12)=OLDYP
         RETURN
      ENDIF
      OLDXP=RQUEST(11)
      OLDYP=RQUEST(12)
      SCALE=1.
      IF(REDIT.NE.0.)SCALE=REDIT
      RQUEST(20)=0.
      RQUEST(21)=1.
      RQUEST(22)=0.
      RQUEST(23)=1.
      RQUEST(30)=0.
      RQUEST(31)=1.
      RQUEST(32)=0.
      RQUEST(33)=1.
      ITNRI=0
      XLOC=(RQUEST(11)+RDWXMI*REDIT)/SCALE
      YLOC=(RQUEST(12)+RDWYMI*REDIT)/SCALE
#if defined(CERNLIB_ZEBRA)
*
*             There is displayed picture
*
      IF(LPICD.GT.0)THEN
         IPRIOO=IQ(LDNT0+3)
         NBNT=IQ(LPICD+8)-1
         LN=LQ(LDNT0)
         DO 100 I=1,NBNT
            IAWV=LDF+IQ(LN+2)
            IPRION=IQ(LN+3)
            RQUEST(20)=Q(IAWV)
            RQUEST(21)=Q(IAWV+1)
            RQUEST(22)=Q(IAWV+2)
            RQUEST(23)=Q(IAWV+3)
            RQUEST(30)=Q(IAWV+4)
            RQUEST(31)=Q(IAWV+5)
            RQUEST(32)=Q(IAWV+6)
            RQUEST(33)=Q(IAWV+7)
            IF((RQUEST(30)*SCALE-(RDWXMI*REDIT).LE.RQUEST(11)).AND.
     +         (RQUEST(11).LE.RQUEST(31)*SCALE-(RDWXMI*REDIT)).AND.
     +         (RQUEST(32)*SCALE-(RDWYMI*REDIT).LE.RQUEST(12)).AND.
     +         (RQUEST(12).LE.RQUEST(33)*SCALE-(RDWYMI*REDIT)))THEN
               IF(IPRION.GT.IPRIOO)THEN
                  XLOC=((((RQUEST(11)+(RDWXMI*REDIT))/SCALE)
     +                 -RQUEST(30))
     +                 /(RQUEST(31)-RQUEST(30)))
     +                 *(RQUEST(21)-RQUEST(20))
     +                +RQUEST(20)
                  YLOC=((((RQUEST(12)+(RDWYMI*REDIT))/SCALE)
     +                 -RQUEST(32))
     +                 /(RQUEST(33)-RQUEST(32)))
     +                 *(RQUEST(23)-RQUEST(22))
     +                 +RQUEST(22)
                  ITNRI=IQ(LN+1)
                  IPRIOO=IPRION
               ENDIF
            ENDIF
            LN=LQ(LN)
            IF(LN.EQ.0)RETURN
  100    CONTINUE
         RETURN
      ENDIF
#endif
#if (!defined(CERNLIB_NTC))&&(!defined(CERNLIB_X11))
*
*             The computing of XLOC and YLOC in done with the parameters
*             stored in HINT.
*
      NBNT=NTSTOR(1)
      IPRIOO=NT0PRI
      DO 110 I=1,NBNT
         NNT=NTSTOR(2*I)
         CALL IZGNT(NNT,IFIND,IAWV,IPRION,IHPRIO)
         RQUEST(20)=WNVPST(IAWV)
         RQUEST(21)=WNVPST(IAWV+1)
         RQUEST(22)=WNVPST(IAWV+2)
         RQUEST(23)=WNVPST(IAWV+3)
         RQUEST(30)=WNVPST(IAWV+4)
         RQUEST(31)=WNVPST(IAWV+5)
         RQUEST(32)=WNVPST(IAWV+6)
         RQUEST(33)=WNVPST(IAWV+7)
         IF((RQUEST(30)*SCALE-(RDWXMI*REDIT).LE.RQUEST(11)).AND.
     +      (RQUEST(11).LE.RQUEST(31)*SCALE-(RDWXMI*REDIT)).AND.
     +      (RQUEST(32)*SCALE-(RDWYMI*REDIT).LE.RQUEST(12)).AND.
     +      (RQUEST(12).LE.RQUEST(33)*SCALE-(RDWYMI*REDIT)))THEN
            IF(IPRION.GT.IPRIOO)THEN
               XLOC=((((RQUEST(11)+(RDWXMI*REDIT))/SCALE)
     +              -RQUEST(30))
     +              /(RQUEST(31)-RQUEST(30)))
     +              *(RQUEST(21)-RQUEST(20))
     +             +RQUEST(20)
               YLOC=((((RQUEST(12)+(RDWYMI*REDIT))/SCALE)
     +              -RQUEST(32))
     +              /(RQUEST(33)-RQUEST(32)))
     +              *(RQUEST(23)-RQUEST(22))
     +              +RQUEST(22)
               ITNRI=NNT
               IPRIOO=IPRION
            ENDIF
         ENDIF
  110 CONTINUE
#endif
#if defined(CERNLIB_NTC)||defined(CERNLIB_X11)
      IPRIOO = NT0PRI
      IEND   = IGNNXT(1)
  120 IF(IEND.EQ.0)THEN
         CALL IGNGET(NNT,IPRION,IHPRIO
     +,              RQUEST(20),RQUEST(21),RQUEST(22),RQUEST(23)
     +,              RQUEST(30),RQUEST(31),RQUEST(32),RQUEST(33),I3D)
         IF((RQUEST(30)*SCALE-(RDWXMI*REDIT).LE.RQUEST(11)).AND.
     +      (RQUEST(11).LE.RQUEST(31)*SCALE-(RDWXMI*REDIT)).AND.
     +      (RQUEST(32)*SCALE-(RDWYMI*REDIT).LE.RQUEST(12)).AND.
     +      (RQUEST(12).LE.RQUEST(33)*SCALE-(RDWYMI*REDIT)))THEN
            IF(IPRION.GT.IPRIOO)THEN
               XLOC=((((RQUEST(11)+(RDWXMI*REDIT))/SCALE)
     +              -RQUEST(30))
     +              /(RQUEST(31)-RQUEST(30)))
     +              *(RQUEST(21)-RQUEST(20))
     +             +RQUEST(20)
               YLOC=((((RQUEST(12)+(RDWYMI*REDIT))/SCALE)
     +              -RQUEST(32))
     +              /(RQUEST(33)-RQUEST(32)))
     +              *(RQUEST(23)-RQUEST(22))
     +              +RQUEST(22)
               ITNRI=NNT
               IPRIOO=IPRION
            ENDIF
         ENDIF
         IEND = IGNNXT(0)
         GOTO 120
      ENDIF
#endif
*
      END
