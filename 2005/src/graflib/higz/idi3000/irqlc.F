*
* $Id: irqlc.F,v 1.2 1998/01/30 15:22:11 couet Exp $
*
* $Log: irqlc.F,v $
* Revision 1.2  1998/01/30 15:22:11  couet
* - APOLLO version removed
*
* Revision 1.1.1.1  1996/02/14 13:10:29  mclareni
* Higz
*
*
#if defined(CERNLIB_DI3000)
#include "higz/pilot.h"
*CMZ :  1.21/03 01/06/94  13.38.23  by  O.Couet
*-- Author :
      SUBROUTINE IRQLC(WKID,LCN,STATUS,TNR,XLOC,YLOC)
*.===========>
*.
*.   This routine returns the graphic cursor position in WC space in
*. XLOC and YLOC . The same position in the NDC space is available in
*. RQUEST(11) and RQUEST(12) . The normalization tranformation number
*. is returned in TNR . After a call to IRQLC the window coordinates
*. are available in RQUEST(20), RQUEST(21), RQUEST(22), and RQUEST(23).
*. The viewport coordinates are available in RQUEST(30), RQUEST(31),
*. RQUEST(32), and RQUEST(33). This routine computed the XLOC and YLOC
*. value according to value of the REDIT factor, but the value of
*. RQUEST(11) and RQUEST(12) do not care about the value of REDIT.
*. The REDIT factor is usefull for the graphics editor and the menu
*. mode of KUIP. The values of XLOC and YLOC are computed with the
*. parameters stored in the NT storage (HINT) if ZEBRA is not used, or
*. if there no displayed picture. If an HIGZ picture is displayed,
*. only the NT existing in this picture are used to compute the values
*. of XLOC and YLOC.
*.
*. _Input parameters:
*.
*.  INTEGER WKID : Workstation identifier .
*.  INTEGER LCN  : Input device number .
*.                 If LCN>10 then LCN=10*IPET+LNDNR
*.  INTEGER STATUS : Return status: 0=BREAK, 1=OK .
*.
*. _Output parameters:
*.
*.  INTEGER TNR : Normalization transformation number .
*.  REAL XLOC YLOC : Cursor position in WC space .
*.
*..==========> (O.Couet)
#if defined(CERNLIB_ZEBRA)
#include "higz/hipaw.inc"
#endif
#if !defined(CERNLIB_ZEBRA)
#include "higz/hiques.inc"
#endif
#include "higz/hint.inc"
#include "higz/hiloc.inc"
#include "higz/hiatt.inc"
#if defined(CERNLIB_MAIL)
#include "higz/himail.inc"
#endif
#if defined(CERNLIB_MAIL)
#include "higz/hiflag.inc"
#endif
      INTEGER WKID,LCDNR,STATUS,TNR,NNT
      INTEGER INPFCT,IECHO,IBUTTN
      CHARACTER*80 STR(4)
*.______________________________________
*
#if defined(CERNLIB_MAIL)
*
*              MAIL option
*
      IF(MFLAG)THEN
         WRITE (CHMAIL,'(3I3)') 555,WKID,LCN
         CALL IMWRIT(1)
         READ (5,'(2I5,2E16.7)') STATUS,TNR,XLOC,YLOC
         CALL IMWRIT(5)
         RETURN
      ENDIF
#endif
*
      CALL IGSG(0)
      LCDNR=MOD(LCN,10)
      IPET=1
      LOCDEV=1
      INPFCT=2
      IECHO=1
      LSTR=0
      CALL JPECHO(WKID,INPFCT,LOCDEV,OLDXP,OLDYP)
      CALL JIENAB(WKID,INPFCT,LOCDEV)
      CALL JLOCAT(WKID,LOCDEV,IECHO,IBUTTN,RQUEST(11),RQUEST(12))
      IF(IBUTTN.EQ.3) THEN
         STATUS = 0
      ELSE
         STATUS = 1
      ENDIF
      OLDXP=RQUEST(11)
      OLDYP=RQUEST(12)
*
      SCALE=1.
      IF(REDIT.NE.0.)SCALE=REDIT
*
      RQUEST(20)=0.
      RQUEST(21)=1.
      RQUEST(22)=0.
      RQUEST(23)=1.
      RQUEST(30)=0.
      RQUEST(31)=1.
      RQUEST(32)=0.
      RQUEST(33)=1.
      TNR=0
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
         DO 10 I=1,NBNT
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
                  TNR=IQ(LN+1)
                  IPRIOO=IPRION
               ENDIF
            ENDIF
            LN=LQ(LN)
            IF(LN.EQ.0)RETURN
   10    CONTINUE
         RETURN
      ENDIF
#endif
#if !defined(CERNLIB_NTC)
*
*             The computing of XLOC and YLOC in done with the parameters
*             stored in HINT.
*
      NBNT=NTSTOR(1)
      IPRIOO=NT0PRI
      DO 20 I=1,NBNT
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
               TNR=NNT
               IPRIOO=IPRION
            ENDIF
         ENDIF
   20 CONTINUE
#endif
#if defined(CERNLIB_NTC)
      IPRIOO = NT0PRI
      IEND   = IGNNXT(1)
   30 IF(IEND.EQ.0)THEN
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
         GOTO 30
      ENDIF
#endif
*
      END
#endif
