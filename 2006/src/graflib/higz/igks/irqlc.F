*
* $Id: irqlc.F,v 1.3 1998/01/30 15:22:30 couet Exp $
*
* $Log: irqlc.F,v $
* Revision 1.3  1998/01/30 15:22:30  couet
* - APOLLO version removed
*
* Revision 1.2  1996/06/05 10:20:38  cernlib
* Move pilot.h before the ifdef for GKS
*
* Revision 1.1.1.1  1996/02/14 13:10:44  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_GKS)
*CMZ :  1.21/03 01/06/94  13.38.23  by  O.Couet
*-- Author :
      SUBROUTINE IRQLC(WKID,LCN,ISTAT,TNR,XLOC,YLOC)
*.===========>
*.
*.   This routine returns the graphic cursor position in WC space in
*. XLOC and YLOC . The same position in the NDC space is available in
*. RQUEST(11) and RQUEST(12) . The normalization tranformation number
*. is returned in TNR . After a call to IRQLC the window coordinates are
*. available in RQUEST(20), RQUEST(21), RQUEST(22), and RQUEST(23). The
*. viewport coordinates are available in RQUEST(30), RQUEST(31),
*  RQUEST(32), and RQUEST(33). This routine computed the XLOC and YLOC
*. value according to value of the REDIT factor, but the value of
*. RQUEST(11) and RQUEST(12) do not care about the value of REDIT. The
*. REDIT factor is usefull for the graphics editor and the menu mode of
*. KUIP.
*.   The values of XLOC and YLOC are computed with the parameters stored
*. in the NT storage (HINT) if ZEBRA is not used, or if there no
*. displayed picture. If an HIGZ picture is displayed, only the NT
*. existing in this picture are used to compute the values of XLOC and
*. YLOC.
*.
*. _Input parameters:
*.
*.  INTEGER WKID : Workstation identifier .
*.  INTEGER LCN  : Input device number .
*.                 If LCN>10 then LCN=10*IPET+LNDNR
*.
*. _Output parameters:
*.
*.  INTEGER ISTAT : Return status: 0=BREAK, 1=OK .
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
#if !defined(CERNLIB_NTC)
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
      INTEGER WKID,TNR
      CHARACTER*80 STR(4)
#if defined(CERNLIB_SUNGKS)
      DIMENSION IA(10),RA(10)
      CHARACTER*1 CA
#endif
*.______________________________________
*
#if defined(CERNLIB_MAIL)
*
*              MAIL option
*
      IF(MFLAG)THEN
         WRITE (CHMAIL,'(3I3)') 555,WKID,LCN
         CALL IMWRIT(1)
         READ (5,'(2I5,2E16.7)') ISTAT,TNR,XLOC,YLOC
         CALL IMWRIT(5)
         RETURN
      ENDIF
*
#endif
      CALL IGSG(0)
      LCDNR=MOD(LCN,10)
      IPET=1
      LOCDEV=1
#if !defined(CERNLIB_SUNGKS)
      LSTR=0
#endif
#if defined(CERNLIB_SUNGKS)
      IF (IPET.LE.3) THEN
         IA(1)=1
         CALL GPREC(1,IA,0,RA,0,0,CA,4,IERR,LSTR,STR)
      ELSEIF (IPET.EQ.4) THEN
         IA(1)=1
         IA(2)=0
         IA(3)=0
         IA(4)=0
         IA(5)=1
         IA(6)=1
         IA(7)=0
         RA(1)=1
         CALL GPREC(7,ID1,1,RD1,0,0,CA,4,IERR,LSTR,STR)
      ELSEIF (IPET.EQ.5) THEN
         IA(1)=0
         IA(2)=1
         IA(3)=0
         IA(4)=0
         IA(5)=0
         IA(6)=1
         IA(7)=1
         IA(8)=0
         RA(1)=1
         CALL GPREC(8,ID1,1,RD1,0,0,CA,4,IERR,LSTR,STR)
      ENDIF
#endif
#if defined(CERNLIB_ATCGKS)
      CALL GPREC(0,,0,,0,,CA,4,IERR,LSTR,STR)
#endif
#if defined(CERNLIB_DECGKS)
      CALL GINLC(WKID,LOCDEV,0,OLDXP,OLDYP,IPET
     +,          RDVXMI,RDVXMA,RDVYMI,RDVYMA,LSTR,STR)
      LCDNR=LOCDEV
#endif
#if !defined(CERNLIB_DECGKS)
      CALL GINLC(WKID,LOCDEV,0,OLDXP,OLDYP,IPET,0.,RMDSX,0.,RMDSY,
     +           LSTR,STR)
#endif
      CALL GRQLC(WKID,LCDNR,ISTAT,TNR,RQUEST(11),RQUEST(12))
      IF(ISTAT.EQ.0)THEN
         RQUEST(11)=OLDXP
         RQUEST(12)=OLDYP
         RETURN
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
