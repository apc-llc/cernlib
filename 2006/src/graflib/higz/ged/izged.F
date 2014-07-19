*
* $Id: izged.F,v 1.1.1.1 1996/02/14 13:10:24 mclareni Exp $
*
* $Log: izged.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:24  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.12/08 28/03/91  12.13.41  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IZGED(PNAMEI,CHOPT)
*.===========>
*.
*.   This routine invokes the graphics editor .
*.
*. CHOPT='S': Software character for the menus.
*. CHOPT='A': shAdow mode is used.
*.
*..==========> (O.Couet)
#include "higz/hipaw.inc"
#include "higz/hiatt.inc"
#include "higz/hiflag.inc"
#include "higz/higed.inc"
#include "higz/hiatnb.inc"
#include "higz/hisved.inc"
#include "higz/hicode.inc"
      CHARACTER*(*) PNAMEI,CHOPT
      CHARACTER*32 CHVAL(15)
      CHARACTER*20 MERPIC,PRIPIC,SHOPIC,REAPIC,PNAME
      CHARACTER*12 CINTR
      DIMENSION IOPT(2)
      EQUIVALENCE (IOPT(1),IOPTS),(IOPT(2),IOPTA)
      LOGICAL ZFS
      DIMENSION RATVAL(NBRATT)
      EQUIVALENCE (RATVAL(1),RMKSC)
      DIMENSION IATVAL(NBIATT)
      EQUIVALENCE (IATVAL(1),IPLCI)
#include "higz/hiatnm.inc"
*.______________________________________
*
      CALL UOPTC(CHOPT,'SA',IOPT)
      PNAME=PNAMEI
*
*              Adressing pictures by number
*
      INUM=IGASCI(PNAME(1:1))
      IF(49.LE.INUM.AND.INUM.LE.57)THEN
         READ (PNAME,'(I15)',ERR=20   ) INUM
         IF(INUM.GT.NBPICT)THEN
            CALL IGERR('Number of pictures is not so big','IZGED')
            RETURN
         ENDIF
         LP=LQ(LHIGZ)
         DO 10     I=1,INUM-1
            LP=LQ(LP)
   10    CONTINUE
         LCH=LQ(LP-4)
         CALL UHTOC(IQ(LCH+1),4,PNAME,IQ(LP+1))
         GOTO 30
   20    CALL IGERR('PNAME must begin with a letter','IZGED')
         RETURN
      ENDIF
   30 CONTINUE
*
      DO 40 I=1,NBIATT
         ISAVED(I)=IATVAL(I)
   40 CONTINUE
      DO 50 I=1,NBRATT
         RSAVED(I)=RATVAL(I)
   50 CONTINUE
*
*              Encoding ISOF
*
      ISOF=0
      IF(IOPTS.NE.0)CALL SBIT1(ISOF,1)
      IF(IOPTA.NE.0)CALL SBIT1(ISOF,2)
*
      WX1S=RDWXMI
      WX2S=RDWXMA
      WY1S=RDWYMI
      WY2S=RDWYMA
      VX1S=RDVXMI
      VX2S=RDVXMA
      VY1S=RDVYMI
      VY2S=RDVYMA
      IF(RMDSX.GE.RMDSY)THEN
         CALL ISWKWN(IDID,0.,1.,0.,RMDSY/RMDSX)
      ELSE
         CALL ISWKWN(IDID,0.,RMDSX/RMDSY,0.,1.)
      ENDIF
      CALL ISWKVP(IDID,0.,RMDSX,0.,RMDSY)
*
      ZFLAG=.FALSE.
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
      GFLAG=.TRUE.
      REDITS=REDIT
      CALL IGSRAP(0.)
      CALL ICLRWK(1,1)
      ZFLAG=.TRUE.
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
      CALL IGSRAP(4./5.)
      CALL IGCLES
*
*              Check the validity of the picture name
*
      IF(IZRPIP(PNAME).GT.0)THEN
         CALL IZPICT(PNAME,'DC')
         EDIPIC=PNAME
      ELSE
         IF(PNAME.NE.' ')THEN
            CALL IZPICT(PNAME,'M')
            EDIPIC=PNAME
         ELSE
            CALL IZPICT(EDIPIC,'R')
            IF(EDIPIC.NE.' ')THEN
               CALL IZPICT(EDIPIC,'DC')
            ELSE
               EDIPIC='PICTGED'
               CALL IZPICT(EDIPIC,'M')
               CALL IZPICT(EDIPIC,'C')
            ENDIF
         ENDIF
      ENDIF
*
*              Initialization
*
      CALL IGINME
      ZFLAG=.TRUE.
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
*
*              Put the UNDO vode in all the NT banks
*              to protect the initial contains of the
*              picture
*
      LNT=LQ(LPICT-1)
      DO 60 I=1,IQ(LPICT+8)
         IF(LNT.EQ.0)GOTO 70
         IQ(LNT+IQ(LNT+4))=1000000*IUNDCO
         IQ(LNT+4)=IQ(LNT+4)+1
         IF(LNT.EQ.LHNT)ITBPTR=ITBPTR+1
         LNT=LQ(LNT)
   60 CONTINUE
*
*              Select the NT with the higher priority
*
   70 IHNTR=IQ(IZGNGT(LPICT,IZGHNP(LPICT)-1)+1)
      CALL ISELNT(IHNTR)
*.______________________________________
*
*              Start with the GED menu mode
*
   80 TITLE(1)='MAIN MENU'
      CHITEM(1,1)='|Primitives'
      CHITEM(2,1)='|Windows'
      CHITEM(3,1)='|Pictures'
      CHITEM(4,1)='|Files'
      CHUSKE(1,1)='Exit'
      IPLACE=1
      CALL IGREQ(11,1,4,IPLACE,ICHOIC,CHVAL)
*
   90 CONTINUE
*
      IF(ICHOIC.EQ.-1)GOTO 270
*
      IF(ICHOIC.EQ.-1000)GOTO 80
*
      IF(ICHOIC.EQ.1)GOTO 100
*
      IF(ICHOIC.EQ.2)GOTO 140
*
      IF(ICHOIC.EQ.3)GOTO 220
*
      IF(ICHOIC.EQ.4)GOTO 260
*
      GOTO 80
*.______________________________________
*
*              Primitives Menu
*
  100 TITLE(2)='PRIMITIVES'
      CHITEM(1,2)='Software text'
      CHITEM(2,2)='Text'
      CHITEM(3,2)='Fill area'
      CHITEM(4,2)='Polyline'
      CHITEM(5,2)='Polymarker'
      CHITEM(6,2)='Axis'
      CHITEM(7,2)='Arc'
      CHITEM(8,2)='Box'
      CHITEM(9,2)='Paving-block'
      CHITEM(10,2)='Frame box'
      CHITEM(11,2)='Arrow'
      CHITEM(12,2)='|Change Att.'
      CHITEM(13,2)='|Delete'
      CHITEM(14,2)='|Move'
      CHITEM(15,2)='|Front'
      CHITEM(16,2)='Grid'
      CHUSKE(1,2)='Att.'
      CHUSKE(2,2)='Redr.'
      CHUSKE(3,2)='Undo'
      IPLACE=2
      CALL IGREQ(21,3,16,IPLACE,ICHOIC,CHVAL)
*
  110 CONTINUE
*
      IF(IPLACE.EQ.1)GOTO 90
*
      IF(ICHOIC.EQ.-1)GOTO 100
*
      IF(ICHOIC.EQ.-1000)GOTO 100
*
      IF(ICHOIC.EQ.-2)THEN
         CALL IGCLES
         CALL IZPICT(EDIPIC,'D')
         CALL IEGRID
         GOTO 100
      ENDIF
*
      IF(ICHOIC.EQ.-3)THEN
         CALL IZUNDO
         GOTO 100
      ENDIF
*
      IF(ICHOIC.EQ.1)THEN
         CALL IESTX(ICF,ICHOIC)
         IF(ICHOIC.NE.0)THEN
            IF(ICF.EQ.1)GOTO 90
            IF(ICF.EQ.2)GOTO 110
         ENDIF
      ENDIF
*
      IF(ICHOIC.EQ.2)THEN
         CALL IETX(ICF,ICHOIC)
         IF(ICHOIC.NE.0)THEN
            IF(ICF.EQ.1)GOTO 90
            IF(ICF.EQ.2)GOTO 110
         ENDIF
      ENDIF
*
      IF(ICHOIC.EQ.3)THEN
         CALL IEFA(ICF,ICHOIC)
         IF(ICHOIC.NE.0)THEN
            IF(ICF.EQ.1)GOTO 90
            IF(ICF.EQ.2)GOTO 110
         ENDIF
      ENDIF
*
      IF(ICHOIC.EQ.4)THEN
         CALL IEPL(ICF,ICHOIC)
         IF(ICHOIC.NE.0)THEN
            IF(ICF.EQ.1)GOTO 90
            IF(ICF.EQ.2)GOTO 110
         ENDIF
      ENDIF
*
      IF(ICHOIC.EQ.5)THEN
         CALL IEPM(ICF,ICHOIC)
         IF(ICHOIC.NE.0)THEN
            IF(ICF.EQ.1)GOTO 90
            IF(ICF.EQ.2)GOTO 110
         ENDIF
      ENDIF
*
      IF(ICHOIC.EQ.6)THEN
         CALL IEAXIS(ICF,ICHOIC)
         IF(ICHOIC.NE.0)THEN
            IF(ICF.EQ.1)GOTO 90
            IF(ICF.EQ.2)GOTO 110
         ENDIF
      ENDIF
*
      IF(ICHOIC.EQ.7)THEN
         CALL IEARC(ICF,ICHOIC)
         IF(ICHOIC.NE.0)THEN
            IF(ICF.EQ.1)GOTO 90
            IF(ICF.EQ.2)GOTO 110
         ENDIF
      ENDIF
*
      IF(ICHOIC.EQ.8)THEN
         CALL IEBOX(ICF,ICHOIC)
         IF(ICHOIC.NE.0)THEN
            IF(ICF.EQ.1)GOTO 90
            IF(ICF.EQ.2)GOTO 110
         ENDIF
      ENDIF
*
      IF(ICHOIC.EQ.9)THEN
         CALL IEPAVE(ICF,ICHOIC)
         IF(ICHOIC.NE.0)THEN
            IF(ICF.EQ.1)GOTO 90
            IF(ICF.EQ.2)GOTO 110
         ENDIF
      ENDIF
*
      IF(ICHOIC.EQ.10)THEN
         CALL IEFBOX(ICF,ICHOIC)
         IF(ICHOIC.NE.0)THEN
            IF(ICF.EQ.1)GOTO 90
            IF(ICF.EQ.2)GOTO 110
         ENDIF
      ENDIF
*
      IF(ICHOIC.EQ.11)THEN
         CALL IEARRO(ICF,ICHOIC)
         IF(ICHOIC.NE.0)THEN
            IF(ICF.EQ.1)GOTO 90
            IF(ICF.EQ.2)GOTO 110
         ENDIF
      ENDIF
*
      IF(ICHOIC.EQ.12)THEN
         CALL IECPA(ICF,ICHOIC)
         IF(ICHOIC.NE.0)THEN
            IF(ICF.EQ.1)GOTO 90
            IF(ICF.EQ.2)GOTO 110
         ENDIF
      ENDIF
*
      IF(ICHOIC.EQ.13)THEN
         CALL IEDELE(ICF,ICHOIC)
         IF(ICHOIC.NE.0)THEN
            IF(ICF.EQ.1)GOTO 90
            IF(ICF.EQ.2)GOTO 110
         ENDIF
      ENDIF
*
      IF(ICHOIC.EQ.14)THEN
         CALL IEMOPR(ICF,ICHOIC)
         IF(ICHOIC.NE.0)THEN
            IF(ICF.EQ.1)GOTO 90
            IF(ICF.EQ.2)GOTO 110
         ENDIF
      ENDIF
*
      IF(ICHOIC.EQ.15)THEN
         CALL IEPOP(ICF,ICHOIC)
         IF(ICHOIC.NE.0)THEN
            IF(ICF.EQ.1)GOTO 90
            IF(ICF.EQ.2)GOTO 110
         ENDIF
      ENDIF
*
      IF(ICHOIC.EQ.16)THEN
         TITLE(3)='GRID ATTRIBUTES'
         CHITEM(1,3)='X Step'
         CHITEM(2,3)='Y Step'
         CHITEM(3,3)='X Factor'
         CHITEM(4,3)='Y Factor'
         CHITEM(5,3)='Grid'
         IPLACE=3
         IF(IXSTEP.EQ.0.AND.IYSTEP.EQ.0)THEN
            IXSTEP=RWXMAX
            IYSTEP=RWYMAX
         ENDIF
         CALL IZITOC(IXSTEP,CHDEF(1))
         CALL IZITOC(IYSTEP,CHDEF(2))
         CALL IZITOC(IXFACT,CHDEF(3))
         CALL IZITOC(IYFACT,CHDEF(4))
         CHDEF(5)='Yes,No'
         CALL IGREQ(217,0,5,IPLACE,ICHOIC,CHVAL)
         IF(IPLACE.EQ.1)GOTO 90
         IF(IPLACE.EQ.2)GOTO 110
         CALL IZCTOI(CHVAL(1),IXSTEP)
         CALL IZCTOI(CHVAL(2),IYSTEP)
         CALL IZCTOI(CHVAL(3),IXFACT)
         CALL IZCTOI(CHVAL(4),IYFACT)
         IF(CHVAL(5).EQ.'No')THEN
            IXFACT=0
            GOTO 100
         ENDIF
         IF(IXSTEP.EQ.0.AND.IYSTEP.NE.0)IXSTEP=IYSTEP
         IF(IXSTEP.NE.0.AND.IYSTEP.EQ.0)IYSTEP=IXSTEP
         IF(IXSTEP.EQ.0.AND.IYSTEP.EQ.0)GOTO 100
         IF(IXFACT.EQ.0.AND.IYFACT.NE.0)IXFACT=IYFACT
         IF(IXFACT.NE.0.AND.IYFACT.EQ.0)IYFACT=IXFACT
         IF(IXFACT.EQ.0.AND.IYFACT.EQ.0)GOTO 100
         CALL IEGRID
      ENDIF
*
      GOTO 100
*.______________________________________
*
*              Windows Menu
*
  140 TITLE(2)='WINDOWS'
      CHITEM(1,2)='|Create'
      CHITEM(2,2)='  '
      CHITEM(3,2)='Select'
      CHITEM(4,2)='Move'
      CHITEM(5,2)='Show'
      CHITEM(6,2)='Push behind'
      CHITEM(7,2)=' '
      CHITEM(8,2)=' '
      CALL IZITOC(INTR,CINTR)
      CHUSKE(1,2)=CINTR
      CHUSKE(2,2)='Redraw'
      IPLACE=2
      CALL IGREQ(31,2,8,IPLACE,ICHOIC,CHVAL)
*
  150 CONTINUE
*
      IF(IPLACE.EQ.1)GOTO 90
*
      IF(ICHOIC.EQ.-1000)GOTO 140
*
      IF(ICHOIC.EQ.-1)GOTO 140
*
      IF(ICHOIC.EQ.-2)THEN
         CALL IGCLES
         CALL IZPICT(EDIPIC,'D')
         CALL IEGRID
         GOTO 140
      ENDIF
*
      IF(ICHOIC.EQ.1)THEN
         CALL IEWIN(ICF,ICHOIC)
         IF(ICHOIC.NE.0)THEN
            IF(ICF.EQ.1)GOTO 90
            IF(ICF.EQ.2)GOTO 150
         ENDIF
      ENDIF
*
      IF(ICHOIC.EQ.2)THEN
      ENDIF
*
      IF(ICHOIC.EQ.3)THEN
         IPLACE=2
  160    CALL IGREQ(31,2,8,IPLACE,ICHOIC,CHVAL)
         IF(IPLACE.EQ.1)GOTO 90
         IF(ICHOIC.NE.0)GOTO 150
         ZFLAG=.TRUE.
         GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
         CALL ISELNT(IQUEST(10))
         CALL IZITOC(IQUEST(10),CINTR)
         CHUSKE(1,2)=CINTR
         IPLACE=2
         CALL IGMENU(0,TITLE(IPLACE)
     +,  XPM(1,IPLACE),XPM(2,IPLACE)
     +,  YPM(1,IPLACE),YPM(2,IPLACE)
     +,  NBU(IPLACE),CHUSKE(1,IPLACE)
     +,  NBITEM(IPLACE),CHITEM(1,IPLACE)
     +,  CHDEF,CHVAL,1,'UWT')
         GOTO 160
      ENDIF
*
      IF(ICHOIC.EQ.4)THEN
  170    IF(INTR.EQ.0)GOTO 140
         INTSAV=INTR
         CALL ISELNT(0)
         IPLACE=2
         CALL IGREQ(31,2,8,IPLACE,ICHOIC,CHVAL)
         IF(IPLACE.EQ.1)THEN
            ZFLAG=.TRUE.
            GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
            CALL ISELNT(INTSAV)
            GOTO 90
         ENDIF
         IF(ICHOIC.NE.0)THEN
            ZFLAG=.TRUE.
            GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
            CALL ISELNT(INTSAV)
            GOTO 150
         ENDIF
         X(1)=RQUEST(13)
         Y(1)=RQUEST(14)
         IPLACE=2
         CALL IGREQ(31,2,8,IPLACE,ICHOIC,CHVAL)
         IF(IPLACE.EQ.1)THEN
            ZFLAG=.TRUE.
            GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
            CALL ISELNT(INTSAV)
            GOTO 90
         ENDIF
         IF(ICHOIC.NE.0)THEN
            ZFLAG=.TRUE.
            GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
            CALL ISELNT(INTSAV)
            GOTO 150
         ENDIF
         X(2)=RQUEST(13)
         Y(2)=RQUEST(14)
         ZFS=ZFLAG
         CALL IGCLWN(INTSAV)
         CALL IGEX(X(1),X(2))
         CALL IGEX(Y(1),Y(2))
         ZFLAG=.TRUE.
         GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
         CALL ISVP(INTSAV,X(1),X(2),Y(1),Y(2))
         CALL ISELNT(INTSAV)
         ZFLAG=.FALSE.
         GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
         LN=IZGNTP(INTSAV)
         CALL IZDNB(LN,LDI,LDF,LDC,0,0)
         ZFLAG=ZFS
         GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
         GOTO 170
      ENDIF
*
      IF(ICHOIC.EQ.5)THEN
         NGT=0
         LN=IZGNGT(LPICD,NGT)
         DO 190 I=1,IQ(LPICD+8)
            IF(LN.EQ.0)GOTO 140
            IWVA=IQ(LN+2)
            INTN=IQ(LN+1)
            ILNTY=1
            IF(INTN.EQ.INTR)ILNTY=2
            IF(INTN.EQ.0)GOTO 180
            CALL IGBORD(INTN
     +,               Q(LDF+IWVA)
     +,               Q(LDF+IWVA+1)
     +,               Q(LDF+IWVA+2)
     +,               Q(LDF+IWVA+3)
     +,               ILNTY)
  180       NGT=IQ(LN+3)
            LN=IZGNGT(LPICD,NGT)
  190    CONTINUE
      ENDIF
*
      IF(ICHOIC.EQ.6)THEN
         NGT=0
         LN=IZGNGT(LPICD,NGT)
         DO 200 I=1,IQ(LPICD+8)-1
            IF(LN.EQ.0)GOTO 210
            NGT=IQ(LN+3)
            CALL ISELNT(IQ(LN+1))
            LN=IZGNGT(LPICT,NGT)
  200    CONTINUE
  210    IHNTR=IQ(IZGNGT(LPICT,IZGHNP(LPICT)-1)+1)
         CALL IZITOC(IHNTR,CINTR)
         CHUSKE(1,2)=CINTR
         IPLACE=2
         CALL IGMENU(0,TITLE(IPLACE)
     +,  XPM(1,IPLACE),XPM(2,IPLACE)
     +,  YPM(1,IPLACE),YPM(2,IPLACE)
     +,  NBU(IPLACE),CHUSKE(1,IPLACE)
     +,  NBITEM(IPLACE),CHITEM(1,IPLACE)
     +,  CHDEF,CHVAL,1,'UTW')
         CALL ISELNT(IHNTR)
      ENDIF
*
      IF(ICHOIC.EQ.7)THEN
         IPLACE=2
         CALL IGREQ(31,2,8,IPLACE,ICHOIC,CHVAL)
         IF(IPLACE.EQ.1)GOTO 90
         IF(ICHOIC.GE.1)GOTO 150
         LPCOP=LPICD
         LNTCOP=IZGNTP(IQUEST(10))
      ENDIF
*
      GOTO 140
*.______________________________________
*
*              Pictures Menu
*
  220 TITLE(2)='PICTURES'
      CHITEM(1,2)='List'
      CHITEM(2,2)='Protect'
      CHITEM(3,2)='Merge'
      CHITEM(4,2)='Create'
      CHITEM(5,2)='Select'
      CHITEM(6,2)='Next'
      CHITEM(7,2)='Print struct.'
      CHITEM(8,2)='Show'
      CHUSKE(1,2)=EDIPIC
      IPLACE=2
      CALL IGREQ(41,1,8,IPLACE,ICHOIC,CHVAL)
*
  230 CONTINUE
*
      IF(IPLACE.EQ.1)GOTO 90
*
      IF(ICHOIC.EQ.-1.OR.ICHOIC.EQ.-1000)GOTO 220
*
      IF(ICHOIC.EQ.1)THEN
         CALL IZPICT(' ','L')
         GOTO 220
      ENDIF
*
      IF(ICHOIC.EQ.2)THEN
         LNT=LQ(LPICT-1)
         DO 240 I=1,IQ(LPICT+8)
            IF(LNT.EQ.0)GOTO 220
            IQ(LNT+IQ(LNT+4))=1000000*IUNDCO
            IQ(LNT+4)=IQ(LNT+4)+1
            IF(LNT.EQ.LHNT)ITBPTR=ITBPTR+1
            LNT=LQ(LNT)
  240    CONTINUE
      ENDIF
*
      IF(ICHOIC.EQ.3)THEN
         TITLE(3)=' '
         CHITEM(1,3)='Picture Name ?'
         IPLACE=3
         CHDEF(1)=' '
         CALL IGREQ(42,0,1,IPLACE,ICHOIC,MERPIC)
         IF(IPLACE.EQ.1)GOTO 90
         IF(IPLACE.EQ.2)GOTO 230
         IF(IZRPIP(MERPIC).LE.0)GOTO 220
         CALL CLTOU(MERPIC)
         INTSAV=INTR
         CALL ISELNT(0)
         IPLACE=2
         CALL IGREQ(41,1,8,IPLACE,ICHOIC,CHVAL)
         IF(IPLACE.EQ.1)THEN
            CALL ISELNT(INTSAV)
            GOTO 90
         ENDIF
         IF(ICHOIC.NE.0)THEN
            CALL ISELNT(INTSAV)
            GOTO 230
         ENDIF
         X(1)=RQUEST(13)
         Y(1)=RQUEST(14)
         IPLACE=2
         CALL IGREQ(41,1,8,IPLACE,ICHOIC,CHVAL)
         IF(IPLACE.EQ.1)THEN
            CALL ISELNT(INTSAV)
            GOTO 90
         ENDIF
         IF(ICHOIC.NE.0)THEN
            CALL ISELNT(INTSAV)
            GOTO 230
         ENDIF
         X(2)=RQUEST(13)
         Y(2)=RQUEST(14)
         CALL IGEX(X(1),X(2))
         CALL IGEX(Y(1),Y(2))
         CALL IZMERG(MERPIC,X(1),Y(1),X(2)-X(1),' ')
      ENDIF
*
      IF(ICHOIC.EQ.4)THEN
         TITLE(3)=' '
         CHITEM(1,3)='Picture Name ?'
         IPLACE=3
         CHDEF(1)=' '
         CALL IGREQ(42,0,1,IPLACE,ICHOIC,EDIPIC)
         CALL CLTOU(EDIPIC)
         CALL IZPICT(EDIPIC,'M')
         CALL IGCLES
         CALL ISELNT(0)
         GOTO 250
      ENDIF
*
      IF(ICHOIC.EQ.5)THEN
         TITLE(3)=' '
         CHITEM(1,3)='Picture Name ?'
         IPLACE=3
         CHDEF(1)=' '
         CALL IGREQ(42,0,1,IPLACE,ICHOIC,PRIPIC)
         CALL CLTOU(PRIPIC)
         IF(IPLACE.EQ.1)GOTO 90
         IF(IPLACE.EQ.2)GOTO 230
         IF(IZRPIP(PRIPIC).LE.0)GOTO 220
         EDIPIC=PRIPIC
         CALL IZPICT(EDIPIC,'C')
         CALL IGCLES
         CALL IZPICT(EDIPIC,'D')
         GOTO 250
      ENDIF
*
      IF(ICHOIC.EQ.6)THEN
         CALL IGCLES
         CALL IZPICT(' ','N')
         CALL IZPICT(' ','D')
         CALL IZPICT(EDIPIC,'R')
         GOTO 250
      ENDIF
*
      IF(ICHOIC.EQ.7)THEN
         TITLE(3)=' '
         CHITEM(1,3)='Picture Name ?'
         IPLACE=3
         CHDEF(1)=EDIPIC
         CALL IGREQ(42,0,1,IPLACE,ICHOIC,PRIPIC)
         CALL CLTOU(PRIPIC)
         CALL IZPICT(PRIPIC,'P')
         GOTO 220
      ENDIF
*
      IF(ICHOIC.EQ.8)THEN
         TITLE(3)=' '
         CHITEM(1,3)='Picture Name ?'
         IPLACE=3
         CHDEF(1)=' '
         CALL IGREQ(42,0,1,IPLACE,ICHOIC,SHOPIC)
         IF(IPLACE.EQ.1)GOTO 90
         IF(IPLACE.EQ.2)GOTO 230
         IF(IZRPIP(SHOPIC).LE.0)GOTO 220
         CALL CLTOU(SHOPIC)
         CALL IGSRAP(.3)
         CALL IGCLES
         CALL IZPICT(SHOPIC,'D')
         CALL IZPICT(EDIPIC,'C')
         CALL IGSRAP(.8)
         GOTO 220
      ENDIF
*
      GOTO 220
  250 CONTINUE
      CHUSKE(1,2)=EDIPIC(1:LENOCC(EDIPIC))
      IPLACE=2
      CALL IGMENU(0,TITLE(IPLACE),XPM(1,IPLACE),XPM(2,IPLACE),
     +YPM(1,IPLACE),YPM(2,IPLACE),NBU(IPLACE),CHUSKE(1,IPLACE)
     +,NBITEM(IPLACE),CHITEM(1,IPLACE),CHDEF,CHVAL,1,'UTW')
      GOTO 220
*.______________________________________
*
*              Files Menu
*
  260 TITLE(2)='FILES'
      CHITEM(1,2)='Save Picture'
      CHITEM(2,2)='Read Picture'
      IPLACE=2
      CALL IGREQ(51,0,2,IPLACE,ICHOIC,CHVAL)
*
      IF(IPLACE.EQ.1)GOTO 90
*
      IF(ICHOIC.EQ.-1.OR.ICHOIC.EQ.-1000)GOTO 260
*
      IF(ICHOIC.EQ.1)THEN
         CALL IZPIWI(EDIPIC)
         CALL IZOUT(EDIPIC,ICYCLE)
         GOTO 260
      ENDIF
*
      IF(ICHOIC.EQ.2)THEN
         TITLE(3)=' '
         CHITEM(1,3)='Picture Name ?'
         IPLACE=3
         CHDEF(1)=' '
         CALL IGREQ(42,0,1,IPLACE,ICHOIC,REAPIC)
         CALL CLTOU(PRIPIC)
         CALL IZIN(REAPIC,9999)
         GOTO 260
      ENDIF
*
      GOTO 260
*.______________________________________
*
*              End with the Graphics Editor
*
  270 CALL ISWKWN(IDID,WX1S,WX2S,WY1S,WY2S)
      CALL ISWKVP(IDID,VX1S,VX2S,VY1S,VY2S)
      CALL IGSRAP(0.)
      CALL ICLRWK(0,0)
      CALL IGSRAP(REDITS)
      DO 280 I=1,NBIATT
         CALL IGSET(CHIATT(I),FLOAT(ISAVED(I)))
  280 CONTINUE
      DO 290 I=1,NBRATT
         CALL IGSET(CHRATT(I),RSAVED(I))
  290 CONTINUE
      CALL IGSRAP(0.)
      CALL IZPIWI(EDIPIC)
*
      END
