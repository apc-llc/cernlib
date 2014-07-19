*
* $Id: izcopy.F,v 1.1.1.1 1996/02/14 13:11:08 mclareni Exp $
*
* $Log: izcopy.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:08  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_ZEBRA)
*CMZ :  1.12/08 28/03/91  12.13.39  by  O.Couet
*-- Author :
      SUBROUTINE IZCOPY(PNME1,PNME2,CHOPT)
*.===========>
*.
*.   This routine copy or rename the picture PNAME1 to the picture PNAME2
*. according to the parameter CHOPT .
*.
*. _Input parameters:
*.
*.  CHARACTER PNME1 : Picture name 1 .
*.  CHARACTER PNME2 : Picture name 2 .
*.  CHARACTER CHOPT : Option .
*.
*.                    CHOPT='C'opy .
*.                    CHOPT='R'ename .
*.
*..==========> (O.Couet)
#include "higz/hipaw.inc"
#include "higz/hiflag.inc"
      CHARACTER*(*) CHOPT,PNME1,PNME2
      CHARACTER*20 PNAME1,PNAME2
      DIMENSION IOPT(2)
      LOGICAL GFSAV,ZFSAV
      EQUIVALENCE (IOPT(1),IOPTC),(IOPT(2),IOPTR)
*.______________________________________
*
      CALL UOPTC (CHOPT,'CR',IOPT)
      IF(CHOPT.EQ.' ')IOPTC=1
      PNAME1=PNME1
      PNAME2=PNME2
      NCHP2=LENOCC(PNME2)
      IF(NCHP2.GT.20)NCHP2=20
      IQUEST(1)=0
*
*              Adressing pictures by number
*
      INUM=IGASCI(PNAME2(1:1))
      IF(49.LE.INUM.AND.INUM.LE.57)GOTO 20
      INUM=IGASCI(PNAME1(1:1))
      IF(49.LE.INUM.AND.INUM.LE.57)THEN
         READ (PNAME1,'(I15)',ERR=20  ) INUM
         IF(INUM.GT.NBPICT)THEN
            CALL IGERR('Picture not in memory','IZCOPY')
            RETURN
         ENDIF
         LP=LQ(LHIGZ)
         DO 10  I=1,INUM-1
            LP=LQ(LP)
   10    CONTINUE
         LCH=LQ(LP-4)
         CALL UHTOC(IQ(LCH+1),4,PNAME1,IQ(LP+1))
      ENDIF
      GOTO 30
   20 CALL IGERR('PNAME must begin with a letter','IZCOPY')
      RETURN
   30 CONTINUE
*
*              Check the validity of PNAME1 and PNAME2
*
      LP1=IZRPIP(PNAME1)
      LP2=IZRPIP(PNAME2)
      IF(PNAME1.EQ.' ')LP1=LPICT
      IF(LP2.NE.0)THEN
         CALL IGERR('Picture already exist','IZCOPY')
         RETURN
      ENDIF
      IF(LP1.EQ.0)THEN
         CALL IGERR('Picture not in memory','IZCOPY')
         RETURN
      ENDIF
*
*              Copy
*
      IF(IOPTC.NE.0)THEN
         LPIC2=LP1
         L2NT0=LQ(LPIC2-1)
         L2I=LQ(LPIC2-2)
         L2F=LQ(LPIC2-3)
         L2C=LQ(LPIC2-4)
         CALL IZPICT(PNAME2,'M')
         GFSAV=GFLAG
         ZFSAV=ZFLAG
         GFLAG=.FALSE.
         ZFLAG=.TRUE.
         GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
         L2NT=L2NT0
         N=IQ(LPIC2+8)
         DO 50 I=1,N
            NT=IQ(L2NT+1)
            IF(NT.EQ.0)GOTO 40
            J=IQ(L2NT+2)
            CALL ISWN(NT,Q(L2F+J),Q(L2F+J+1),Q(L2F+J+2),Q(L2F+J+3))
            CALL ISVP(NT,Q(L2F+J+4),Q(L2F+J+5),Q(L2F+J+6),Q(L2F+J+7))
   40       CALL ISELNT(NT)
            CALL IZDNB(L2NT,L2I,L2F,L2C,0,0)
            L2NT=LQ(L2NT)
            IF(L2NT.EQ.0)GOTO 60
   50    CONTINUE
         IF(JBIT(IQ(LP1),1).NE.0)CALL SBIT1(IQ(LP2),1)
   60    GFLAG=GFSAV
         ZFLAG=ZFSAV
         GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
         CALL IZPICT(PNAME1,'C')
         RETURN
      ENDIF
*
*              Rename
*
      IF(IOPTR.NE.0)THEN
         LPSAV=LPICT
         CALL IZSCPI(LP1)
         CALL UCTOH(PNAME2,IQ(LHC+1),4,20)
         IQ(LPICT+1)=NCHP2
         LPICT=LPSAV
         IF(LPSAV.GT.0)CALL IZSCPI(LPSAV)
         RETURN
      ENDIF
*
      END
#endif
