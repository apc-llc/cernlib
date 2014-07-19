*
* $Id: izout.F,v 1.1.1.1 1996/02/14 13:11:12 mclareni Exp $
*
* $Log: izout.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:12  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_ZEBRA)
*CMZ :  1.12/08 28/03/91  12.13.40  by  O.Couet
*-- Author :
      SUBROUTINE IZOUT(PNAME,ICYCLE)
*.===========>
*.
*.   This routine manages the OUTput of the pictures on RZ files .
*.
*. _Input parameters:
*.
*.  CHARACTER PNAME : Picture name.
*.                   - If PNAME='*' all pictures in memory
*.                     are stored on disk
*.                   - If PNAME=' ' the current picture in memory
*.                     is stored on disk
*.  INTEGER ICYCLE : Number of version of the picture on the RZ file .
*.
*..==========> (O.Couet)
#include "higz/hipaw.inc"
      CHARACTER*(*) PNAME
      CHARACTER*20 PNAMEI
      DIMENSION NAME(5)
*.______________________________________
*
      IF(NBPICT.EQ.0)THEN
         CALL IGERR('No picture in memory','IZOUT')
         RETURN
      ENDIF
      PNAMEI=PNAME
*
*              Adressing pictures by number
*
      INUM=IGASCI(PNAMEI(1:1))
      IF(49.LE.INUM.AND.INUM.LE.57)THEN
         READ (PNAMEI,'(I15)',ERR=21 ) INUM
         IF(INUM.GT.NBPICT)THEN
            CALL IGERR('Picture not in memory','IZOUT')
            RETURN
         ENDIF
         LP=LQ(LHIGZ)
         DO 11 I=1,INUM-1
            LP=LQ(LP)
   11    CONTINUE
         LCH=LQ(LP-4)
         CALL UHTOC(IQ(LCH+1),4,PNAMEI,IQ(LP+1))
         GOTO 31
   21    CALL IGERR('PNAME must begin with a letter','IZOUT')
         RETURN
      ENDIF
   31 CONTINUE
*
*              Store all pictures in memory in the current
*              picture data base.
*
      IF(PNAMEI.EQ.'*')THEN
         LP=LPICT
         IF(LP.GT.0)LPSAV=LPICT
         CALL IZPICT(' ','F')
         DO 10 I=1,NBPICT
            CALL IZPICT(PNAMEI,'R')
            IF(PNAMEI.EQ.' ')GOTO 20
            LAST=IZRPIP(PNAMEI)
            NCH=LENOCC(PNAMEI)
            CALL VBLANK(NAME,5)
            CALL UCTOH(PNAMEI,NAME,4,NCH)
            CALL IZWIP(LAST)
            CALL RZOUT(IXHIGZ,LAST,NAME,ICYCLE,' ')
            CALL RZSAVE
            CALL IZPICT(' ','N')
   10    CONTINUE
   20    IF(LP.GT.0)THEN
            CALL IZSCPI(LPSAV)
         ELSE
            LPICT=-1
         ENDIF
         RETURN
      ENDIF
*
*              Store the picture PNAME (or the current picture if
*              PNAME=' ') in memory in the current picture data base.
*
      IF(PNAMEI.NE.' ')THEN
         LAST=IZRPIP(PNAMEI)
         IF(LAST.EQ.0)THEN
            CALL IGERR('This picture is not in memory','IZOUT')
            RETURN
         ENDIF
      ELSE
         IF(LPICT.LE.0)THEN
            CALL IGERR('No current picture','IZOUT')
            RETURN
         ENDIF
         LAST=LPICT
         CALL IZPICT(PNAMEI,'R')
      ENDIF
      NCH=LENOCC(PNAMEI)
      CALL VBLANK(NAME,5)
      CALL UCTOH(PNAMEI,NAME,4,NCH)
      CALL IZWIP(LAST)
      CALL RZOUT(IXHIGZ,LAST,NAME,ICYCLE,' ')
      CALL RZSAVE
*
      END
#endif
