*
* $Id: izin.F,v 1.1.1.1 1996/02/14 13:11:10 mclareni Exp $
*
* $Log: izin.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:10  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_ZEBRA)
*CMZ :  1.07/00 24/05/89  16.36.33  by  O.Couet
* Control if the current directory is a picture data base.
*-- Author :
      SUBROUTINE IZIN(PNAME,ICYCLE)
*.===========>
*.
*.   This routine manages the INput of the pictures on RZ files .
*.
*. _Input parameters:
*.
*.  CHARACTER PNAME : Picture name .
*.  INTEGER ICYCLE : Cycle number of the picture on the RZ file .
*.
*.  Special cases:
*.
*.      If ICYCLE<0 then IABS(ICYCLE) is assumed to be the
*.      picture serial number in the RZ file.
*.      In this case PNAME is an output parameter containing
*.      the name of the picture.
*.      If this serial number does not exist, then PNAME is set to ' '
*.      and IQUEST(1)=1
*.
*.      If PNAME='*' all pictures on disk are put in memory
*.
*..==========> (O.Couet)
#include "higz/hipaw.inc"
      CHARACTER*(*) PNAME
      CHARACTER*20 PNAMEI
      CHARACTER*32 CHPATH
      DIMENSION NAME(5)
      CHARACTER*1 CHOPT
      CHARACTER*8 CHTAG(5),CHFORM
*.______________________________________
*
*              Verify if the picture PNAME is in memory
*
      IF(IZRPIP(PNAME).NE.0)THEN
         CALL IGERR('Picture already in memory','IZIN')
         RETURN
      ENDIF
*
*              Verify if the current working directrory
*              is a pictures data base
*
      CALL RZKEYD(NWKEY,CHFORM,CHTAG)
      IF(CHTAG(2).NE.'PICTURE')RETURN
*
*              Create the bank LHIGZ if it does'nt exist
*
      LPSAV=LPICT
      IF(LHIGZ.EQ.0)THEN
         CALL MZBOOK(IXHIGZ,LPICT,LHIGZ,1,'PICT',1,1,5,2,0)
      ENDIF
*
*              Compute the link of the last picture in memory
*
      LP=LHIGZ
   10 IF(LP.NE.0)THEN
         LAST=LP
         LP=LQ(LP)
         GOTO 10
      ENDIF
*
*              Get the number of pictures on disk
*
      CALL RZCDIR(CHPATH,'RP')
      NKEYS=IQUEST(7)
*
*              Put all pictures on disk in memory
*
      IF(PNAME.EQ.'*')THEN
         DO 40 I=1,NKEYS
            CALL RZIN(IXHIGZ,LAST,0,I,9999,'S')
            LP=LHIGZ
   20       IF(LP.NE.0)THEN
               LAST=LP
               LP=LQ(LP)
               GOTO 20
            ENDIF
            CALL IZSCPI(LAST)
            IF(IQUEST(1).EQ.0)NBPICT=NBPICT+1
            CALL IZPICT(PNAMEI,'R')
            LP=IZRPIP(PNAMEI)
            IF((LP.GT.0).AND.(LP.NE.LPICT))CALL IZPICT(' ','S')
            LP=LHIGZ
   30       IF(LP.NE.0)THEN
               LAST=LP
               LP=LQ(LP)
               GOTO 30
            ENDIF
   40    CONTINUE
         CALL IZSCPI(LPSAV)
         RETURN
      ENDIF
*
*              Put one picture on disk in memory (by name or by number)
*
      IF(ICYCLE.GE.0)THEN
         CHOPT=' '
         LP=IZRPIP(PNAME)
         NCH=LENOCC(PNAME)
         CALL VBLANK(NAME,5)
         CALL UCTOH(PNAME,NAME,4,NCH)
         ICY=ICYCLE
      ELSE
         NAME(1)=-ICYCLE
         PNAME=' '
         IF(NAME(1).GT.NKEYS)THEN
            IQUEST(1)=1
            RETURN
         ENDIF
         NCH=LEN(PNAME)
         NCH=MIN(NCH,20)
         CHOPT='S'
         LP=0
         ICY=9999
      ENDIF
      IF(LP.GT.0) CALL MZDROP(IXHIGZ,LP,' ')
      IF(LAST.EQ.0)THEN
         CALL RZIN(IXHIGZ,LHIGZ,1,NAME,ICY,CHOPT)
      ELSE
         CALL RZIN(IXHIGZ,LAST,0,NAME,ICY,CHOPT)
      ENDIF
      IF(IQUEST(1).EQ.0)THEN
         NBPICT=NBPICT+1
         IF(ICYCLE.LT.0) CALL UHTOC(IQUEST(21),4,PNAME,NCH)
      ENDIF
      CALL IZSCPI(LPSAV)
*
      END
#endif
