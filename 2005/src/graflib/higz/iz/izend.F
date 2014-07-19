*
* $Id: izend.F,v 1.1.1.1 1996/02/14 13:11:09 mclareni Exp $
*
* $Log: izend.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:09  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
*CMZ :  1.07/00 23/05/89  15.36.30  by  O.Couet
*-- Author :
      SUBROUTINE IZEND
*.===========>
*.
*.   End with HIGZ . If the automatic saving is on the whole
*. pictures always in memory are saved in the current open
*. HIGZ file.
*.
*..==========> (O.Couet)
#if defined(CERNLIB_ZEBRA)
#include "higz/hipaw.inc"
#endif
#include "higz/hiflag.inc"
#if defined(CERNLIB_ZEBRA)
#include "higz/hicur.inc"
#endif
#if defined(CERNLIB_MAIL)
#include "higz/himail.inc"
#endif
      DIMENSION INAME(5)
      CHARACTER*32 OLDDIR
*.______________________________________
*
#if defined(CERNLIB_ZEBRA)
      IF((NBPICT.NE.0).AND.ASFLAG)THEN
         LP=LQ(LHIGZ)
         CALL RZCDIR(OLDDIR,'R')
         CALL RZCDIR(HICDIR,' ')
         DO 10 I=1,NBPICT
            CALL IZSCPI(LP)
            CALL VBLANK(INAME,5)
            LOOP=(IQ(LPICT+1)+3)/4
            DO 20 II=1,LOOP
               INAME(II)=IQ(LQ(LPICT-4)+II)
  20        CONTINUE
            CALL IZWIP(LPICT)
            CALL RZOUT(IXHIGZ,LPICT,INAME,ICYCLE,' ')
            CALL RZSAVE
            LP=LQ(LPICT)
  10     CONTINUE
         CALL RZCDIR(OLDDIR,' ')
      ENDIF
#endif
#if defined(CERNLIB_MAIL)
*
      IF(MFLAG)THEN
         WRITE (CHMAIL,'(I3)') 999
         CALL IMWRIT(4)
      ENDIF
#endif
*
      END
#endif
