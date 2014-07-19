*
* $Id: izclrd.F,v 1.1.1.1 1996/02/14 13:11:08 mclareni Exp $
*
* $Log: izclrd.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:08  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_PSCRIPT)||defined(CERNLIB_MAIL)
*CMZ :  1.23/01 24/05/95  10.42.01  by  O.Couet
*-- Author :
      SUBROUTINE IZCLRD
*.===========>
*.
*.   This routine stores in the current picture the necessary data to
*. clear a workstation .
*.
*..==========> (O.Couet)
#include "higz/hiflag.inc"
#if defined(CERNLIB_ZEBRA)
#include "higz/hipaw.inc"
#endif
#if defined(CERNLIB_PSCRIPT)
#include "higz/hipost.inc"
#endif
#if defined(CERNLIB_MAIL)
#include "higz/himail.inc"
#endif
*.______________________________________
*
#if defined(CERNLIB_PSCRIPT)
      IF(PFLAG)THEN
         IF(LOPRT)THEN
            IF(TFLAG)THEN
               CALL IPPSTR(BSLASH//'fi'//BSLASH//'end{picture}@')
               IF(IPTWTY.EQ.-778)THEN
                  CALL IPPSTR(BSLASH//'vfill'//BSLASH//'eject@')
                  LOBB=.FALSE.
               ENDIF
            ELSE
               IF (NBSAVE.GT.NBSAV0) CALL IPSVRT(-1)
            ENDIF
            LOPRT=.FALSE.
            LOCLR=.TRUE.
         ENDIF
      ENDIF
#endif
#if defined(CERNLIB_ZEBRA)
*
      IF(ZFLAG)THEN
         IF(LPICT.LT.0)RETURN
         CALL SBIT1(IQ(LPICT),1)
      ENDIF
#endif
#if defined(CERNLIB_MAIL)
*
      IF(MFLAG)THEN
         WRITE (CHMAIL,'(I3)') 101
         CALL IMWRIT(4)
      ENDIF
#endif
*
      END
#endif
