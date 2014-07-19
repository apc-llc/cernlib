*
* $Id: igsrap.F,v 1.1.1.1 1996/02/14 13:10:38 mclareni Exp $
*
* $Log: igsrap.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:38  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.14/11 26/06/92  14.55.46  by  O.Couet
*-- Author :    O.Couet   26/10/88
      SUBROUTINE IGSRAP(R)
*.===========>
*.
*.   This routine allows to change the REDIT factor.
*.
*. _Input parameters:
*.
*.  REAL R : New REDIT factor (If R.GE.0)
*.
*. _Output parameters:
*.
*.  REAL R : Current REDIT factor (If R.LT.0)
*.
*..==========> (O.Couet)
#if (defined(CERNLIB_KERNEL))&&(!defined(CERNLIB_BATCH))
#include "higz/hikern.inc"
#endif
#if defined(CERNLIB_MAIL)
#include "higz/himail.inc"
#endif
#if defined(CERNLIB_MAIL)
#include "higz/hiflag.inc"
#endif
#include "higz/hiatt.inc"
*.______________________________________
*
#if defined(CERNLIB_MAIL)
*
      IF(MFLAG)THEN
         WRITE (CHMAIL,'(I3,E16.7)') 558,R
         CALL IMWRIT(1)
         IF(R.LT.0)READ (5,'(E16.7)') R
         CALL IMWRIT(5)
         RETURN
      ENDIF
#endif
*
      IF(R.LT.0)THEN
         R=REDIT
         RETURN
      ELSE
         REDIT=R
         IF(REDIT.EQ.1.)REDIT=0.
      ENDIF
#if (defined(CERNLIB_KERNEL))&&(!defined(CERNLIB_BATCH))
*
      DO 10 I=1,INOPWK
         WKSC(I)=REDIT
         IF(WKSC(I).LE.0)WKSC(I)=1.
         WKSC(I)=WKSC(I)*WKMAX(I)
#endif
#if defined(CERNLIB_X11)
         CALL IKUWK(ILOPWK(I))
#endif
#if (defined(CERNLIB_KERNEL))&&(!defined(CERNLIB_BATCH))
   10 CONTINUE
#endif
*
      END
