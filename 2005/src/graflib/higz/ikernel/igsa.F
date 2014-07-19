*
* $Id: igsa.F,v 1.3 1998/01/28 14:34:41 couet Exp $
*
* $Log: igsa.F,v $
* Revision 1.3  1998/01/28 14:34:41  couet
* - GGDM driver removed
*
* Revision 1.2  1996/09/25 14:58:44  couet
* - GPR driver removed
*
* Revision 1.1.1.1  1996/02/14 13:10:55  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.21/03 06/06/94  11.51.09  by  O.Couet
*-- Author :    O.Couet   06/02/90
      SUBROUTINE IGSA(IWK)
#if defined(CERNLIB_FALCO)
#include "higz/hiatt.inc"
#endif
#include "higz/hiflag.inc"
#if defined(CERNLIB_FALCO)
      CHARACTER*4 CHREP
#endif
      IF(GRFLAG)THEN
         GRFLAG=.FALSE.
#if defined(CERNLIB_FALCO)
         IF(IWK.EQ.0)THEN
            DO 10 I=1,INOPWK
               IF(IWTYL(I).EQ.7878)THEN
                  CALL IFMOVE(0.,0.)
                  CALL IFPUT(0)
                  READ (*,'(A)') CHREP
                  CALL IFPUT(31)
                  CALL IFPUT(24)
                  CALL IFPUT(0)
                  GOTO 10
               ENDIF
               IF(IWTYL(I).EQ.7879)THEN
                  CALL IFPUT(27)
                  CALL IFPUT(3)
                  CALL IFPUT(0)
                  GOTO 10
               ENDIF
   10       CONTINUE
         ELSE
            IWT = IGIWTY(IWK)
            IF(IWT.EQ.7878)THEN
               CALL IFMOVE(0.,0.)
               CALL IFPUT(0)
               READ (*,'(A)') CHREP
               CALL IFPUT(31)
               CALL IFPUT(24)
               CALL IFPUT(0)
               RETURN
            ENDIF
            IF(IWT.EQ.7879)THEN
               CALL IFPUT(27)
               CALL IFPUT(3)
               CALL IFPUT(0)
               RETURN
            ENDIF
         ENDIF
#endif
#if defined(CERNLIB_MSDOS)
         CALL LOCATE(0,0)
#endif
#if defined(CERNLIB_MACMPW)
         CALL IMACPLO(0,0,-999)
#endif
      ENDIF
      END
