*
* $Id: igsgn.F,v 1.2 1996/06/05 10:20:35 cernlib Exp $
*
* $Log: igsgn.F,v $
* Revision 1.2  1996/06/05 10:20:35  cernlib
* Move pilot.h before the ifdef for GKS
*
* Revision 1.1.1.1  1996/02/14 13:10:44  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_GKS)
*CMZ :  1.06/03 12/12/88  15.20.43  by  O.Couet
*-- Author :
      SUBROUTINE IGSGN
*.===========>
*.
*.   IGSGN send escape characters to clear workstation (T4107)
*. The terminal must be an activated HIGZ workstation .
*.
*..==========> (O.Couet)
#if (defined(CERNLIB_PLOT10GKS))&&(defined(CERNLIB_IBM))
#include "higz/hiatt.inc"
#include "higz/hiflag.inc"
      INTEGER T4107
      PARAMETER (T4107=410700)
      DIMENSION N(3),M(32)
      DATA N/31,27,12/
      DATA M/32*22/
*.______________________________________
*
      CALL IGSG(0)
      IF(IWTYPE.EQ.T4107)THEN
         CALL HWRASC(3,N)
         DO 10 I=1,8
            CALL HWRASC(32,M)
  10     CONTINUE
      ENDIF
#endif
*
      END
#endif
