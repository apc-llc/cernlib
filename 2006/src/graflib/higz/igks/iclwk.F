*
* $Id: iclwk.F,v 1.2 1996/06/05 10:20:31 cernlib Exp $
*
* $Log: iclwk.F,v $
* Revision 1.2  1996/06/05 10:20:31  cernlib
* Move pilot.h before the ifdef for GKS
*
* Revision 1.1.1.1  1996/02/14 13:10:44  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_GKS)
*CMZ :  1.18/09 08/06/93  17.58.36  by  O.Couet
*-- Author :
      SUBROUTINE ICLWK(IWKID)
*.===========>
*.
*.   This routine closes the IWKID workstation .
*.
*. _Input parameters:
*.
*.  INTEGER IWKID : Workstation identifier .
*.
*..==========> (O.Couet)
#include "higz/hiatt.inc"
#include "higz/hiflag.inc"
#if defined(CERNLIB_PSCRIPT)
#include "higz/hipost.inc"
#endif
#include "higz/himeta.inc"
*.______________________________________
*
      IF(IWKID.EQ.IDMETA.AND.FILOPN)FILOPN=.FALSE.
#if defined(CERNLIB_PSCRIPT)
      IF(IGIWTY(IWKID).LT.0)THEN
         CALL IPEND
         GOTO 30
      ENDIF
#endif
#if defined(CERNLIB_GKS)
      CALL IGSG(0)
      CALL GCLWK(IWKID)
#endif
*
*              Update the open-workstation list
*
   30 CONTINUE
      DO 50 I=1,INOPWK
         IF(ILOPWK(I).EQ.IWKID)THEN
            INOPWK=INOPWK-1
            DO 40 J=I,INOPWK
               ILOPWK(J) = ILOPWK(J+1)
               IWTYL(J)  = IWTYL(J+1)
#if defined(CERNLIB_PSCRIPT)
               IF (IWTYL(J).LT.0) IPSI = J
#endif
               ACWKFL(J) = ACWKFL(J+1)
   40       CONTINUE
            RETURN
         ENDIF
   50 CONTINUE
*
      END
#endif
