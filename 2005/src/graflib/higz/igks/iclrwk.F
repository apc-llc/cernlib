*
* $Id: iclrwk.F,v 1.2 1996/06/05 10:20:31 cernlib Exp $
*
* $Log: iclrwk.F,v $
* Revision 1.2  1996/06/05 10:20:31  cernlib
* Move pilot.h before the ifdef for GKS
*
* Revision 1.1.1.1  1996/02/14 13:10:44  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_GKS)
*CMZ :  1.20/01 03/11/93  11.06.46  by  O.Couet
*-- Author :
      SUBROUTINE ICLRWK(WKID,COFL)
*.===========>
*.
*.   This routine clears the WKID workstation .
*.
*. _Input parameters:
*.
*.  INTEGER WKID : Workstation identifier .
*.  INTEGER COFL : Control flag .
*.
*..==========> (O.Couet)
#include "higz/hiflag.inc"
#include "higz/hiatt.inc"
#include "higz/himeta.inc"
      INTEGER WKID,COFL
*.______________________________________
*
      IF(IGIWTY(WKID).LT.0)GOTO 20
      IF(GFLAG)THEN
         CALL IGSG(0)
         IF(WKID.NE.0)THEN
            IF((REDIT.NE.0).AND.(REDIT.NE.1.))THEN
               CALL IGCLES
            ELSE
               CALL GCLRWK(WKID,COFL)
            ENDIF
         ELSE
            DO 10 I=1,INOPWK
               IF(IGIWTY(ILOPWK(I)).LT.0)GOTO 10
               IF(ILOPWK(I).EQ.1.AND..NOT.TEKACT)GO TO 10
               IF((REDIT.NE.0).AND.(REDIT.NE.1.))THEN
                  CALL IGCLES
               ELSE
                  CALL GCLRWK(ILOPWK(I),COFL)
               ENDIF
   10       CONTINUE
         ENDIF
         CALL IGSGN
      ENDIF
   20 CONTINUE
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_PSCRIPT)||defined(CERNLIB_MAIL)
      IF(GLFLAG)CALL IZCLRD
#endif
*
      END
#endif
