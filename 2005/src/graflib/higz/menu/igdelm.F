*
* $Id: igdelm.F,v 1.1.1.1 1996/02/14 13:11:15 mclareni Exp $
*
* $Log: igdelm.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:15  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.20/00 01/11/93  10.50.18  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IGDELM(IOPT)
*.===========>
*.
*.   This routine delete the current displayed menu.
*.
*..==========> (O.Couet)
#include "higz/himenu.inc"
#if defined(CERNLIB_GKS)||defined(CERNLIB_DI3000)
#include "higz/hiflag.inc"
#endif
      CHARACTER*8 CHOPT
      DIMENSION IOPT(*)
*.______________________________________
*
#if (defined(CERNLIB_GKS))&&(!defined(CERNLIB_MGKS))
      IF(GFLAG)THEN
         IF(MENNUM.NE.0)THEN
            IF(ISEGM(MENNUM).NE.0)THEN
               CALL GSVIS(MENNUM,0)
               RETURN
            ENDIF
         ENDIF
      ENDIF
#endif
#if defined(CERNLIB_DI3000)
      IF(MENNUM.NE.0)THEN
         IF(ISEGM(MENNUM).NE.0)THEN
            CALL JVISBL(MENNUM,0)
            RETURN
         ENDIF
      ENDIF
#endif
*
      CHOPT=' '
      DZ=0.
      IF(IOPT(14).NE.0)CHOPT(1:2)='TR'
      IF(IOPT(15).NE.0)CHOPT(1:2)=' R'
      IF(IOPT(16).NE.0)CHOPT(3:3)='P'
      IF(IOPT(18).NE.0)CHOPT(4:4)='S'
      IF(IOPT(19).NE.0)CHOPT(5:5)='K'
      IF(CHOPT.NE.' ')DZ=5.*DEC
      CHOPT(6:6)='D'
      CALL IGPAVE(XPOS(1),XPOS(2),YPOS(1),YPOS(2)
     +,           DZ,0,0,CHOPT)
*
      END
