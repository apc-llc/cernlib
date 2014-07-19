*
* $Id: igend.F,v 1.1.1.1 1996/02/14 13:10:34 mclareni Exp $
*
* $Log: igend.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:34  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.20/02 03/11/93  11.47.49  by  O.Couet
*-- Author :
      SUBROUTINE IGEND
*.===========>
*.
*.   This routine end an HIGZ cession .
*.
*..==========> (O.Couet)
#if defined(CERNLIB_GKS)
#include "higz/hiatt.inc"
#endif
#if defined(CERNLIB_DI3000)
#include "higz/di3seg.inc"
#endif
#if defined(CERNLIB_DI3000)
#include "higz/di3dev.inc"
#endif
#if defined(CERNLIB_DI3000)
      INTEGER*4 MAXDEV
      PARAMETER (MAXDEV = 9)
      INTEGER*4 DEVICES(0:MAXDEV)
      EQUIVALENCE (ILOPWK,DEVICES)
#endif
*.______________________________________
*
#if defined(CERNLIB_FALCO)
      CALL IGSA(0)
#endif
#if defined(CERNLIB_GKS)
      INOP=INOPWK
      DO 10 I=1,INOP
         CALL IGQWK(ILOPWK(I),'ACTI',RVAL)
         IF(RVAL.NE.0.)THEN
            CALL IDAWK(ILOPWK(I))
         ENDIF
         CALL ICLWK(ILOPWK(I))
  10  CONTINUE
#endif
#if defined(CERNLIB_DI3000)
      IF (ENTRY_FLAG) CALL JUPDAT
      IF (SEGOPN) THEN
         CALL JCLOSE
         SEGOPN = .FALSE.
      ENDIF
      DO 10 I=0,MAXDEV
         IF (DEVICES(I).NE.0) THEN
            IF (DEVICES(I).EQ.2) THEN
               IF (I.GT.0) CALL IGSG(I)
               CALL JDEVOF(I)
            ENDIF
            CALL JDEND(I)
            DEVICES(I) = 0
         ENDIF
   10 CONTINUE
#endif
      CALL ICLKS
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
      CALL IZEND
#endif
      CALL IGINIT(-1)
*
      END
