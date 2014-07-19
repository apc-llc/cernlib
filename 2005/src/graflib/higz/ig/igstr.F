*
* $Id: igstr.F,v 1.1.1.1 1996/02/14 13:10:39 mclareni Exp $
*
* $Log: igstr.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:39  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.14/00 05/02/92  18.55.08  by  O.Couet
*-- Author :    O.Couet   07/01/92
      SUBROUTINE IGSTR(ISTID,CHOPT)
*.===========>
*.
*.  This routine allows to manage PHIGS structures. If HIGZ
*. is build without PHIGS, it just do nothing.
*.
*. _Input parameters:
*.
*.  INTEGER IST : Structure identifier
*.  CHARACTER CHOPT : Option
*.
*.      'O' open structure ISTID
*.      'N' open structure with name set
*.      'C' close structure
*.      'E' append structure ISTID in the current opened structure
*.      'D' delete all structures
*.      'P' post the structure on the active workstations
*.
*..==========> (O.Couet)
      CHARACTER*(*) CHOPT
#if defined(CERNLIB_PHIGS)
#include "higz/hiatt.inc"
#include "higz/hiphig.inc"
      DIMENSION IOPT(6)
      EQUIVALENCE (IOPTO,IOPT(1)),(IOPTN,IOPT(2))
      EQUIVALENCE (IOPTC,IOPT(3)),(IOPTE,IOPT(4))
      EQUIVALENCE (IOPTD,IOPT(5)),(IOPTP,IOPT(6))
      COMMON /NAMESET/ NSNO
*.______________________________________
*
      CALL UOPTC(CHOPT,'ONCEDP',IOPT)
*
      IF(IDIM.NE.3)RETURN
*
*              Open a new structure ISTID
*
      IF(IOPTO.NE.0)THEN
         CALL POPST(ISTID)
         LOSTOP=.TRUE.
      ENDIF
*
*              Close the current opened structure (ISTID not used)
*
      IF(IOPTC.NE.0)THEN
         CALL PCLST
         LOSTOP=.FALSE.
      ENDIF
*
*              Extend the current opened structure with the stucture ISTID
*
      IF (IOPTE.NE.0) THEN
         CALL PEXST(ISTID)
      ENDIF
*
*              Open structure with name set
*
      IF (IOPTN.NE.0) THEN
         CALL POPST(ISTID)
         CALL PADS(1,NSNO)
         NSNO=NSNO+1
         LOSTOP=.TRUE.
      ENDIF
*
*              Post structure
*
      IF(IOPTP.NE.0)THEN
         DO 10 I=1,INOPWK
            IF(IGIWTY(ILOPWK(I)).GT.0.AND.ACWKFL(I))THEN
               CALL PPOST(ILOPWK(I),ISTID,1.)
            ENDIF
   10    CONTINUE
      ENDIF
*
*              Delete all structures
*
      IF(IOPTD.NE.0)THEN
         CALL PDAS
         LOSTEX=.FALSE.
      ENDIF
*
#endif
      END
