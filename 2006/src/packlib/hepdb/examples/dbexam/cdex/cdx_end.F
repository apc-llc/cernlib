*
* $Id: cdx_end.F,v 1.1.1.1 1996/02/28 16:23:40 mclareni Exp $
*
* $Log: cdx_end.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:40  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDX_END
*     ------------------
*                        -- Author :    Boris Khomenko   10/02/94
      IMPLICIT NONE
*
c\+CDE,CDXSTOR.  (ICDXDIV)
*
#include "cdxluns.inc"
*
#include "cdxfils.inc"
*
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC
CC+KEEP,CDXFILS.
CC*
CC      INTEGER MCDXFIL
CC      PARAMETER (MCDXFIL = 10)
CC      COMMON /CDXFILS/ NCDXFIL,KCDXFIL,
CC     +                 ICXFSTA(10),CDXFPRF(10),CDXFTOP(10),CDXFNAM(10)
CC      INTEGER   NCDXFIL, KCDXFIL, ICXFSTA
CC      CHARACTER CDXFPRF*2, CDXFTOP*8, CDXFNAM*40
CC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
*
*  -- externals --
*
*  -- Local variables --
*
      INTEGER   IRC
      INTEGER IFOPN , IFDCL
      INTEGER   LR , JJ, K
      CHARACTER MESL*78
      INTEGER   LMES
*
*  -- Check everything is closed in the CDXFILS table --
        IFOPN=0
        IFDCL=0
        DO K=1,NCDXFIL
          IF(ICXFSTA(K).NE.0.OR.CDXFPRF(K).NE.' ') IFDCL=IFDCL+1
          IF(ICXFSTA(K).GT.0) THEN
            IFOPN=IFOPN+1
          ENDIF
        ENDDO
  111   CONTINUE
        MESL='>>End CDEX:   /   DB files opened/declared<<'
        JJ=INDEX(MESL,'/')
        CALL ENCODI(IFOPN, 1,MESL(JJ-2:JJ-1),LR)
        CALL ENCODI(IFDCL,-1,MESL(JJ+1:JJ+2),LR)
        LMES=INDEX(MESL,'<<')+1
        CALL CDX_MESS(MESL(:LMES))
        IF(IFOPN.GT.0) THEN
          CALL CDX_MESS('>>CDEND(,''A'',) automatic call<<')
          CALL CDEND(' ','A',IRC)
          CALL UXIRCM('CDEND ',IRC)
        ELSE
          CALL CDX_MESS('>>No call to CDEND<<')
        ENDIF
      END
