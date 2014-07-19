*
* $Id: igpid.F,v 1.1.1.1 1996/02/14 13:10:37 mclareni Exp $
*
* $Log: igpid.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:37  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.16/06 29/10/92  14.01.00  by  O.Couet
*-- Author :    O.Couet   21/05/92
      SUBROUTINE IGPID(ILEVI,CHPID,IPID,CHOPT)
*.===========>
*.
*.  This routine allows to put primitives identifiers
*. in the HIGZ data structure.
*.
*. _Input parameters:
*.
*.  INTEGER ILEVI   : Level number
*.  CHARACTER CHPID : Primitives name
*.  INTEGER IPID    : Primitives identifier
*.  CHARACTER CHOPT : Option
*.                    CHOPT=' ' the level becomes LEVEL
*.                    CHOPT='U' one level Up
*.                    CHOPT='D' one level Down
*.                    CHOPT='S' Same level
*.
*..==========> (O.Couet)
#if defined(CERNLIB_ZEBRA)
#include "higz/hipaw.inc"
#endif
#if defined(CERNLIB_ZEBRA)
#include "higz/hicode.inc"
#endif
#if defined(CERNLIB_ZEBRA)
#include "higz/hiflag.inc"
#endif
#if defined(CERNLIB_ZEBRA)
#include "higz/hipick.inc"
#endif
#if defined(CERNLIB_ZEBRA)
      CHARACTER*(*) CHPID,CHOPT
      DIMENSION IOPT(3)
      EQUIVALENCE (IOPTU,IOPT(1)),(IOPTD,IOPT(2))
      EQUIVALENCE (IOPTS,IOPT(3))
*.______________________________________
*
      IF(.NOT.ZFLAG)RETURN
      CALL UOPTC(CHOPT,'UDS',IOPT)
*
      ILEV               = ILEVI
      IF(IOPTU.NE.0)ILEV = INBLEV-1
      IF(IOPTD.NE.0)ILEV = INBLEV+1
      IF(IOPTS.NE.0)ILEV = INBLEV
*
      IF(LPICT.LT.0)RETURN
      IF(ILEV.GT.INBLEV+1)THEN
         CALL IGERR('Invalid level number','IGPID')
         RETURN
      ENDIF
      INBLEV     = ILEV
      IQ(LHNT+5) = INBLEV
      ILEN       = MIN(LENOCC(CHPID),LEVLEN)
      IF(ILEN.GT.0)THEN
         IF(IZPUSH(3,0,LEVLEN/2,'IGPID').NE.0)RETURN
         CALL IZSTCC(IPICO1,INTPTR)
         IQ(LHI+INTPTR)   = ILEV
         IQ(LHI+INTPTR+1) = IPID
         IQ(LHI+INTPTR+2) = ISTPTR
         CALL IZINCI(3)
         CHLVL            = ' '
         CHLVL(1:ILEN)    = CHPID(1:ILEN)
         CALL UCTOH(CHLVL,IQ(LHC+ISTPTR),4,LEVLEN)
         CALL IZINCS(LEVLEN/2)
      ELSE
         IF(IZPUSH(2,0,0,'IGPID').NE.0)RETURN
         CALL IZSTCC(IPICO2,INTPTR)
         IQ(LHI+INTPTR)   = ILEV
         IQ(LHI+INTPTR+1) = IPID
         CALL IZINCI(2)
      ENDIF
#endif
*
      END
