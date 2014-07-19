*
* $Id: pzgeta.F,v 1.1.1.1 1996/03/01 11:38:47 mclareni Exp $
*
* $Log: pzgeta.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:47  mclareni
* Paw
*
*
#include "paw/pilot.h"
#if defined(CERNLIB_CZ)
*CMZ :  2.03/01 09/06/93  15.43.37  by  Alfred Nathaniel
*-- Author :    Alfred Nathaniel   03/06/93
      SUBROUTINE PZGETA(CHSRC,CHTRG,LRECL,ISTAT)
      CHARACTER*(*) CHSRC,CHTRG
*
* file transfer between client and Piaf server
*
#include "hbook/hcpiaf.inc"

      INTEGER PZXFER

      ISTAT=PZXFER(CHSRC,CHTRG,'GA',CLINPF,LRECL)
      GOTO 1

      ENTRY PZGETB(CHSRC,CHTRG,LRECL,ISTAT)
      ISTAT=PZXFER(CHSRC,CHTRG,'GB',CLINPF,LRECL)
      GOTO 1

      ENTRY PZPUTA(CHSRC,CHTRG,LRECL,ISTAT)
      ISTAT=PZXFER(CHSRC,CHTRG,'PA',CLINPF,LRECL)
      GOTO 1

      ENTRY PZPUTB(CHSRC,CHTRG,LRECL,ISTAT)
      ISTAT=PZXFER(CHSRC,CHTRG,'PB',CLINPF,LRECL)
      GOTO 1

 1    CONTINUE
      IF(ISTAT.LT.0) RETURN
      CALL PFLOOP(ISTAT)
      END
#endif
