*
* $Id: putp.F,v 1.1.1.1 1996/03/08 15:44:19 mclareni Exp $
*
* $Log: putp.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:19  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE PUTP
*
*         Action routine for PUTP command
*
#include "cspack/czsock.inc"
      CHARACTER*80 REMOTE,LOCAL
      CHARACTER*4  CHOPT
*
#include "cspack/czopen.inc"
      CALL KUGETS(LOCAL,NCHL)
      CALL KUGETS(REMOTE,NCHR)
      IF(NCHR.LE.0)REMOTE=LOCAL
      IF(REMOTE(1:NCHR).EQ.'=') REMOTE=LOCAL
      NCHR = LENOCC(REMOTE)
      CALL KUGETC(CHOPT,NCH)
#include "cspack/czfold.inc"
#if defined(CERNLIB_CRAY)
      PRINT *,'PUTP is not yet available for the Cray...'
      PRINT *,'You may transfer PAM files '
      PRINT *,'          in CARD format using GET/PUTA'
      PRINT *,'          in CETA format using GET/PUTB'
      PRINT *,'          in CMZ  format using GET/PUTRZ'
      RETURN
#endif
#if !defined(CERNLIB_CRAY)
      CALL XZPUTP(LOCAL,REMOTE,CHOPT,IRC)
#endif
99    END
 
