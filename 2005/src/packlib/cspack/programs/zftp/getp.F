*
* $Id: getp.F,v 1.1.1.1 1996/03/08 15:44:19 mclareni Exp $
*
* $Log: getp.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:19  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE GETP
*
*         Action routine for GETP command
*
#include "cspack/czsock.inc"
      CHARACTER*80 REMOTE,LOCAL
      CHARACTER*4  CHOPT
*
#include "cspack/czopen.inc"
      CALL KUGETS(REMOTE,NCHR)
      CALL KUGETS(LOCAL,NCHL)
      IF(NCHL.LE.0)LOCAL=REMOTE
      IF(LOCAL(1:NCHL).EQ.'=') LOCAL=REMOTE
      NCHL = LENOCC(LOCAL)
      CALL KUGETC(CHOPT,NCH)
#include "cspack/czfold.inc"
#if defined(CERNLIB_CRAY)
      PRINT *,'GETP is not yet available for the Cray...'
      PRINT *,'You may transfer PAM files '
      PRINT *,'          in CARD format using GET/PUTA'
      PRINT *,'          in CETA format using GET/PUTB'
      PRINT *,'          in CMZ  format using GET/PUTRZ'
      RETURN
#endif
#if !defined(CERNLIB_CRAY)
      CALL XZGETP(LOCAL,REMOTE,CHOPT,IRC)
#endif
99    END
 
