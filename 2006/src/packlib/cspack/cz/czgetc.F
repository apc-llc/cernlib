*
* $Id: czgetc.F,v 1.1.1.1 1996/03/08 15:44:24 mclareni Exp $
*
* $Log: czgetc.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:24  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE CZGETC(NTOT,ISTAT)
*
*          To receive NTOT characters in CBUF
*
#include "cspack/czsock.inc"
#include "cspack/czbuff.inc"
      INTEGER SRECVSTR
*
#if defined(CERNLIB_VAXVMS)
      IF(IPROT.NE.0) THEN
         CALL CZDGTC(NTOT,ISTAT)
         RETURN
      ENDIF
#endif
      NMAX=NTOT
#if (!defined(CERNLIB_IBM))&&(!defined(CERNLIB_VAXVMS))
      NBYTES=SRECVSTR(ISKIN,CHBUF,NMAX)
*     NBYTES=SRECVSTR(ISKIN,ICBUF,NMAX)
#endif
#if (defined(CERNLIB_VAXVMS))&&(!defined(CERNLIB_IBM))
      NBYTES=SRECVSTR(ISKIN,CHBUF,NMAX)
#endif
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB_TCPSOCK))
      NBYTES=SRECVSTR(ISKIN,CHBUF,NMAX)
#endif
#if (defined(CERNLIB_IBM))&&(!defined(CERNLIB_TCPSOCK))
      CALL SRECV(ISKIN,CHBUF,NMAX,NBYTES)
#endif
      IF(NBYTES.LT.NTOT)THEN
         IF(LUNCZ.NE.0)WRITE(LUNCZ,*)'Problem in receiving text'
         ISTAT=NBYTES+1
      ELSE
         ISTAT=0
         ntot = nbytes
      ENDIF
#if defined(CERNLIB_DEBUG)
      IF(LUNCZ.NE.0)WRITE(LUNCZ,1103)NBYTES,CHBUF(1:80)
 1103 FORMAT(' CZGETC receiving',I10,' bytes :',/,1X,A)
#endif
*
      END
 
