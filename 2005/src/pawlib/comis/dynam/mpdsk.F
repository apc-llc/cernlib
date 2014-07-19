*
* $Id: mpdsk.F,v 1.1.1.1 1996/02/26 17:16:57 mclareni Exp $
*
* $Log: mpdsk.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:57  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.12/03 16/03/92  18.41.34  by  Unknown
*-- Author : V.Berezhnoi
      SUBROUTINE MPDSK(IOP,NCH,NUMR,IBUF,NRSIZ)
      DIMENSION IBUF(NRSIZ)
      CHARACTER*40 MESS
      DATA MESS/'MPDSK: I/O ERROR IOSTAT= '/
      IF(IOP.NE.0) GO TO 1
      READ (UNIT=NCH,REC=NUMR,IOSTAT=IOST,ERR=2) IBUF
      RETURN
    1 WRITE (UNIT=NCH,REC=NUMR,IOSTAT=IOST,ERR=2) IBUF
      RETURN
C   IF THERE WAS EOF, DO NOTHING:
    2 IF(IOST.EQ.-1) RETURN
C   SYSTEM-DEPENDENT: IF READ BEYOND EOF, DO NOTHING:
#if defined(CERNLIB_VAX)
      IF(IOST.EQ.2)  RETURN
      IF(IOST.EQ.36) RETURN
#endif
#if defined(CERNLIB_IBMRT)
      IF(IOST.EQ.10) RETURN
#endif
#if defined(CERNLIB_SGI)
      IF(IOST.EQ.27) RETURN
#endif
#if defined(CERNLIB_DECS)
      IF(IOST.EQ.36) RETURN
#endif
#if defined(CERNLIB_HPUX)
      IF(IOST.EQ.922) RETURN
#endif
#if defined(CERNLIB_IPSC)
      IF(IOST.EQ.217) RETURN
#endif
#if defined(CERNLIB_APOLLO)
      IF(IOST.EQ.84148232) RETURN
      IF(IOST.EQ.0) RETURN
#endif
#if !defined(CERNLIB_IBM)
C   ELSE NOTIFY THE USER AND PAUSE:
      WRITE(MESS(25:),FMT='(I12)')IOST
      CALL MDPAUS(MESS)
#endif
      RETURN
      END
