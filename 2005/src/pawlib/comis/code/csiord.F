*
* $Id: csiord.F,v 1.2 2000/01/10 18:07:56 couet Exp $
*
* $Log: csiord.F,v $
* Revision 1.2  2000/01/10 18:07:56  couet
* - Mods to fix a problem of reading with format A on NT
*
* Revision 1.1.1.1  1996/02/26 17:16:24  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.12/02 12/03/92  01.07.42  by  Rene Brun
*-- Author : V.Berezhnoi
      INTEGER FUNCTION CSIORD(LUN)
#include "comis/csfmt.inc"
      CSIORD=0
      LUNIO=LUN
      IF(LUN.EQ.-1)LUNIO=5
      IBF=1
      IBFLST=1
      LBUF=132
      IF(IFMT.EQ.0)THEN
        IF(NDAREC.EQ.-1)THEN
#if !defined(CERNLIB_IBM)
          READ(LUNIO,77,IOSTAT=ISTA,ERR=111,END=222)BUF
#endif
#if defined(CERNLIB_IBM)
          BUF=' '
          READ(LUNIO,77,IOSTAT=ISTA,ERR=111,END=222)BUF(1:80)
#endif
        ELSE
          READ(LUNIO,77,REC=NDAREC,IOSTAT=ISTA,ERR=111)BUF
          NDAREC=NDAREC+1
        ENDIF
      ELSEIF(IFMT.EQ.-1)THEN
*     init unformatted input:
        IUBTOP=1
        LULIST=0
      ELSE
        CALL CSFMTC(-1)
        IF(NDAREC.EQ.-1)THEN
#if !defined(CERNLIB_IBM)
          READ(LUNIO,77,IOSTAT=ISTA,ERR=111,END=222)BUF
#endif
#if defined(CERNLIB_IBM)
          BUF=' '
          READ(LUNIO,77,IOSTAT=ISTA,ERR=111,END=222)BUF(1:80)
#endif
        ELSE
          READ(LUNIO,77,REC=NDAREC,IOSTAT=ISTA,ERR=111)BUF
          NDAREC=NDAREC+1
        ENDIF
 77       FORMAT(A)
      ENDIF
      CSIORD=1
      RETURN
 111  IFMTST=ISTA
      IF(LIOERR.EQ.-1) PRINT *,' i/o error during read'
      RETURN
 222  IF(LIOEND.EQ.-1) PRINT *,' i/o end of file during read'
      IFMTST=-1
      END
