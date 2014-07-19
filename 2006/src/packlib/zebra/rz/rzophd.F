*
* $Id: rzophd.F,v 1.1.1.1 1996/03/06 10:47:26 mclareni Exp $
*
* $Log: rzophd.F,v $
* Revision 1.1.1.1  1996/03/06 10:47:26  mclareni
* Zebra
*
*
#include "zebra/pilot.h"
      SUBROUTINE RZOPHD(LUN,JREC,IREC,IBUF,IRC)
      DIMENSION IBUF(JREC)
      IRC = 0
      WRITE(LUN,REC=IREC,ERR=99,IOSTAT=ISTAT) IBUF
      RETURN
 99   IRC = ISTAT
      END
