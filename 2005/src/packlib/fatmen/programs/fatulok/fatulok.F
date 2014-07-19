*
* $Id: fatulok.F,v 1.1.1.1 1996/03/07 15:17:42 mclareni Exp $
*
* $Log: fatulok.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:42  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      PROGRAM FATULOK
*CMZ :          18/02/91  14.49.14  by  Jamie Shiers
*-- Author :    Jamie Shiers   18/02/91
      COMMON/QUEST/IQUEST(100)
      COMMON/PAWC/PAW(50000)
      CHARACTER*80   FATFIL
      CALL HLIMIT(50000)
      LRECL  = 0
      FATFIL = 'CERN.FATRZ'
      CALL RZOPEN(1,'//FAT',FATFIL,'U',
     +            LRECL,ISTAT)
      PRINT *,'FATULOK. return from RZOPEN with LRECL/ISTAT = ',
     +        LRECL,ISTAT
      CALL RZFILE(1,'//FAT','UDL')
      PRINT *,'FATULOK. return code ',IQUEST(1),' from RZFILE'
      CALL RZEND('FAT')
      END
