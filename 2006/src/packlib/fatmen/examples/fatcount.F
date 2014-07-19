*
* $Id: fatcount.F,v 1.1.1.1 1996/03/07 15:18:01 mclareni Exp $
*
* $Log: fatcount.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:01  mclareni
* Fatmen
*
*
      PROGRAM FATCOUNT
*------------------------------------------------------------------------
* Count files & directories
*------------------------------------------------------------------------
      PARAMETER (LURCOR=200000)
      COMMON/CRZT/IXSTOR,IXDIV,IFENCE(2),LEV,LEVIN,BLVECT(LURCOR)
      DIMENSION    LQ(999),IQ(999),Q(999)
      EQUIVALENCE (IQ(1),Q(1),LQ(9)),(LQ(1),LEV)
*
      COMMON /USRLNK/LUSRK1,LUSRBK,LUSRLS
*
#include "fatmen/quest.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fatbug.inc"
#include "fatmen/slate.inc"
*
      PARAMETER (MAXDIR=100)
      CHARACTER*255 CHDIR(MAXDIR)
      CHARACTER*8   DSN
      CHARACTER*16  CHSYS,CHGRP
      EXTERNAL      UROUT
*
*
*     Initialise ZEBRA
*
      CALL MZEBRA(-3)
      CALL MZSTOR(IXSTOR,'/CRZT/','Q',IFENCE,LEV,BLVECT(1),BLVECT(1),
     +            BLVECT(5000),BLVECT(LURCOR))
      CALL MZLOGL(IXSTOR,-3)
 
*
* *** Define user division and link area like:
*
      CALL MZDIV  (IXSTOR, IXDIV, 'USERS', 50000, LURCOR, 'L')
      CALL MZLINK (IXSTOR, '/USRLNK/', LUSRK1, LUSRLS, LUSRK1)
*
*     Units for FATMEN RZ/FZ files
*
      LUNRZ = 1
      LUNFZ = 2
*
*     Get FATMEN system & group names
*
      CALL GETENVF('FATSYS',CHSYS)
      IF(IS(1).EQ.0) THEN
         PRINT *,'FATMEN system defaulted to CERN'
         CHSYS = 'CERN'
         LSYS  = 4
      ELSE
         LSYS  = IS(1)
         PRINT *,'FATMEN system: ',CHSYS(1:LSYS)
      ENDIF
 
      CALL GETENVF('FATGRP',CHGRP)
      IF(IS(1).EQ.0) THEN
         PRINT *,'FATMEN group not defined'
         STOP
      ELSE
         LGRP   = IS(1)
         PRINT *,'FATMEN group:  ',CHGRP(1:LGRP)
      ENDIF
*
*     Initialise FATMEN
*
      CALL FMINIT(IXSTOR,LUNRZ,LUNFZ,
     +     '//'//CHSYS(1:LSYS)//'/'//CHGRP(1:LGRP),IRC)
 
      CALL FMFILC('//'//CHSYS(1:LSYS)//'/'//CHGRP(1:LGRP)//'*/',
     +            NFILES,IRC)
 
      WRITE(6,9001) IQUEST(12),IQUEST(13),
     +              '//'//CHSYS(1:LSYS)//'/'//CHGRP(1:LGRP)
 
9001  FORMAT(' FATCOUNT. total of ',I6,' files in ',I6,
     +       ' directories for ',A)
      END
