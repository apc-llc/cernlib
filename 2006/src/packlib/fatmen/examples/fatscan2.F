*
* $Id: fatscan2.F,v 1.1.1.1 1996/03/07 15:18:00 mclareni Exp $
*
* $Log: fatscan2.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:00  mclareni
* Fatmen
*
*
*------------------------------------------------------------------------
*
* Print all directory names & # files in each directory
* Useful for comparing two FATMEN catalogues for missing
* entries. The missing entries can be added using the
* FATOUCH program.
*------------------------------------------------------------------------
      PARAMETER (LURCOR=200000)
      COMMON/CRZT/IXSTOR,IXDIV,IFENCE(2),LEV,LEVIN,BLVECT(LURCOR)
      DIMENSION    LQ(999),IQ(999),Q(999)
      EQUIVALENCE (IQ(1),Q(1),LQ(9)),(LQ(1),LEV)
*
      COMMON /USRLNK/LUSRK1,LUSRBK,LUSRLS
 
      CHARACTER*255 CHFILE
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
*
*     Log file
*
      CHFILE = CHGRP(1:LGRP)//'.SCANLOG'
 
      LFILE = LGRP + 8
 
      CALL CUTOL(CHFILE(1:LFILE))
*
*     Remove existing log file, if any
*
      CALL UNLINKF(CHFILE(1:LFILE))
*
*     Open log file
*
      CALL FAFILE(3,CHFILE(1:LFILE),IRC)
      IF(IRC.NE.0) THEN
         PRINT *,'Cannot open log file'
         STOP
      ELSE
         PRINT *,'Logfile is ',CHGRP(1:LGRP)//'.SCANLOG'
      ENDIF
 
      CALL FMSCAN('//'//CHSYS(1:LSYS)//'/'//CHGRP(1:LGRP)//'/*',
     +            99,UROUT,IRC)
*
*     Close log file
*
      CLOSE(3)
      END
 
      SUBROUTINE UROUT(PATH,IRC)
      CHARACTER*(*) PATH
      COMMON/QUEST/IQUEST(100)
      WRITE(3,9001) PATH(1:LENOCC(PATH)),IQUEST(14)
9001  FORMAT(A64,I5)
      IRC = 0
      END
