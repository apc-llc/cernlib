*
* $Id: fatloop2.F,v 1.1.1.1 1996/03/07 15:18:00 mclareni Exp $
*
* $Log: fatloop2.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:00  mclareni
* Fatmen
*
*
      PROGRAM FATLOOP2
      PARAMETER (LURCOR=200000)
      COMMON/CRZT/IXSTOR,IXDIV,IFENCE(2),LEV,LEVIN,BLVECT(LURCOR)
      DIMENSION    LQ(999),IQ(999),Q(999)
      EQUIVALENCE (IQ(1),Q(1),LQ(9)),(LQ(1),LEV)
*
      COMMON/USRLNK/LUSRK1,LUSRBK,LUSRLS
      COMMON/FLOOP2/NFILES
*
#include "fatmen/quest.inc"
      PARAMETER (MAXDIR=100)
      CHARACTER*255 CHDIR(MAXDIR)
#include "fatmen/fatpara.inc"
#include "fatmen/fatbug.inc"
      CHARACTER*16  CHSYS,CHGRP
      CHARACTER*8   CHLOG
#include "fatmen/slate.inc"
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
 
      NFILES = 0
*
*     Units for FATMEN RZ/FZ files
*
      LUNRZ  = 1
      LUNFZ  = 2
 
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
 
      CALL GETENVF('FMLOGL',CHLOG)
      IF(IS(1).EQ.0) THEN
         LOGLV  = 0
      ELSE
         LOGLV  = ICDECI(CHLOG,1,IS(1))
      ENDIF
*
*     Initialise FATMEN
*
      CALL FMINIT(IXSTOR,LUNRZ,LUNFZ,
     +     '//'//CHSYS(1:LSYS)//'/'//CHGRP(1:LGRP),IRC)
 
      CALL FMLOGL(LOGLV)
 
      CALL FMLOOP('//'//CHSYS(1:LSYS)//'/'//CHGRP(1:LGRP)//'*/*',
     +            -1,UROUT,IRC)
 
      IF(IDEBFA.GE.0) PRINT *,'FATLOOP2. # files  = ',NFILES
 
      END
 
      SUBROUTINE UROUT(GENAM,KEYS,IRC)
      CHARACTER*(*) GENAM
      PARAMETER (LURCOR=200000)
      COMMON/CRZT/IXSTOR,IXDIV,IFENCE(2),LEV,LEVIN,BLVECT(LURCOR)
      DIMENSION    LQ(999),IQ(999),Q(999)
      EQUIVALENCE (IQ(1),Q(1),LQ(9)),(LQ(1),LEV)
      PARAMETER     (LKEYFA=10)
      DIMENSION     KEYS(LKEYFA)
#include "fatmen/fatbug.inc"
#include "fatmen/quest.inc"
      COMMON/FLOOP2/NFILES
      IRC   = 0
      LGN   = LENOCC(GENAM)
      LBANK = 0
      IF(IDEBFA.GT.0) PRINT *,'Get: ',GENAM(1:LGN)
      CALL FMGETK(GENAM(1:LGN),LBANK,KEYS,IRC)
      IF(IRC.EQ.0) THEN
         NFILES = NFILES + 1
         CALL MZDROP(IXSTOR,LBANK,' ')
      ENDIF
      END
