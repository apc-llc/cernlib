*
* $Id: fmbtof.F,v 1.1.1.1 1996/03/07 15:17:36 mclareni Exp $
*
* $Log: fmbtof.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:36  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMBTOF(GENAME,IRC)
      CHARACTER*(*) GENAME
#include "fatmen/slate.inc"
#include "fatmen/fatbug.inc"
      PARAMETER (LURCOR=250000)
      COMMON/CRZT/IXSTOR,IXDIV,IFENCE(2),LEV,LEVIN,BLVECT(LURCOR)
      DIMENSION    LQ(999),IQ(999),Q(999)
      EQUIVALENCE (IQ(1),Q(1),LQ(9)),(LQ(1),LEV)
*
      COMMON/USRLNK/LSRCBK,LDSTBK
      CHARACTER*20  FNAME
      CHARACTER*255 CHDIR,CHNAME,CHFILE
      CHARACTER*20  CHGRP
      DIMENSION     IUHEAD(80),IOCH(80)
      LOGICAL       EXIST
      PARAMETER     (NW=80)
      PARAMETER     (IEV=1)
      PARAMETER     (LUNOUT=9)
 
      IRC    = 0
      LGN    = LENOCC(GENAME)
*
*     Get the FATMEN group name
*
      ISTART = INDEX(GENAME(3:),'/') + 3
      IEND   = INDEX(GENAME(ISTART:),'/') + ISTART - 2
 
      CHGRP  = 'FM' // GENAME(ISTART:IEND)
      LGRP   = IEND - ISTART + 3
 
      IF(IDEBFA.GE.3) PRINT *,'FMBTOF. enter for ',GENAME(1:LGN),
     +   ' ',CHGRP(1:LGRP)
*
*     Find directory where updates should go
*
      CALL GETENVF(CHGRP(1:LGRP),CHDIR)
      LDIR   = IS(1)
 
      IF(LDIR.EQ.0) THEN
         PRINT *,'FMBTOF. error - cannot find update directory for ',
     +      CHGRP(1:LGRP)
         IRC = -1
         RETURN
      ELSE
         IF(IDEBFA.GE.3) PRINT *,'FMBTOF. update directory is ',
     +      CHDIR(1:LDIR)
      ENDIF
*
*     Set up descriptor of header vector
*
      CALL MZIOCH(IOCH,NW,'70H 10I')
*
*     Fill header vector
*
      CALL UCTOH('MOD ',IUHEAD,4,4)
*
*     Fill with blanks for safety
*
      CALL VBLANK(IUHEAD(2),69)
 
      CALL UCTOH(GENAME,IUHEAD(2),4,LGN)
*
*     Keys
*
      LEND  = INDEXB(GENAME,'/') + 1
      FNAME = GENAME(LEND:LGN)
      IUHEAD(71) = 0
      IUHEAD(77) = IQ(LDSTBK+MCPLFA+KOFUFA)
      IUHEAD(78) = IQ(LDSTBK+MMTPFA+KOFUFA)
      IUHEAD(79) = IQ(LDSTBK+MLOCFA+KOFUFA)
      IUHEAD(80) = LKEYFA
      LENFN = LGN-LEND+1
*
*     IUHEAD 71-80 contains the keys, which includes the CHFILEame
*
      DO 2 I=LENFN+1,20
2     FNAME(I:I) = ' '
      CALL UCTOH(FNAME,IUHEAD(72),4,20)
*
*     Get a unique file name
*
300   CONTINUE
      CALL FMFNME(CHNAME)
      CALL CUTOL(CHNAME)
 
      CHFILE = CHDIR(1:LDIR) // '/todo/'
     +// CHNAME(1:LENOCC(CHNAME))
      LENF  = LENOCC(CHFILE)
      IF(IDEBFA.GE.3) PRINT *,'FMBTOF. assigned temporary file ',
     +   CHFILE(1:LENF)
      INQUIRE(FILE=CHFILE(1:LENF),EXIST=EXIST)
      IF(EXIST) THEN
         IF(IDEBFA.GE.3) PRINT *,'FMBTOF. file already exists - ',
     +      'sleeping for 1 second'
         CALL SLEEPF(1)
         GOTO 300
      ENDIF
 
      OPEN(LUNOUT,STATUS='NEW',FILE=CHFILE(1:LENF),ERR=998)
 
      CALL FZFILE(LUNOUT,0,'AO')
      CALL FZLOGL(LUNOUT,MAX(IDEBFA-1,-3))
      CALL FZOUT(LUNOUT,IXSTOR,LDSTBK,IEV,'S',IOCH,NW,IUHEAD)
      CALL FZENDO(LUNOUT,'T')
      CLOSE (LUNOUT)
      IF(IDEBFA.GE.0) PRINT *,'FMBTOF - update queued for processing',
     +   ' (MOD ',GENAME(1:LGN),')'
      RETURN
*
998   CONTINUE
      PRINT *,'FMBTOF - error opening temporary file, name=',
     + CHFILE(1:LENF)
      CLOSE (LUNOUT)
      END
