*
* $Id: sppiaf.F,v 1.2 1996/04/11 14:54:13 cernlib Exp $
*
* $Log: sppiaf.F,v $
* Revision 1.2  1996/04/11 14:54:13  cernlib
* Zserv/pawserv used to be build from two patches; these were put both into this
* directory.
* The #includes in all files copied from the other directory had to updated.
*
* Revision 1.1.1.1  1996/03/08 15:44:21  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
#if defined(CERNLIB_UNIX)
      SUBROUTINE SPPIAF(CHF)
*
*     Transfer a RZ file from the client
*
#include "hbook/hcmail.inc"
#include "cspack/czunit.inc"
      COMMON/QUEST/IQUEST(100)
#include "cspack/pawc.inc"
      DIMENSION IQ(2),Q(2),LQ(8000)
      EQUIVALENCE (LQ(1),LMAIN),(IQ(1),LQ(9)),(Q(1),IQ(1))
      CHARACTER*(*) CHF
      CHARACTER*80 LOCAL
      CHARACTER*20 CHOPT
      CHARACTER*20 CHUSER,CHGRP
      INTEGER      SYSTEMF
*
*     Get file parameters from client
*
      READ(CHF,'(I6)') ISIZE
 
      LEND = LENOCC(CHF)
      LBLA = INDEX(CHF(7:LEND),' ')
      IF(LBLA.EQ.0) THEN
         LCHF   = LEND
         CHOPT  = ' '
      ELSE
         CHOPT  = CHF(LBLA+7:LEND)
         LCHF   = LBLA + 5
      ENDIF
 
      LOCAL  = CHF(7:LCHF)
      LLOCAL = LCHF - 6
 
*
*     Get group and usernames
*
      ISLASH = INDEX(LOCAL,'/')
      CHGRP  = LOCAL(2:ISLASH)
      LGRP   = ISLASH - 2
 
      JSLASH = INDEX(LOCAL(ISLASH+1:),'/')
      CHUSER = LOCAL(ISLASH+1:ISLASH+JSLASH-1)
      LUSER  = JSLASH - 1
 
      LPATH  = INDEXB(LOCAL(1:LLOCAL),'/')
*
*     Issue SFGET to create local file and assign symbolic link
*
      CALL SSFGET(CHGRP(1:LGRP),CHUSER(1:LUSER),
     +            LOCAL(1:LPATH-1),LOCAL(LPATH+1:LLOCAL),
     +            IMODE,ISIZE,CHOPT,IRC)
      IF(IRC.NE.0) THEN
         WRITE(CHMAIL,9001) IRC
9001  FORMAT('3Cannot open remote file - return code ',
     +       I6,' from SFGET')
         CALL CZPUTA(CHMAIL,ISTAT)
         GOTO 99
      ELSE
         CALL CZPUTA('0OK',ISTAT)
      ENDIF
 
  99  END
#endif
