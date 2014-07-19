*
* $Id: fmrmdr.F,v 1.1.1.1 1996/03/07 15:18:11 mclareni Exp $
*
* $Log: fmrmdr.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:11  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMRMDR(GENAME,IRC)
#include "fatmen/faust.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fatbank.inc"
      CHARACTER*4 COMM
      CHARACTER*(*) GENAME
      CHARACTER*20  SUBDIR
      PARAMETER   (LKEYFA=10)
      DIMENSION   KEYS(LKEYFA)
 
      NFRDIR = NFRDIR + 1
 
      IRC    = 0
*
*     Set current directory to the one to be deleted
*
      LGN = LENOCC(GENAME)
      CALL FACDIR(GENAME(1:LGN),' ')
*
*     Check that directory is empty
*
      CALL RZRDIR(1,SUBDIR,NDIR)
      IF (NDIR .GT. 0) THEN
         WRITE(LPRTFA,9001) GENAME(1:LGN),IQUEST(11)
9001  FORMAT(' FMRMDR. Cannot delete directory ',A,' - ',
     +       ' # of subdirectories = ',I10)
         IRC = 99
         GOTO 99
      ENDIF
*
*     Get keys from current directory
*
      CALL RZKEYS(LKEYFA,1,KEYS,NKEYS)
      IF (NKEYS .GT. 0) THEN
         WRITE(LPRTFA,9002) GENAME(1:LGN),NKEYS
9002  FORMAT(' FMRMDR. Cannot delete directory ',A,' - ',
     +       ' # of files = ',I10)
         IRC = 99
         GOTO 99
      ENDIF
      L    = 0
      CALL FMVERI(GENAME(1:LGN),L,KEYS,'G',IRC)
      IF(IRC.NE.0) THEN
         IF(IDEBFA.GT.-3) PRINT 9003
9003  FORMAT(' FMRMDR. errors detected by FMVERI -',
     +       ' update will not be processed')
         RETURN
      ENDIF
 
      COMM = 'DDIR'
      CALL FMFZO(COMM,GENAME,L,KEYS,IRC)
   99 CONTINUE
*
*     Reset current directory
*
      CALL FACDIR(GENAME(1:LGN),' ')
 
      RETURN
      END
