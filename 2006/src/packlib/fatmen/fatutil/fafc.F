*
* $Id: fafc.F,v 1.1.1.1 1996/03/07 15:18:05 mclareni Exp $
*
* $Log: fafc.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:05  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FAFC(PATH,IRC)
*CMZ :          30/08/91  09.35.24  by  Jamie Shiers
*-- Author :    Jamie Shiers   30/08/91
*
*     Slave routine to FMFC. This routine is called by
*     FMSCAN for all directories that match the input
*     pattern to the FC command.
*
      CHARACTER*(*) PATH
      PARAMETER     (MAXFIL=100)
      PARAMETER     (LKEYFA=10)
      DIMENSION     MYKEYS(LKEYFA,MAXFIL)
      CHARACTER*20  FNAME,CHTEMP
#include "fatmen/fatpara.inc"
#include "fatmen/fafccm.inc"
#include "zebra/quest.inc"
#include "fatmen/fatbug.inc"
      LPATH = LENOCC(PATH)
      LF    = LENOCC(CHFILE)
      IF(IDEBFA.GE.3) PRINT *,'FAFC. enter for path = ',
     +   PATH(1:LPATH)
*
*     Bump directory count
*
      NDIRT  = NDIRT + 1
      IFIRST = 1
      ILAST  = MAXFIL
      NMAT   = 0
      NFIL   = 0
      IRC    = 0
*
*     Get number of directories below CWD (filled by FMSCAN)
*
      NDIR   = IQUEST(12)
*
*     Fast count - if CHFILE = *, just count number of files
*
      IF(CHFILE(1:LF).EQ.'*') THEN
         NFIL = IQUEST(14)
         NMAT = NFIL
         GOTO 40
      ELSE
 
   10    CONTINUE
         CALL FMKEYS(LKEYFA,MAXFIL,IFIRST,ILAST,MYKEYS,NFILES,IRET)
         NFIL = IQUEST(11)
         IF(IQUEST(1) .NE. 0) THEN
            IF(IDEBFA.GE.2) PRINT *,'FMFC. More than ',MAXFIL,' files '
     +      //'in ',PATH(1:LPATH)
            IF(IDEBFA.GE.2) PRINT *,'FMFC. IQUEST(11-12) = ',IQUEST(11)
     +      ,IQUEST(12)
         ENDIF
*
*     Process all keys returned and move those that match to KEYS
*
         NRET = IQUEST(13)
         IF(NRET.EQ.0) GOTO 40
   20    CONTINUE
         DO 30 J=1,NRET
            CALL UHTOC(MYKEYS(2,J),4,FNAME,(MKCLFA-MKFNFA)*4)
            CALL FMATCH(FNAME(1:LENOCC(FNAME)),CHFILE(1:LF),IMAT)
            IF(IMAT.NE.0) GOTO 30
            NMAT = NMAT + 1
 
   30    CONTINUE
         IF(ILAST.LT.NFIL) THEN
            IFIRST = IFIRST + MAXFIL
            ILAST = MIN(NFIL,IFIRST+MAXFIL-1)
            GOTO 10
         ENDIF
 
      ENDIF
 
   40 CONTINUE
      NFOUND = NFOUND + NFIL
      NMATCH = NMATCH + NMAT
*
*     Display only empty (zero files) directories
*
      IF((IOPTZ.NE.0).AND.(NFIL.NE.0)) GOTO 50
*
*     Display only lowest level directories
*
      IF((IOPTL.NE.0).AND.(NDIR.NE.0)) GOTO 50
 
      IF((IOPTD.NE.0).OR.(IOPTF.NE.0)) THEN
         WRITE(LWRITE,*)
         WRITE(LWRITE,*) 'Directory: ',PATH(1:LPATH)
      ENDIF
 
      IF(IOPTD.NE.0) WRITE(LWRITE,*) ' subdirectories: ',NDIR
*
*     Number of files, matches
*
      IF(IOPTF.NE.0) WRITE(LWRITE,*) ' files: ',NFIL, ' matches: ',
     +NMAT
 
   50 CONTINUE
      END
