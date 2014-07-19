*
* $Id: fmlist.F,v 1.1.1.1 1996/03/07 15:18:05 mclareni Exp $
*
* $Log: fmlist.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:05  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMLIST(CHPATH,FILES,KEYS,NFOUND,MAXFIL,IRC)
*
*     Subroutine to return all generic names matching input
*     generic name pattern, which may include wild cards.
*     N.B. CHPATH is a pathname not a file name!
*
#include "fatmen/fatbank.inc"
      PARAMETER     (LKEYFA=10)
      PARAMETER     (NLEVEL=100)
      PARAMETER     (MAXDIR=1000)
      CHARACTER*(*) CHPATH,FILES(MAXFIL)
      COMMON/FALIST/FNAMES,SUBDIR
      CHARACTER*20  FNAMES(MAXDIR),FNAME
      CHARACTER*255 SUBDIR(MAXDIR)
      DIMENSION     KEYS(LKEYFA,MAXFIL)
*
      NFOUND = 0
      IRC    = 0
*
*     Find first wild card in generic name
*
      LPATH  = LENOCC(CHPATH)
      IF(IDEBFA.GE.3)
     +PRINT *,'FMLIST. enter for PATH = ',CHPATH(1:LPATH)
      IWILD  = ICFMUL('*%(',CHPATH,1,LPATH)
 
      IF(IWILD.GT.LPATH) THEN
         CALL FMFNMS(CHPATH(1:LPATH),FILES,KEYS,NFOUND,MAXFIL,IRC)
         IF(IRC.NE.0.AND.IDEBFA.GT.-3)
     +   PRINT *,'FMLIST. Return code ',IRC,' from FMFNMS'
         DO 1 I=1,NFOUND
         FNAME    = FILES(I)(1:LENOCC(FILES(I)))
1        FILES(I) = CHPATH(1:LPATH)//'/'//FNAME(1:LENOCC(FNAME))
         RETURN
         ENDIF
 
      ISLASH = INDEXB(CHPATH(1:IWILD),'/')
*
*     Find subdirectories below first branch
*
      CALL FMTREE(CHPATH(1:ISLASH-1),SUBDIR,NLEVEL,NDIR,MAXDIR,IRC)
      IF(IRC.NE.0.AND.IDEBFA.GT.-3)
     +PRINT *,'FMLIST. Return code ',IRC,' from FMTREE'
      IF(IRC.EQ.0.AND.IDEBFA.GE.3)
     +PRINT *,'FMLIST. ',NDIR,' directories found below ',
     +CHPATH(1:ISLASH-1)
*
*     Process each subdirectory in turn
*
      NFOUND = 0
      DO 10 I=1,NDIR
*
*     Match against input pattern
*
      LDIR = LENOCC(SUBDIR(I))
      CALL FMATCH(SUBDIR(I)(1:LDIR),CHPATH(1:LPATH),IMAT)
      IF(IMAT.NE.0) GOTO 10
*
*     Get files in current directory
*
      NFILES = 0
      IF(NFOUND.LT.MAXFIL) THEN
      CALL FMFNMS(SUBDIR(I)(1:LDIR),FNAMES,KEYS(1,NFOUND+1),NFILES,
     +            MAXFIL-NFOUND,IRC)
      IF(IRC.NE.0.AND.IDEBFA.GT.-3)
     +PRINT *,'FMLIST. Return code ',IRC,' from FMFNMS'
 
      IF(IDEBFA.GE.3) PRINT *,'FMLIST. ',NFILES,' files found in ',
     +                         SUBDIR(I)(1:LDIR)
      DO 20 J=1,NFILES
20    FILES(NFOUND+J) = SUBDIR(I)(1:LDIR)//'/'//FNAMES(J)
      NFOUND = NFOUND + NFILES
      ELSE
      IF(IDEBFA.GE.1) PRINT *,'FMLIST. maximum number of files reached'
      IRC = -1
      RETURN
      ENDIF
10    CONTINUE
      END
 
