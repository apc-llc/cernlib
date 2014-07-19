*
* $Id: cdldc.F,v 1.2 1996/03/12 13:08:43 cernlib Exp $
*
* $Log: cdldc.F,v $
* Revision 1.2  1996/03/12 13:08:43  cernlib
* Build hepdb programs: hepdb, cdserv, cdnew, and cdmove
*
* Revision 1.1.1.1  1996/02/28 16:23:29  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDLDC
#include "hepdb/cdunit.inc"
      CHARACTER*255 CHPATH,PATH,CHDIR
      CHARACTER*12  CHOPT
#include "hepdb/quest.inc"
#include "hepdb/cdopen.inc"
*
*     List contents of current directory
*        Options: C - creation date & time
*                 M - modification date & time
*                 O - number of objects
*                 R - list subdirectories recursively
*                 S - number of subdirectories
*                 T - tags
*
      CHOPT = ' '
      CALL KUGETC(PATH,LPATH)
      CALL CDFIXF(PATH,CHPATH)
      LPATH = LENOCC(CHPATH)
      CALL KUGETI(NLEVEL)
      CALL KUGETC(CHOPT,LCHOPT)
 
*     Save current directory
*
      CALL RZCDIR(CHDIR,'R')
      LCHDIR = LENOCC(CHDIR)
 
      IF(LCHOPT.EQ.0) THEN
         LCHOPT = 1
         CHOPT  = ' '
      ENDIF
*
      IF(INDEX(CHOPT(1:LCHOPT),'H').NE.0) THEN
         WRITE(LOUTCD,*) '>>> Command: LD ',
     +   'Options: ',CHOPT(1:LCHOPT)
         WRITE(LOUTCD,*) '>>> Path: ',CHPATH(1:LPATH)
         WRITE(LOUTCD,*) '>>> Current directory: ',CHDIR(1:LCHDIR)
      ENDIF
*
*     Turn on option R if NLEVEL > 1
*
      IF(NLEVEL.GT.1) CHOPT = 'R'
      IF(INDEX(CHOPT,'R').EQ.0) THEN
         IWILD = ICFMUL('*%()<>',CHPATH,1,LPATH)
         IF(IWILD.LE.LPATH) THEN
            ISLASH = INDEXB(CHPATH(1:IWILD),'/') + 1
*
*     Turn on option R if more than one level of wild-cards
*
            IF(INDEX(CHPATH(ISLASH:LPATH),'/').NE.0) THEN
               LCH = LENOCC(CHOPT)
               LCH = LCH + 1
               CHOPT(LCH:LCH) = 'R'
            ENDIF
 
         ENDIF
      ELSE
*
*     Set maximum number of levels if -R and NLEVEL=1 (default)
*
         IF(NLEVEL.EQ.1) NLEVEL = 99
      ENDIF
 
      CALL CDLDIR(CHPATH(1:LPATH),LOUTCD,NLEVEL,CHOPT,IRET)
 
      NDIR = IQUEST(11)
      NMAT = IQUEST(12)
 
         IF(NDIR.NE.0) THEN
            WRITE(LOUTCD,9001) NDIR,NMAT
         ELSE
            WRITE(LOUTCD,9002)
         ENDIF
*
*     Reset current directory
*
         CALL RZCDIR(CHDIR(1:LCHDIR),'U')
 
 9001    FORMAT(' Total of ',I10,' subdirectories of which ',
     +          I10,' match')
 9002    FORMAT(' No subdirectories found')
 
         END
