*
* $Id: mget.F,v 1.1.1.1 1996/03/08 15:44:19 mclareni Exp $
*
* $Log: mget.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:19  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE MGET
*-- Author :    Jamie Shiers   12/02/91
      CHARACTER*64 LOCAL,REMOTE,FNAME1,FNAME2
      CHARACTER*8  LOCTYP,REMTYP
      CHARACTER*4  CHOPT
      PARAMETER    (NMAX=100)
      CHARACTER*128 FILES(NMAX)
#include "cspack/quest.inc"
#include "cspack/hcmail.inc"
#include "cspack/czsock.inc"
#include "cspack/czunit.inc"
#include "cspack/czopen.inc"
*
*     Get multiple files from remote node, with optional
*     change of extension name (e.g. .CAR -> .CARDS)
*
      CALL KUGETC(REMOTE,NCHR)
      CALL KUGETC(LOCAL,NCHL)
      CALL KUGETC(CHOPT,NCH)
 
      IF(NCHL.EQ.0) THEN
         LOCAL = REMOTE
         NCHL  = NCHR
      ENDIF
 
      IF((NCHL.EQ.1).AND.(LOCAL(1:1).EQ.'=')) THEN
         LOCAL = REMOTE
         NCHL  = NCHR
      ENDIF
*
*     Get list of file names
*
      IF(IDEBXZ.GE.1) PRINT *,'MGET. retrieving list of file names...'
 
      ICONT = 0
      JDOT   = INDEX(LOCAL(1:NCHL),'.')
 
      IF(IDEBXZ.GE.3) PRINT *,'MGET. call XZLS for ',REMOTE(1:NCHR)
    1 CONTINUE
      CALL XZLS(REMOTE(1:NCHR),FILES,NMAX,NFILES,ICONT,' ',IC)
      IF(IDEBXZ.GE.3) PRINT *,'MGET. ',NFILES,' files found '
      IF(ICONT.NE.0) THEN
         PRINT *,'MGET. too many files found. Excess names will be ',
     +           'discarded'
#include "cspack/czflush.inc"
      ENDIF
      DO 20 I   = 1,NFILES
         ISTART = 1
         IEND   = LENOCC(FILES(I))
*
*     Fix VM  file names...
*
         CALL CSQMBL(FILES(I),ISTART,IEND)
         IEND   = LENOCC(FILES(I))
         CALL CTRANS(' ','.',FILES(I),ISTART,IEND)
*
*     Fix VMS file names...
*
         IBRA   = ICFMUL(']>',FILES(I),1,IEND)
         IF(IBRA.LE.IEND) ISTART = IBRA + 1
         FNAME1 = FILES(I)(ISTART:IEND)
         LF1    = LENOCC(FNAME1)
         ICOL   = INDEX(FNAME1(1:LF1),';')
         IF(ICOL.NE.0) LF1 = ICOL - 1
         IF(IDEBXZ.GE.1) PRINT *,'MGET. processing ',FNAME1(1:LF1),
     +   '...'
         CALL CLTOU(FNAME1)
         IDOT   = INDEX(FNAME1(1:LF1),'.')
         IF((IDOT.NE.0).AND.(JDOT.NE.0)) THEN
            IF(LOCAL(1:NCHL).NE.REMOTE(1:NCHR)) THEN
               FNAME2 = FNAME1(1:IDOT)//LOCAL(JDOT+1:NCHL)
               LF2    = LENOCC(FNAME2)
            ELSE
               FNAME2 = FNAME1
               LF2    = LF1
            ENDIF
         ENDIF
 
         IF(LENOCC(FNAME2).EQ.0) THEN
            FNAME2 = FNAME1
            LF2    = LF1
         ENDIF
 
#if !defined(CERNLIB_IBMVM)
*
*     Remove file mode from VM file names...
*
         KDOT = INDEX(FNAME1(1:LF1),'.')
         LDOT = INDEXB(FNAME1(1:LF1),'.')
         IF(LDOT.GT.KDOT) THEN
            LF1 = LDOT - 1
         ENDIF
#endif
 
         IF(INDEX(FNAME1(1:LF1),'.PAM').NE.0) THEN
            IF(IDEBXZ.GE.2) PRINT *,'MGET. retrieving ',FNAME2(1:LF2),
     +                              ' to ',FNAME1(1:LF1),' via GETP'
            CALL XZGETP(FNAME1(1:LF1),FNAME2(1:LF2),CHOPT,IC)
         ELSEIF(INDEX(FNAME1(1:LF1),'.CET').NE.0) THEN
            IF(IDEBXZ.GE.2) PRINT *,'MGET. retrieving ',FNAME2(1:LF2),
     +                              ' to ',FNAME1(1:LF1),' via GETB'
            CALL XZGETB(FNAME1(1:LF1),FNAME2(1:LF2),3600,CHOPT,IC)
         ELSEIF(INDEX(FNAME1(1:LF1),'.CMZ').NE.0) THEN
            IF(IDEBXZ.GE.2) PRINT *,'MGET. retrieving ',FNAME2(1:LF2),
     +                              ' to ',FNAME1(1:LF1),' via GETRZ'
            CALL XZGETR(FNAME1(1:LF1),FNAME2(1:LF2),CHOPT,IC)
         ELSE
            IF(IDEBXZ.GE.2) PRINT *,'MGET. retrieving ',FNAME2(1:LF2),
     +                              ' to ',FNAME1(1:LF1),' via GETA'
            CALL XZGETA(FNAME1(1:LF1),FNAME2(1:LF2),CHOPT,IC)
         ENDIF
   20 CONTINUE
 
   99 END
