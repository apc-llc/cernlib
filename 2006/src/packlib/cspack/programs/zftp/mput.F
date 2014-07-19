*
* $Id: mput.F,v 1.1.1.1 1996/03/08 15:44:19 mclareni Exp $
*
* $Log: mput.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:19  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE MPUT
*-- Author :
*CMZ :          12/02/91  13.50.05  by  Jamie Shiers
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
*     Put multiple files to remote node, with optional
*     change of extension name (e.g. .CAR -> .CARDS)
*
      CALL KUGETC(LOCAL,NCHL)
      CALL KUGETC(REMOTE,NCHR)
      CALL KUGETC(CHOPT,NCH)
 
      IF(NCHR.EQ.0) THEN
         REMOTE = LOCAL
         NCHR  = NCHL
      ENDIF
 
      IF((NCHR.EQ.1).AND.(REMOTE(1:1).EQ.'=')) THEN
         REMOTE = LOCAL
         NCHR  = NCHL
      ENDIF
*
*     Get list of file names
*
 
      ICONT = 0
      JDOT   = INDEX(REMOTE(1:NCHR),'.')
 
    1 CONTINUE
      CALL XZLLS(LOCAL(1:NCHL),FILES,NMAX,NFILES,ICONT,' ',IC)
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
         IBRA   = ICFMUL('>]',FILES(I),1,IEND)
         IF(IBRA.LE.IEND) ISTART = IBRA + 1
         FNAME1 = FILES(I)(ISTART:IEND)
         LF1    = LENOCC(FNAME1)
         ICOL   = INDEX(FNAME1(1:LF1),';')
         IF(ICOL.NE.0) LF1 = ICOL - 1
         IF(IDEBXZ.GE.1) PRINT *,'MPUT. processing ',FNAME1(1:LF1),
     +   '...'
         CALL CLTOU(FNAME1)
         IDOT   = INDEX(FNAME1(1:LF1),'.')
         IF((IDOT.NE.0).AND.(JDOT.NE.0)) THEN
            IF(LOCAL(1:NCHL).NE.REMOTE(1:NCHR)) THEN
               FNAME2 = FNAME1(1:IDOT)//REMOTE(JDOT+1:NCHR)
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
*
*     Remove file mode from VM file names...
*
         KDOT = INDEX(FNAME2(1:LF2),'.')
         LDOT = INDEXB(FNAME2(1:LF2),'.')
         IF(LDOT.GT.KDOT) THEN
            LF2 = LDOT - 1
            IF(IDEBXZ.GE.3) PRINT *,'MPUT. removing file mode from ',
     +         'VM file name.'
         ENDIF
 
         IF(INDEX(FNAME1(1:LF1),'.PAM').NE.0) THEN
            IF(IDEBXZ.GE.2) PRINT *,'MPUT. sending ',FNAME1(1:LF1),
     +                              ' to ',FNAME2(1:LF2),' via PUTP'
            CALL XZPUTP(FNAME1(1:LF1),FNAME2(1:LF2),CHOPT,IC)
         ELSEIF(INDEX(FNAME1(1:LF1),'.CET').NE.0) THEN
            IF(IDEBXZ.GE.2) PRINT *,'MPUT. sending ',FNAME1(1:LF1),
     +                              ' to ',FNAME2(1:LF2),' via PUTB'
            CALL XZPUTB(FNAME1(1:LF1),FNAME2(1:LF2),3600,CHOPT,IC)
         ELSEIF(INDEX(FNAME1(1:LF1),'.CMZ').NE.0) THEN
            IF(IDEBXZ.GE.2) PRINT *,'MPUT. sending ',FNAME1(1:LF1),
     +                              ' to ',FNAME2(1:LF2),' via PUTRZ'
            CALL XZPUTR(FNAME1(1:LF1),FNAME2(1:LF2),CHOPT,IC)
         ELSE
            IF(IDEBXZ.GE.2) PRINT *,'MPUT. sending ',FNAME1(1:LF1),
     +                              ' to ',FNAME2(1:LF2),' via PUTA'
            CALL XZPUTA(FNAME1(1:LF1),FNAME2(1:LF2),CHOPT,IC)
         ENDIF
   20 CONTINUE
      IF(ICONT.NE.0) GOTO 1
 
   99 END
