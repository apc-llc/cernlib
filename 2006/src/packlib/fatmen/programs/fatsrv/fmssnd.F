*
* $Id: fmssnd.F,v 1.1.1.1 1996/03/07 15:17:41 mclareni Exp $
*
* $Log: fmssnd.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:41  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMSSND(GENAM,CHFILE,IRC)
*CMZ :          05/07/91  11.15.20  by  Jamie Shiers
*-- Author :    Jamie Shiers   05/07/91
#include "fatmen/fatsys.inc"
      CHARACTER*(*) GENAM,CHFILE
      PARAMETER     (MAXSRV=50)
      PARAMETER     (MAXDIR=16)
      PARAMETER     (LUNNAM=10)
      CHARACTER*20  CHIN(2,10)
      CHARACTER*255 CHOUT(2,17)
      CHARACTER*255 CHQUE
      CHARACTER*255 CHSERV
      CHARACTER*255 CHNAME,FNAME
      CHARACTER*8   CHUSER
      CHARACTER*80  CHNICK(MAXSRV)
      CHARACTER*80  CHNODE(MAXSRV)
      CHARACTER*80  CHQUES(MAXSRV)
      CHARACTER*80  CHDIRS(MAXSRV,MAXDIR)
      DIMENSION     LDIRS(MAXSRV,MAXDIR)
      DIMENSION     LQUES(MAXSRV)
      DIMENSION     LNODE(MAXSRV)
      INTEGER       FMUSER,FMNODE,SYSTEMF
      LOGICAL       IEXIST
      SAVE          CHNODE,CHDIRS,CHQUES,LNODE,LDIRS,LQUES,NSERV
#include "fatmen/fatbug.inc"
 
      DATA NENTRY/0/
      SAVE NENTRY
 
      IRC = 0
 
      LGN = LENOCC(GENAM)
      LF  = LENOCC(CHFILE)
*
*     On first entry, get list of remote servers
*
      IF(NENTRY.EQ.0) THEN
 
         LDEF   = LENOCC(DEFAULT)
         NENTRY = 1
#if defined(CERNLIB_VAXVMS)
         CHNAME = DEFAULT(1:LDEF)//SERNAM(1:LENOCC(SERNAM))//'.NAMES'
         LN     = LENOCC(CHNAME)
#endif
#if defined(CERNLIB_UNIX)
         CHNAME = DEFAULT(1:LDEF)//'/'
     +            //SERNAM(1:LENOCC(SERNAM))//'.NAMES'
         LN     = LENOCC(CHNAME)
         CALL CUTOL(CHNAME(1:LN))
#endif
#if defined(CERNLIB_IBMMVS)
         CHNAME = DEFAULT(1:LDEF)//'.'
     +            //SERNAM(1:LENOCC(SERNAM))//'.NAMES'
         LN     = LENOCC(CHNAME)
#endif
*
*     Get the list of FATSERVERS...
*
         CHIN(1,1)  = ':nick'
         CHIN(2,1)  = 'FATSERVERS'
 
         CHOUT(1,1) = ':list'
 
         NIN        = 1
         NOUT       = 1
         NSERV      = 0
 
         IF(IDEBFA.GE.1) WRITE(LPRTFA,9001) CHNAME(1:LN)
 9001 FORMAT(' FMSSND. names file is ',A)
 
         CALL NAMEFD(LUNNAM,CHNAME(1:LN),CHIN,NIN,CHOUT,NOUT,IRC)
*
*    IRC = -1 : file not found
*          32 : no match
*
         IF(IRC.NE.0) THEN
            IF(IDEBFA.GE.0) PRINT 9002,IRC
 9002 FORMAT(' FMSSND. return code ',I5,' from NAMEFD')
            RETURN
         ENDIF
*
*     For each server, get
*        :node and list of directory patterns
*
         CHSERV = CHOUT(2,1)
         CALL CSQMBL(CHSERV,1,LEN(CHSERV))
         LCHSERV = LENOCC(CHSERV)
         IF(LCHSERV.EQ.0) RETURN
 
         CALL FMNWRD(' ',CHSERV(1:LCHSERV),NSERV)
 
         IF(NSERV.GT.MAXSRV) THEN
            IF(IDEBFA.GE.0) PRINT *,
     +         'FMSSND. cannot process more than ',MAXSRV,
     +         ' servers'
            NSERV = MAXSRV
         ENDIF
 
         DO 30 I=1,NSERV
            CALL FMWORD(CHNICK(I),I-1,' ',CHSERV(1:LCHSERV),IRC)
            IF(IDEBFA.GE.0) PRINT *,'FMSSND. processing ',CHNICK(I)
*
*     For each nick name, get the node and up to 50 directory names
*
            CHIN(1,1)  = ':nick'
            CHIN(2,1)  = CHNICK(I)
 
            CHOUT(1,1) = ':node'
            CHOUT(2,1) = ' '
*
* ***       CHOUT(2,1) = ':dir1' etc.
*
            DO 10 J=2,MAXDIR+1
            JJ = J-1
            IF(JJ.LT.10) THEN
               WRITE(CHOUT(1,J),9003) JJ
            ELSE
               WRITE(CHOUT(1,J),9004) JJ
            ENDIF
            CHOUT(2,J) = ' '
 9003       FORMAT(':DIR',I1)
 9004       FORMAT(':DIR',I2)
   10       CONTINUE
 
            NIN        = 1
            NOUT       = MAXDIR + 1
            CALL NAMEFD(LUNNAM,CHNAME(1:LN),CHIN,NIN,CHOUT,NOUT,IRC)
 
            LNODE(I)   = LENOCC(CHOUT(2,1))
            CHNODE(I)  = CHOUT(2,1)(1:LNODE(I))
            IF(IDEBFA.GE.0) PRINT *,'FMSSND. node        : ',
     +         CHNODE(I)(1:LNODE(I))
 
            DO 20 J=1,MAXDIR
            LDIRS(I,J)      = LENOCC(CHOUT(2,J+1))
            IF(LDIRS(I,J).GT.0) THEN
               CHDIRS(I,J) = CHOUT(2,J+1)(1:LDIRS(I,J))
               IF(IDEBFA.GE.0) PRINT *,'FMSSND. directory   : ',
     +               CHOUT(2,J+1)(1:LDIRS(I,J))
               CALL CLTOU(CHDIRS(I,J)(1:LDIRS(I,J)))
            ENDIF
*
   20 CONTINUE
#if defined(CERNLIB_UNIX)||defined(CERNLIB_VAXVMS)
            CHQUES(I)  = 'TO'//CHNODE(I)(1:LNODE(I))
            LQUES(I)   = LNODE(I) + 2
#endif
#if defined(CERNLIB_UNIX)
            CALL CUTOL(CHQUES(I)(1:LQUES(I)))
#endif
*
*     Check that this subdirectory exists, if not, create
*     (unless the node points to this one)
*
            IRC = FMNODE(CHNODE(I)(1:LNODE(I)))
            IF(IRC.NE.0) THEN
#if defined(CERNLIB_VAXVMS)
               CHQUE = DEFAULT(1:LDEF-1) // '.' // CHQUES(I)(1:LQUES(I))
     +                 // DEFAULT(LDEF:LDEF)
               LQUE  = LENOCC(CHQUE)
               INQUIRE(FILE=DEFAULT(1:LDEF)//
     +                 CHQUES(I)(1:LQUES(I))//'.DIR',
     +                 EXIST=IEXIST)
               IF(.NOT.IEXIST) THEN
                  IF(IDEBFA.GE.0) PRINT *,'FMSSND. creating directory ',
     +                    CHQUE(1:LQUE)
                  IC = LIB$CREATE_DIR(CHQUE(1:LQUE))
                  IF(.NOT.IC) THEN
                     IF(IDEBFA.GE.0) PRINT *,
     +               'FMSSND. error creating directory ',
     +               CHQUE(1:LQUE)
                  ENDIF
               ENDIF
#endif
#if defined(CERNLIB_UNIX)
               CHQUE = DEFAULT(1:LDEF) // '/' // CHQUES(I)(1:LQUES(I))
               LQUE  = LENOCC(CHQUE)
               INQUIRE(FILE=CHQUE(1:LQUE),EXIST=IEXIST)
               IF(.NOT.IEXIST) THEN
                  IF(IDEBFA.GE.0) PRINT *,'FMSSND. creating directory ',
     +                  CHQUE(1:LQUE)
                  IC = SYSTEMF('mkdir '//CHQUE(1:LQUE))
                  IF(IC.NE.0) THEN
                     IF(IDEBFA.GE.0) PRINT *,
     +                  'FMSSND. error creating directory ',
     +                  CHQUE(1:LQUE)
                  ENDIF
               ENDIF
#endif
            ELSE
               IF(IDEBFA.GE.0) PRINT *,'FMSSND. skipping node ',
     +            CHNODE(I)(1:LNODE(I))
            ENDIF
   30    CONTINUE
 
      ENDIF
*
*     Copy this file to subdirectories for all remote servers
*
      CALL FMJOUR(FNAME)
      LENF = LENOCC(FNAME)
      DO 60 I=1,NSERV
*
*     Check that we are not copying to ourselves...
*
         IRC = FMNODE(CHNODE(I)(1:LNODE(I)))
         IF(IRC.EQ.0) GOTO 60
*
*     Check that the file did not come from the current remote node
*
         IF(INDEX(CHFILE(1:LF),'_'//CHNODE(I)(1:LNODE(I))).NE.0) GOTO 60
*
*     Check that pathname matches at least one of those specified
*
         DO 40 K=1,MAXDIR
         IF(LDIRS(I,K).EQ.0) GOTO 40
         CALL FMATCH(GENAM(1:LGN),CHDIRS(I,K)(1:LDIRS(I,K)),IRC)
         IF(IRC.EQ.0) GOTO 50
   40    CONTINUE
*
*    No match found - skip
*
         PRINT *,'FMSSND. no match found - update skipped for ',
     +      CHNODE(I)(1:LNODE(I))
         GOTO 60
   50    CONTINUE
         IF(IDEBFA.GE.3) PRINT *,'FMSSND. match found for ',
     +      CHNODE(I)(1:LNODE(I))
#if defined(CERNLIB_VAXVMS)
         CHQUE = DEFAULT(1:LDEF-1) // '.' // CHQUES(I)(1:LQUES(I))
     +           // DEFAULT(LDEF:LDEF)
#endif
#if defined(CERNLIB_UNIX)
         CHQUE = DEFAULT(1:LDEF) // '/' // CHQUES(I)(1:LQUES(I)) // '/'
#endif
#if defined(CERNLIB_IBMMVS)
         CHQUE = DEFAULT(1:LDEF) // '.' // CHQUES(I)(1:LQUES(I)) // '.'
#endif
         LQUE  = LENOCC(CHQUE)
         CALL FACOPY(CHFILE(1:LF),CHQUE(1:LQUE)//
     +               FNAME(1:LENF),IRC)
         IF(IRC.NE.0) THEN
            IF(IDEBFA.GE.-3) PRINT *,'FMSSND. error copying ',
     +         CHFILE(1:LF),' to ',
     +         CHQUE(1:LQUE)//FNAME(1:LENF)
         ENDIF
   60 CONTINUE
 
      END
 
