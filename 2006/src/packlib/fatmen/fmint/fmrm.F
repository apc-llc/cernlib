*
* $Id: fmrm.F,v 1.1.1.1 1996/03/07 15:18:11 mclareni Exp $
*
* $Log: fmrm.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:11  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMRM(GENAME,L,KEYS,IRC)
#include "fatmen/faust.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fatbank.inc"
#include "fatmen/fmpath.inc"
*
*     If key serial number is non-zero, use that
*
      CHARACTER*(*) GENAME
      CHARACTER*4 COMM
      CHARACTER*8 ACCT,ACNT
      PARAMETER   (LKEYFA=10)
#include "fatmen/fmaxcop.inc"
      DIMENSION   KEYS(LKEYFA)
      DIMENSION   KEYSIN(LKEYFA)
      DIMENSION   KEYSOU(LKEYFA,MAXCOP)
      INTEGER     FMACNT
 
      IRC  = 0
      COMM = 'DEL '
      NCH  = LENOCC(GENAME)
*
*     Save current directory
*
      CALL FACDIR(CDIR,'R')
      LCDIR = LENOCC(CDIR)
 
      LFILE = INDEXB(GENAME(1:NCH),'/') -1
      FILE1 = GENAME(LFILE+2:NCH)
      CALL FACDIR(GENAME(1:LFILE),'U')
      LFILE = LENOCC(FILE1)
*
*     Check how many copies of this dataset exist
*
      CALL UCOPY(KEYS,KEYSIN,10)
*
*     Don't compare media type, copy level or location code
*
      KEYSIN(MKMTFA) = -1
      KEYSIN(MKCLFA) = -1
      KEYSIN(MKLCFA) = -1
      CALL FMSELK(GENAME(1:NCH),KEYSIN,KEYSOU,NMATCH,MAXCOP,IRC)
      IF(NMATCH.EQ.0) THEN
         IF(IDEBFA.GE.0)
     +   PRINT *,'FMRM. found 0 matches for ',GENAME(1:NCH)
         IRC = 1
         GOTO 99
      ELSEIF((NMATCH.GT.1).AND.(KEYS(1).EQ.0)) THEN
         IF(IDEBFA.GE.0) THEN
            PRINT *,'FMRM. found ',NMATCH,' matches for ',GENAME(1:NCH)
            PRINT *,'FMRM. Please specify which copy is to be deleted'
         ENDIF
         IRC = 1
         GOTO 99
      ELSE
         DO 10 I=1,NMATCH
            IF((NMATCH.EQ.1).AND.(KEYS(1).EQ.0)) THEN
               CALL UCOPY(KEYSOU(1,1),KEYS,10)
               GOTO 20
             ENDIF
            IF(KEYS(1).EQ.KEYSOU(1,I)) GOTO 20
   10    CONTINUE
         PRINT *,'FMRM. no match for key specified'
         IRC = -1
         GOTO 99
   20    CONTINUE
         LTDSFA = 0
         CALL FMGETK(GENAME(1:NCH),LTDSFA,KEYSOU(1,I),IRC)
         IF(IRC.NE.0) THEN
            IF(IDEBFA.GE.0) PRINT *,'FMRM. Return code ',IRC,' from '
     +      //'FMGETK'
            GOTO 99
         ENDIF
      ENDIF
*
*     Check account against that of creator, permitting account aliases
*
      CALL UHTOC(IQ(LTDSFA+KOFUFA+MCIDFA),4,ACNT,8)
      IC = FMACNT(ACCT)
      CALL FMACCT(ACCT,ACNT,IRC)
*     IF (ACNT .NE. ACCT) THEN
      IF(IRC.NE.0) THEN
 
         WRITE(LPRTFA,*) 'You cannot delete this file'
         WRITE(LPRTFA,*) 'File is owned by ',ACNT,' current account = '
     +   , ACCT
         IRC = 1
         GOTO 99
      ENDIF
 
      CALL FMVERI(GENAME,LTDSFA,KEYS,'A',IRC)
      IF(IRC.NE.0) THEN
         IF(IDEBFA.GE.-3) PRINT *,'FMRM. error(s) detected ',
     +      'by FMVERI. Cannot perform deletion.'
         GOTO 99
      ENDIF
 
      NFRMFL = NFRMFL + 1
      CALL FMFZO(COMM,GENAME,LTDSFA,KEYS,IRC)
 
   99 CONTINUE
*
*     Reset current directory
*
      CALL FACDIR(CDIR(1:LCDIR),' ')
      RETURN
      END
