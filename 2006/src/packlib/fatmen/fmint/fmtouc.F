*
* $Id: fmtouc.F,v 1.1.1.1 1996/03/07 15:18:09 mclareni Exp $
*
* $Log: fmtouc.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:09  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMTOUC(GENAM,KEYS,CHOPT,IRC)
      CHARACTER*(*) GENAM
      PARAMETER (LKEYFA=10)
#include "fatmen/faust.inc"
#include "fatmen/fmaxcop.inc"
      DIMENSION KEYS(LKEYFA)
      DIMENSION KEYSIN(LKEYFA),KEYSOU(LKEYFA,MAXCOP)
      INTEGER FMUSER,FMHOST,FMJOB,FMACNT
      CHARACTER*8  CNAME,CTYPE,CSYS,CUSER,CACCT
      CHARACTER*16 CJOB
 
#include "fatmen/fatbank.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fatopts.inc"
*
*     Options: O - set owner information
*              T - set last access time to now
*              U - zero use count
*              A - set account field
*              C - clear comment field
*
      IRC = 0
      LG  = LENOCC(GENAM)
      IF(IDEBFA.GT.0) PRINT *,'FMTOUC. updating ',GENAM(1:LG)
*
      IC = FMUSER(CUSER)
      IC = FMHOST(CNAME,CTYPE,CSYS)
      IC = FMACNT(CACCT)
      IC = FMJOB(CJOB)
*
      IF(KEYS(1).GT.0) CALL FMPKEY(KEYS,LKEYFA)
      KEYSIN(MKSRFA) = -1
      KEYSIN(MKMTFA) = -1
      KEYSIN(MKLCFA) = -1
      KEYSIN(MKCLFA) = -1
      CALL FMSELK(GENAM(1:LG),KEYSIN,KEYSOU,NFOUND,MAXCOP,IRC)
      IF(IDEBFA.GT.0) PRINT *,'FMTOUC. ',NFOUND,
     +                        ' entries for ',GENAM(1:LG),' found '
 
      DO 10 I=1,NFOUND
      IF((KEYS(1).GT.0).AND.(KEYSOU(1,I).NE.KEYS(1))) GOTO 10
      LTDSFA = 0
      CALL FMGETK(GENAM(1:LG),LTDSFA,KEYSOU(1,I),IRC)
      IF(IRC.NE.0) THEN
         IF(IDEBFA.GE.0)
     +   PRINT *,'FMTOUC. return code ',IRC,' from FMGETK'
         GOTO 10
      ENDIF
 
      IF (IOPTA.NE.0) THEN
*
*     Account field
*
         IF(IDEBFA.GE.0)
     +   PRINT *,'FMTOUC. Setting account field'
         CALL FMPUTC(LTDSFA,CACCT,MCIDFA,NCIDFA,IRC)
         IF(IRC.NE.0) THEN
            IF(IDEBFA.GE.0) PRINT *,'FMTOUC. error setting account'
            GOTO 10
         ENDIF
 
      ENDIF
 
      IF (IOPTC.NE.0) THEN
*
*     Comment field
*
         IF(IDEBFA.GE.0)
     +   PRINT *,'FMTOUC. clearing comment field'
         CALL VBLANK(IQ(LTDSFA+KOFUFA+MUCMFA),NUCMFA)
 
      ENDIF
 
 
      IF (IOPTO.NE.0) THEN
*
*     Owner attributes
*
         IF(IDEBFA.GE.0)
     +   PRINT *,'FMTOUC. Setting owner attributes'
         CALL FMPUTC(LTDSFA,CUSER,MCURFA,NCURFA,IRC)
         IF(IRC.NE.0) THEN
            IF(IDEBFA.GE.0) PRINT *,'FMTOUC. error setting user name'
            GOTO 10
         ENDIF
         CALL FMPUTC(LTDSFA,CNAME,MCNIFA,NCNIFA,IRC)
         IF(IRC.NE.0) THEN
            IF(IDEBFA.GE.0) PRINT *,'FMTOUC. error setting node name'
            GOTO 10
         ENDIF
         CALL FMPUTC(LTDSFA,CACCT,MCIDFA,NCIDFA,IRC)
         IF(IRC.NE.0) THEN
            IF(IDEBFA.GE.0) PRINT *,'FMTOUC. error setting account'
            GOTO 10
         ENDIF
         CALL FMPUTC(LTDSFA,CJOB ,MCJIFA,NCJIFA,IRC)
         IF(IRC.NE.0) THEN
            IF(IDEBFA.GE.0) PRINT *,'FMTOUC. error setting job name'
            GOTO 10
         ENDIF
      ENDIF
 
      IF (IOPTT.NE.0) THEN
*
*     Time attributes
*
         CALL DATIME(IDATE,ITIME)
         CALL FMPKTM(IDATE,ITIME,IPACK,IRC)
         IF(IDEBFA.GE.0)
     +   PRINT *,'FMTOUC. date & time of last access ',
     +           'will be set to ',IDATE,ITIME
         IQ(LTDSFA+KOFUFA+MLATFA) = IPACK
      ENDIF
 
      IF (IOPTU.NE.0) THEN
*
*     Use count
*
         IF(IDEBFA.GE.0) PRINT *,'FMTOUC. use count will be zeroed'
         IQ(LTDSFA+KOFUFA+MUSCFA) = 0
      ENDIF
 
      NFTOUC = NFTOUC + 1
      CALL FMMOD(GENAM(1:LG),LTDSFA,0,IRC)
      CALL MZDROP(IDIVFA,LTDSFA,' ')
      LTDSFA = 0
10    CONTINUE
 
      END
 
