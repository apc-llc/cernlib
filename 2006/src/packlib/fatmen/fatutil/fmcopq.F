*
* $Id: fmcopq.F,v 1.1.1.1 1996/03/07 15:18:07 mclareni Exp $
*
* $Log: fmcopq.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:07  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMCOPQ(GENAM1,LBANK1,KEYS1,GENAM2,LBANK2,KEYS2,
     +CHOPT,IRC)
*
*     Generate update file for COPY command
*
#include "fatmen/faust.inc"
#include "fatmen/fausto.inc"
#include "fatmen/fatsys.inc"
#include "fatmen/fatbank.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fmnkeys.inc"
#include "fatmen/slate.inc"
      CHARACTER*(*) GENAM1,GENAM2,CHOPT
      DIMENSION     KEYS1(LKEYFA),KEYS2(LKEYFA)
      CHARACTER*255 CHEOPS,CHFILE,CHFOUT
      CHARACTER*80  COMM
#if defined(CERNLIB_IBMVM)
      CHARACTER*1   CMXDSK,DISK
#endif
#if defined(CERNLIB_VAXVMS)
      CHARACTER*255 CHTEMP
      INCLUDE       '($RMSDEF)'
#endif
#if defined(CERNLIB_UNIX)
      INTEGER       SYSTEMF
#endif
      PARAMETER     (IFLAG=0)
      LOGICAL       IEXIST
#if defined(CERNLIB_VAXVMS)
      INTEGER       FMHOST,FMVUSR,FMACNT
      CHARACTER*12  CHUSER
      CHARACTER*8   CHACNT,CHHOST,CHTYPE,CHSYS
#endif
#if !defined(CERNLIB_VAXVMS)
      INTEGER       FMHOST,FMUSER,FMACNT
      CHARACTER*8   CHUSER,CHACNT,CHHOST,CHTYPE,CHSYS
#endif
      SAVE NENTRY,LCHOPS,CHEOPS
#include "fatmen/fatran0.inc"
#include "fatmen/fatsat0.inc"
#include "fatmen/fatoptd.inc"
#include "fatmen/fatran1.inc"
#include "fatmen/fatsat1.inc"
 
      DATA NENTRY   /0/
 
#include "fatmen/fatoptc.inc"
 
 
#if defined(CERNLIB_IBMMVS)
      RETURN
#endif
 
 
      NFCOPQ = NFCOPQ + 1
      FATMBQ = FATMBQ + Q(LBANK1+KOFUFA+MFSZFA)
 
      IF(NENTRY.EQ.0) THEN
*
*     Check if CHEOPS directory is accessible
*
#if defined(CERNLIB_IBMVM)
*
*     Try to link to FMCHEOPS
*
         CALL VMCMS('EXEC GIME FMCHEOPS (QUIET NONOTICE STACK)',IRC)
         IF (IRC .LE. 4) THEN
            CALL VMRTRM(CHEOPS,LCHOPS)
            CALL VMCMS('EXEC DROP '//CHEOPS(1:1),IRC)
         ELSEIF(IRC.EQ.104) THEN
            IF(IDEBFA.GT.-3) PRINT *,'FMCOPQ. FMCHEOPS does not ',
     +         'exist - cannot queue updates to CHEOPS server'
            LCHOPS = -1
            RETURN
         ENDIF
 
#endif
#if defined(CERNLIB_VAXVMS)||defined(CERNLIB_UNIX)
*
*     Is CHEOPS directory defined
*
         CALL GETENVF('FMCHEOPS',CHEOPS)
         IF(IS(1).EQ.0) THEN
            IF(IDEBFA.EQ.0) PRINT *,'FMCOPQ. cheops queue directory ',
     +         'not defined - using default'
#endif
#if defined(CERNLIB_UNIX)
            CHEOPS = '/fatmen/fmcheops'
            LCHOPS = 16
#endif
#if defined(CERNLIB_VAXVMS)
            CHEOPS = 'fatmen:[fmcheops]'
            LCHOPS = 17
#endif
#if defined(CERNLIB_VAXVMS)||defined(CERNLIB_UNIX)
         ELSE
            LCHOPS = IS(1)
         ENDIF
#endif
#if defined(CERNLIB_VAXVMS)
         IRC = LIB$FIND_FILE(CHEOPS(1:LCHOPS)//'*.*',CHTEMP,ICONT)
         IC  = LIB$FIND_FILE_END(ICONT)
         IF(IRC.EQ.RMS$_DNF) THEN
            IF(IDEBFA.GT.-3) PRINT *,'FMCOPQ. directory ',
     +         CHEOPS(1:LCHOPS),' does not exist - ',
     +         'cannot queue updates to CHEOPS server'
            LCHOPS = -1
            RETURN
         ENDIF
#endif
#if defined(CERNLIB_UNIX)
         INQUIRE(FILE=CHEOPS(1:LCHOPS),EXIST=IEXIST)
         IF(.NOT.IEXIST) THEN
            IF(IDEBFA.GT.-3) PRINT *,'FMCOPQ. directory ',
     +         CHEOPS(1:LCHOPS),' does not exist - ',
     +         'cannot queue updates to CHEOPS server'
            IRC    = 28
            LCHOPS = -1
            RETURN
         ENDIF
#endif
      ENDIF
 
      IRC  = 0
 
      IF(LCHOPS.LE.0) THEN
         IRC = -99
         RETURN
      ENDIF
*
*     Are source and destination sites defined?
*
      IF(LDSTST.EQ.0.OR.LSRCST.EQ.0) THEN
         IF(IDEBFA.GE.-3) PRINT *,'FMCOPQ. both source and destination',
     +      ' sites must be defined'
         IRC = -98
         RETURN
      ENDIF
*
*     Are they the same?
*
      IF(CHSRCE(1:LSRCST).EQ.CHDEST(1:LDSTST)) THEN
         IF(IDEBFA.GE.-3) PRINT *,'FMCOPQ. error - source and ',
     +      'destination are the same'
         IRC = -97
         RETURN
      ENDIF
 
 
      LOLDFA = LBANK1
      LNEWFA = LBANK2
*
*     Generate file name
*
#if defined(CERNLIB_IBMVM)
      CALL FMFNM(CHFILE)
      LFILE = LENOCC(CHFILE) + 3
      DISK  = CMXDSK()
      CHFILE(LFILE-2:LFILE) = ' ' // DISK // '3'
      LDOT  = INDEX(CHFILE(1:LFILE),'.')
      IF(LDOT.NE.0) CHFILE(LDOT:LDOT) = ' '
#endif
#if defined(CERNLIB_VAXVMS)||defined(CERNLIB_UNIX)
      CALL FMFNME(CHFILE)
      LFILE = LENOCC(CHFILE)
#endif
 
      IF(IDEBFA.GE.3) PRINT *,'FMCOPQ. queue file is ',
     +   CHFILE(1:LFILE)
*
*     Open output file
*
#if defined(CERNLIB_IBMVM)
      OPEN(LUFZFA,FILE='/'//CHFILE(1:LFILE),STATUS='NEW',
     +     ACCESS='SEQUENTIAL',ACTION='WRITE',FORM='FORMATTED',
     +     IOSTAT=IRC)
#endif
#if defined(CERNLIB_VAXVMS)
      OPEN(LUFZFA,FILE=CHEOPS(1:LCHOPS)//CHFILE(1:LFILE),
#endif
#if defined(CERNLIB_UNIX)
      OPEN(LUFZFA,FILE=CHEOPS(1:LCHOPS)//'/'//CHFILE(1:LFILE),
#endif
#if defined(CERNLIB_VAXVMS)||defined(CERNLIB_UNIX)
     +     STATUS='NEW',ACCESS='SEQUENTIAL',
     +     FORM='FORMATTED',IOSTAT=IRC)
#endif
      IF(IRC.NE.0) THEN
         IF(IDEBFA.GE.-3) PRINT *,'FMCOPQ. cannot open output file'
         RETURN
      ENDIF
*
*     Write header
*
*     TRANS_TYPE=REQUEST
*     RQ_SRC_SITE=CERN
*     SRC_SITE=CERN
*     DST_SITE=LISBOA
*     DATASET_SIZE=10000
*     RQID=ID234567
*     EXPIR_DAT=unknown
*     COMPL_CODE=0
*     COMPL_DATE=unknown
*     COMPL_STR=unknown
*     LOCAL_FNAME=unknown
 
      LEND = LENOCC(FATTOP)
      LSTA = INDEXB(FATTOP(1:LEND),'/') + 1
 
      IC   = FMHOST(CHHOST,CHTYPE,CHSYS)
      IC   = FMACNT(CHACNT)
#if !defined(CERNLIB_VAXVMS)
      IC   = FMUSER(CHUSER)
#endif
#if defined(CERNLIB_VAXVMS)
      IC   = FMVUSR(CHUSER)
#endif
 
      WRITE(LUFZFA,9001) 'TRANS_TYPE=REQUEST'
      WRITE(LUFZFA,9001) 'RQ_SRC_SITE=CERN'
      WRITE(LUFZFA,9001) 'SRC_SITE='//CHSRCE(1:LSRCST)
      WRITE(LUFZFA,9001) 'DST_SITE='//CHDEST(1:LDSTST)
      WRITE(LUFZFA,9001) 'DATASET_SIZE=10000'
      WRITE(LUFZFA,9001) 'RQID=ID234567'
      WRITE(LUFZFA,9001) 'EXPIR_DAT=unknown'
      WRITE(LUFZFA,9001) 'COMPL_CODE=0'
      WRITE(LUFZFA,9001) 'COMPL_DATE=unknown'
      WRITE(LUFZFA,9001) 'COMPL_STR=unknown'
      WRITE(LUFZFA,9001) 'LOCAL_FNAME=unknown'
      WRITE(LUFZFA,9001) 'USER='//CHUSER
      WRITE(LUFZFA,9001) 'HOST='//CHHOST
      WRITE(LUFZFA,9001) 'ACCOUNT='//CHACNT
      WRITE(LUFZFA,9001) 'GROUP='//FATTOP(LSTA:LEND)
      WRITE(LUFZFA,9001) 'ORIGIN=FATMEN'
      WRITE(LUFZFA,9001) 'LOCATION=unknown'
      WRITE(LUFZFA,9001) 'INT_PARAM=unknown'
*
*     Write bank information
*
      CALL FMBTOA(GENAM1,LBANK1,KEYS1,'S',IRC)
      CALL FMBTOA(GENAM2,LBANK2,KEYS2,'D',IRC)
*
*     Close and rename/send
*
      CLOSE(LUFZFA)
*
*     Update user comment field
*
      CALL DATIME(IDATE,ITIME)
      WRITE(COMM,'(A33,I6,A4,I4)')
     +   'Copy request queued to CHEOPS on ',IDATE,' at ',ITIME
      CALL UCTOH(COMM,IQ(LBANK2+KOFUFA+MUCMFA),4,80)
*
*     Update catalogue with output file
*
      CALL FMMOD(GENAM2,LBANK2,IFLAG,IRC)
      IF(IRC.NE.0) THEN
         IF(IDEBFA.GE.-3) PRINT *,'FMCOPQ. error updating ',
     +      'catalogue with output file information'
         RETURN
      ENDIF
#if defined(CERNLIB_IBMVM)
*
*     Sendfile to server
*
      CALL VMCMS('EXEC SENDFILE '//CHFILE(1:LFILE)//' TO FMCHEOPS',IRC)
#endif
#if defined(CERNLIB_VAXVMS)||defined(CERNLIB_UNIX)
*
*     Rename file
*
      LDOT = INDEX(CHFILE(1:LFILE),'.')
      CHFOUT = CHFILE(1:LFILE)
      CHFOUT(LDOT:LDOT) = '_'
      CHFOUT(LFILE-3:LFILE) = '.req'
#endif
#if defined(CERNLIB_VAXVMS)
      ISTAT = LIB$RENAME_FILE(CHEOPS(1:LCHOPS)//CHFILE(1:LFILE),
     +                      CHEOPS(1:LCHOPS)//CHFOUT(1:LFILE),,,,,,,,,,)
#include "fatmen/fatvaxrc.inc"
#endif
#if defined(CERNLIB_UNIX)
      IRC = SYSTEMF('mv '//CHEOPS(1:LCHOPS)//'/'//CHFILE(1:LFILE)//
     +              ' '  //CHEOPS(1:LCHOPS)//'/'//CHFOUT(1:LFILE))
#endif
 
9001  FORMAT(A)
 
      END
