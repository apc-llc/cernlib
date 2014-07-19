*
* $Id: fmveri.F,v 1.4 1998/02/13 14:01:21 jamie Exp $
*
* $Log: fmveri.F,v $
* Revision 1.4  1998/02/13 14:01:21  jamie
* dont check bank address is 0
*
* Revision 1.3  1997/10/23 13:39:36  mclareni
* NT mods
*
* Revision 1.2  1996/08/01 11:10:27  jamie
* Changes to support FMVERI On/Off
*
* Revision 1.1.1.1  1996/03/07 15:18:05  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMVERI(GENAM,LBANK,KEYS,CHOPT,IRC)
*
*     Verify the bank at address LBANK with generic name GENAM
*
#include "fatmen/fatveri.inc"
#include "fatmen/fatout.inc"
#include "fatmen/fatbank.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fattyp.inc"
#include "zebra/mzcn.inc"
**  V.Fine  This VIP variable is required to call
**               CALL FMXVID(VID,JP,XVID,VIP,'C',IRC)
**  (see source below)
      CHARACTER*8 VIP
*
      CHARACTER*(*)  GENAM,CHOPT
      PARAMETER      (LKEYFA=10)
      DIMENSION      KEYS(LKEYFA)
      CHARACTER*20   FNAME
      CHARACTER*80   COMM
      CHARACTER*4    FFORM,FLFRM,UFORM
      CHARACTER*256  DSN
      CHARACTER*6    VSN,VID
      CHARACTER*15   XVID
      CHARACTER*8    OWNER,ACCT,JOB,HOST,NODE,OS
      LOGICAL        IVERI,JVERI
      PARAMETER     (NPOSS=4)
      CHARACTER*8    CHPOSS(NPOSS)
 
#include "fatmen/tmsdef0.inc"
#include "fatmen/fatform0.inc"
#include "fatmen/fatoptd.inc"
#include "fatmen/tmsdef1.inc"
#include "fatmen/fatform1.inc"
      SAVE          CHPOSS
 
      DATA          CHPOSS(1)/'OSM'/,
     +              CHPOSS(2)/'UNITREE'/,
     +              CHPOSS(3)/'ADSM'/,
     +              CHPOSS(4)/'EMASS'/
 
#include "fatmen/fatoptc.inc"
 
      LWRITE = LPRTFA
 
      IF((CHOPT(1:1) .EQ. ' ') .OR. (IOPTA.NE.0)) THEN
         DO 10 I=1,26
   10    IOPT(I) = 1
      ENDIF
*
*     Only check TMS if explicitly requested
*
      IF(INDEX(CHOPT,'Q').EQ.0) IOPTQ = 0
*
*     Re-direct output?
*
      IF(INDEX(CHOPT,'R').NE.0) LWRITE = 3
 
      IRC = 0
      LADDR = LBANK + KOFUFA
      CALL VZERO(IQUEST,26)
      NCH = LENOCC(GENAM)
*
*     For warnings...
*
      IVERI = IDEBFA.GT.0
*
*     For errors...
*
      JVERI = IDEBFA.GE.-3
 
      IF(IVERI) WRITE(LWRITE,*) 'FMVERI. enter for ',GENAM(1:NCH),
     +   ' at address ',LBANK
 
      IF (IOPTZ .NE. 0. AND. LBANK .NE. 0) THEN
*
*     Check bank status word is valid
*
         CALL MZCHLS(IDIVFA,LBANK)
         IF(IQFOUL.NE.0) THEN
            IF(JVERI) THEN
               WRITE(LWRITE,*) 'FMVERI. bank status word is invalid'
               WRITE(LWRITE,*) 'FMVERI. Check for overwriting:'
               WRITE(LWRITE,*) '        e.g. IQ(L+iundef) = n'
            ENDIF
            IQUEST(26) = IQFOUL
            IRC        = IQFOUL
         ENDIF
      ENDIF
 
      IF (IOPTG .NE. 0) THEN
*
*     Check that the generic name does not contain a blank
*     or other strange characters
*
         LBLANK = INDEX(GENAM(1:NCH),' ')
 
         IF(LBLANK.NE.0) THEN
            IF(JVERI) THEN
               WRITE(LWRITE,*)
     +           'FMVERI. Error - generic name contains a blank'
               DSN = ' '
               DSN(LBLANK:LBLANK+7) = '^- blank'
               WRITE(LWRITE,*) GENAM(1:NCH)
               WRITE(LWRITE,*) DSN(1:LBLANK+7)
            ENDIF
            IRC = 1
         ENDIF
 
         LUNSCH = ICLUNS(GENAM,1,NCH)
         IF(LUNSCH.NE.0) THEN
            IF(JVERI) THEN
               WRITE(LWRITE,*)
     +           'FMVERI. Error - generic name contains an '//
     +           'unseen character'
               DSN = ' '
               DSN(LUNSCH:LUNSCH+8) = '^- unseen'
               WRITE(LWRITE,*) GENAM(1:NCH)
               WRITE(LWRITE,*) DSN(1:LUNSCH+8)
            ENDIF
            IRC = 1
         ENDIF
*
*     Check against double // in generic name
*
         ISLASH = INDEXB(GENAM(1:NCH),'//')
         IF(ISLASH.GT.1) THEN
            IF(JVERI) THEN
               WRITE(LWRITE,*)
     +         'FMVERI. Error - invalid generic name: ',
     +         GENAM(1:NCH)
               ISLASH = ISLASH + 38
               DSN = ' '
               DSN(ISLASH:ISLASH+9) = '^- invalid'
               WRITE(LWRITE,*) DSN(1:ISLASH+9)
            ENDIF
            IRC = 1
         ENDIF
      ENDIF
 
      IF (IOPTC .NE. 0) THEN
*
*     Comment field - anything goes?
*
         CALL CFILL(' ',COMM,1,80)
         CALL UHTOC(IQ(LADDR+MUCMFA),4,COMM,80)
         LCOMM = LENOCC(COMM)
         IF(LCOMM.EQ.0) THEN
            IF(IVERI) WRITE(LWRITE,*)
     +        'FMVERI. Warning - comment string is blank'
            IQUEST(3) = 1
         ENDIF
         IF(ICLUNS(COMM,1,LCOMM).NE.0) THEN
            IF(IVERI) WRITE(LWRITE,*) 'FMVERI. Warning - ',
     +               'comment string contains "UNSEEN" characters'
            IQUEST(3) = 2
         ENDIF
      ENDIF
 
      IF (IOPTE .NE. 0) THEN
*
*     User exit
*
         CALL FMUVER(GENAM(1:NCH),LBANK,KEYS,IRC)
         IF(IRC.NE.0) THEN
            IF(JVERI) WRITE(LWRITE,*) 'FMVERI. Error - return code ',
     +         IRC,' from user exit'
            IQUEST(5) = 1
         ENDIF
      ENDIF
 
      IF (IOPTF .NE. 0) THEN
*
*     File attributes
*
         IF((IQ(LADDR+MSRDFA).LT.0) .OR. (IQ(LADDR+MERDFA).LT.0) .OR.
     +      (IQ(LADDR+MSBLFA).LT.0) .OR. (IQ(LADDR+MEBLFA).LT.0)) THEN
 
            IQUEST(6) = 1
            IRC       = 1
            IF(JVERI) WRITE(LWRITE,*)
     +           'FMVERI. Error in file description',
     +           IQ(LADDR+MSRDFA),IQ(LADDR+MERDFA),
     +           IQ(LADDR+MSBLFA),IQ(LADDR+MEBLFA)
         ENDIF
 
      ENDIF
 
      IF (IOPTK .NE. 0) THEN
*
*     Keys - check that KEYS(1), KEYS(7-10) are positive
*     KEYS(2-6) must match last part of generic name
*
         FNAME = ' '
         CALL UHTOC(KEYS(2),4,FNAME,20)
         ICH = INDEXB(GENAM(1:NCH),'/')
         JCH = LENOCC(FNAME)
 
         IF(GENAM(ICH+1:NCH).NE.FNAME(1:JCH)) THEN
            IQUEST(11) = 1
            IRC = 1
            IF(JVERI)
     +         WRITE(LWRITE,*) 'FMVERI. Error - file name mismatch ',
     +         GENAM(ICH+1:NCH),'/',FNAME(1:JCH),' (GENAM/KEYS)'
               WRITE(LWRITE,*) 'length = ',NCH-ICH,JCH,' (GENAM/KEYS)'
         ENDIF
*
*     Check that rest of keys vector is ok
*
         IF((KEYS(MKSRFA).LT.0) .OR. (KEYS(MKCLFA).LT.0) .OR.
     +      (KEYS(MKLCFA).LT.0) .OR. (KEYS(MKMTFA).LE.0)) THEN
            IQUEST(11) = 2
            IRC = 1
            IF(JVERI) THEN
               WRITE(LWRITE,*) 'FMVERI. Error in keys vector'
               CALL FMPKEY(KEYS,10)
            ENDIF
 
         ENDIF
*
*     Check that keys match bank contents
*
         IF((KEYS(MKCLFA).NE.IQ(LADDR+MCPLFA)) .OR.
     +      (KEYS(MKLCFA).NE.IQ(LADDR+MLOCFA)) .OR.
     +      (KEYS(MKMTFA).NE.IQ(LADDR+MMTPFA))) THEN
 
            IQUEST(11) = 3
            IF(JVERI) THEN
               WRITE(LWRITE,*)
     +            'FMVERI. Error - Keys / bank mismatch'
               WRITE(LWRITE,*)
     +            KEYS(MKCLFA),IQ(LADDR+MCPLFA),' (copy level)'
               WRITE(LWRITE,*)
     +            KEYS(MKLCFA),IQ(LADDR+MLOCFA),' (location code)'
               WRITE(LWRITE,*) KEYS(MKMTFA),IQ(LADDR+MMTPFA),
     +            ' (media type)'
            ENDIF
 
         ENDIF
 
      ENDIF
 
      IF (IOPTL .NE. 0) THEN
*
*     Logical attributes
*
         FFORM = ' '
         CALL UHTOC(IQ(LADDR+MFLFFA),4,FFORM,4)
         CALL UHTOC(IQ(LADDR+MFUTFA),4,UFORM,4)
         LFF = LENOCC(FFORM)
         IF((LFF.EQ.0) .OR. (ICNTH(FFORM,FATFRM,NFATFM).EQ.0)) THEN
            IQUEST(12) = 1
            IRC        = 1
            IF(JVERI) THEN
               WRITE(LWRITE,*) 'FMVERI. Error in logical attributes'
               WRITE(LWRITE,*) ' File format = ',FFORM
               WRITE(LWRITE,*) ' File format must be one of ',FATFRM
            ENDIF
         ENDIF
         IF(FFORM(1:LFF).EQ.'RMS') THEN
            IQUEST(12) = 2
            IF(IVERI) THEN
               WRITE(LWRITE,*) 'FMVERI. warning - ',FFORM,
     +            ' is a deprecated file format'
            ENDIF
         ENDIF
 
      ENDIF
 
      IF (IOPTM .NE. 0) THEN
*
*     Media attributes
*
         IF (IQ(LADDR+MMTPFA) .EQ. 1) THEN
*
*        Disk dataset, check host type and O/S
*
            CALL UHTOC(IQ(LADDR+MHSNFA),4,HOST,8)
            CALL UHTOC(IQ(LADDR+MHSTFA),4,NODE,8)
            CALL UHTOC(IQ(LADDR+MHOSFA),4,OS,8)
            LHOST = LENOCC(HOST)
            LNODE = LENOCC(NODE)
            LOS   = LENOCC(OS)
            IF(LHOST.EQ.0.OR.LNODE.EQ.0.OR.LOS.EQ.0) THEN
               IQUEST(13) = 1
               IRC        = 1
               IF(JVERI) THEN
                  WRITE(LWRITE,*)
     +               'FMVERI. Error - Host name, type or O/S missing'
                  WRITE(LWRITE,*) 'FMVERI. host name: ',HOST(1:LHOST)
                  WRITE(LWRITE,*) 'FMVERI. host type: ',NODE(1:LNODE)
                  WRITE(LWRITE,*) 'FMVERI. host OS  : ',OS(1:LOS)
               ENDIF
            ENDIF
 
            ELSE
*
*        Tape dataset, check VSN/VID, FSEQ
*
               CALL UHTOC(IQ(LADDR+MVSNFA),4,VSN,6)
               CALL UHTOC(IQ(LADDR+MVIDFA),4,VID,6)
               LVSN = LENOCC(VSN)
               JVSN = ICNUMA(VSN,1,6)
               JVID = ICNUMA(VID,1,6)
               LVID = LENOCC(VID)
               IF((LVSN.EQ.0).OR.(LVID.EQ.0).OR.
     +            (JVSN.LT.7).OR.(JVID.LT.7)) THEN
                  IQUEST(13) = 1
                  IRC        = 1
                  IF(JVERI)
     +               WRITE(LWRITE,*) 'FMVERI. Error - VSN or VID ',
     +                   'invalid ',
     +                   VSN(1:LVSN),' ',VID(1:LVID),' (VSN/VID)'
               ENDIF
*
*     New: check that FSEQ < 1000000. This is the limit (I6) in FMSHOW
*
            IF((IQ(LADDR+MFSQFA).LE.0).OR.
     +         (IQ(LADDR+MFSQFA).GT.9999)) THEN
               IQUEST(13) = 1
               IRC        = 1
               IF (JVERI)
     +            WRITE(LWRITE,*) 'FMVERI. Error - FSEQ must be a ',
     +            'positive integer between 1 and 9999 - ',
     +            IQ(LADDR+MFSQFA)
            ENDIF
*
*     Check file size
*
            IF(IQ(LADDR+MFSZFA).GT.MEDSIZ(IQ(LADDR+MMTPFA))) THEN
               IQUEST(13) = 2
               IRC        = 1
               IF (JVERI) WRITE(LWRITE,*)
     +            'FMVERI. Error - file size exceeds',
     +            ' media capacity. File size = ',IQ(LADDR+MFSZFA),
     +            ' media capacity = ',MEDSIZ(IQ(LADDR+MMTPFA))
            ENDIF
 
            IF(IQ(LADDR+MFSZFA).LT.0) THEN
               IQUEST(13) = 3
               IRC        = 1
               IF (JVERI) WRITE(LWRITE,9001)
 9001 FORMAT(' FMVERI. Error - file size must be 0 or positive')
            ENDIF
         ENDIF
 
      ENDIF
 
      IF (IOPTN .NE. 0) THEN
*
*     Name attributes (DSN on disk/tape)
*
         CALL UHTOC(IQ(LADDR+MFQNFA),4,DSN,NFQNFA)
         LDSN = LENOCC(DSN)
 
         IF(LDSN.EQ.0) THEN
            IQUEST(14) = 1
            IRC        = 1
            IF(JVERI) WRITE(LWRITE,*) 'FMVERI. Error - DSN is missing'
         ELSE
*
*     Link (ln) handling
*
            IF(KEYS(MKLCFA).EQ.0) THEN
               IF(DSN(1:2).NE.'//') THEN
                  IRC        = 1
                  IQUEST(14) = 2
                  IF(IVERI) THEN
                     WRITE(LWRITE,*) 'FMVERI. Error - DSN for a link',
     +                  ' must begin with a //'
                  ENDIF
               ENDIF
            ENDIF
 
            LBLANK = INDEX(DSN(1:LDSN),' ')
            IF(LBLANK.NE.0) THEN
               IQUEST(14) = 3
               IF(IVERI) THEN
                  WRITE(LWRITE,*) 'FMVERI. Warning - DSN contains '
     +                   //'a blank - trailing characters ignored. '
     +                   //'You may have problems accessing this file!'
               ENDIF
            ENDIF
 
*
*     URL syntax: protocol://host[:port]/store/class/id
*
            ISLASH = INDEX(DSN(1:LDSN),'/')
            IURL   = INDEX(DSN(1:LDSN),'://')
            IF(IURL.NE.0.AND.IURL.EQ.ISLASH-1) THEN
*
*     check if this system supported (on this node)
*
               CALL FMMSS(GENAM(1:NCH),DSN(1:LDSN),' ',ITEST)
               IF(ITEST.NE.0) THEN
                  IQUEST(14) = 4
                  IF(IVERI) THEN
                     WRITE(LWRITE,9002) GENAM(1:NCH),DSN(1:LDSN)
 9002 FORMAT(' FMVERI. Warning - generic name ',A,' URL ',A,
     +       ' not accessible on this node')
                  ENDIF
               ENDIF
*
*     check media type
*
               IF(KEYS(MKMTFA).NE.1) THEN
                  IRC        = 1
                  IQUEST(14) = 5
                  IF(JVERI) THEN
                     WRITE(LWRITE,*) 'FMVERI. Error - media type must',
     +                  ' be 1 if DSN contains a URL'
                  ENDIF
               ENDIF
*
*     check protocol
*
               IPROT = ICNTHU(DSN(1:IURL-1),CHPOSS,NPOSS)
               IF(IPROT.EQ.0) THEN
                  IRC        = 1
                  IQUEST(14) = 6
                  IF(JVERI) THEN
                     WRITE(LWRITE,9003)
 9003 FORMAT(' FMVERI. Error in protocol ',A)
               WRITE(LPRTFA,9004) CHPOSS
 9004    FORMAT(' FMVERI. following protocols currently permitted: ',
     +          /,(1X,A))
 
                  ENDIF
               ENDIF
*
*     check that URL contains all components
*           protocol://server-node/store-name/storage-class/bfid
*
               LSLASH = INDEXB(DSN(1:LDSN),'/')
               IF(LSLASH.EQ.0) GOTO 20
*
*     BitfileID
*
*              CHBFID = DSN(LSLASH+1:LDSN)
               LDSN   = LSLASH-1
 
               LSLASH = INDEXB(DSN(1:LDSN),'/')
               IF(LSLASH.EQ.0) GOTO 20
*
*     Storage-class
*
*              CHSGRP = DSN(LSLASH+1:LDSN)
               LDSN   = LSLASH-1
 
               LSLASH = INDEXB(DSN(1:LDSN),'/')
               IF(LSLASH.EQ.0) GOTO 20
*
*     Store-name
*
*              CHNAME  = DSN(LSLASH+1:LDSN)
               LDSN    = LSLASH-1
 
               LSLASH = INDEXB(DSN(1:LDSN),'/')
               IF(LSLASH.EQ.0) GOTO 20
*
*     Server-node
*
*              CHSERV  = DSN(LSLASH+1:LDSN)
               LDSN    = LSLASH-1
 
               IF(INDEX (DSN(1:LDSN),'/').NE.
     +            INDEXB(DSN(1:LDSN),'/')) GOTO 20
*
*     All fields exist
*
               GOTO 30
 
   20 CONTINUE
               IRC        = 1
               IQUEST(14) = 7
               IF(JVERI) THEN
                  WRITE(LWRITE,*) 'FMVERI. Error - invalid URL'
               ENDIF
   30 CONTINUE
            ENDIF
         ENDIF
      ENDIF
 
      IF (IOPTO .NE. 0) THEN
*
*     Owner attributes
*
         CALL CFILL(' ',OWNER,1,8)
         CALL UHTOC(IQ(LADDR+MCURFA),4,OWNER,8)
         CALL CFILL(' ',ACCT,1,8)
         CALL UHTOC(IQ(LADDR+MCIDFA),4,ACCT,8)
         CALL CFILL(' ',NODE,1,8)
         CALL UHTOC(IQ(LADDR+MCNIFA),4,NODE,8)
         CALL CFILL(' ',JOB,1,8)
         CALL UHTOC(IQ(LADDR+MCJIFA),4,JOB,8)
 
         IF((LENOCC(OWNER).EQ.0) .OR.
     +      (LENOCC(NODE).EQ.0)  .OR.
     +      (LENOCC(ACCT).EQ.0)  .OR.
     +      (LENOCC(JOB).EQ.0)) THEN
            IQUEST(15) = 1
            IRC        = 1
            IF(JVERI) WRITE(LWRITE,*)
     +      'FMVERI. Error - owner, account, node or job name missing',
     +       OWNER,'/',ACCT,'/',NODE,'/',JOB,' (OWNER/ACCT/NODE/JOB)'
         ENDIF
 
      ENDIF
 
      IF (IOPTP .NE. 0) THEN
*
*     Physical attributes
*
         CALL UHTOC(IQ(LADDR+MRFMFA),4,FLFRM,4)
         LFLFRM = LENOCC(FLFRM)
         IF(LFLFRM.GT.0) THEN
            CALL CLTOU(FLFRM(1:LFLFRM))
            DO 40 I=1,LFLFRM
               IF(INDEX(ALFNUM(1:26),FLFRM(I:I)).EQ.0) THEN
                  IF(JVERI) WRITE(LWRITE,*) 'FMVERI. error - ',
     +               ' record format contains non alphabetic character '
     +               ,FLFRM(I:I)
                  IQUEST(16) = 1
                  IRC = 1
               ENDIF
   40       CONTINUE
         ENDIF
         IF((LFLFRM.EQ.0).OR.
     +   (ICNTH(FLFRM(1:LFLFRM),FRCFM,NRECFM).EQ.0)) THEN
            IQUEST(16) = 1
            IF(IVERI) WRITE(LWRITE,*)
     +      'FMVERI. Warning - record format missing or invalid - ',
     +      FLFRM
            ENDIF
         IF((IQ(LADDR+MRLNFA).LE.0).OR.(IQ(LADDR+MBLNFA).LE.0).OR.
     +      (IQ(LADDR+MFSZFA).LE.0)) THEN
            IQUEST(16) = 1
            IF(IVERI) THEN
               WRITE(LWRITE,*)
     +         'FMVERI. Warning - ',
     +         'record/block size or file size missing or invalid'
               WRITE(LWRITE,*)
     +         IQ(LADDR+MRLNFA),IQ(LADDR+MBLNFA),IQ(LADDR+MFSZFA)
     +         ,' (LRECL,BLOCKSIZE,FILESIZE)'
            ENDIF
         ENDIF
         IF(IQ(LADDR+MRLNFA).GT.FMXREC) THEN
            IRC = 1
            IQUEST(16) = 1
            IF(JVERI) WRITE(LWRITE,*) 'FMVERI. error - record length ',
     +         'exceeds maximum of ',FMXREC,' 32bit words: ',
     +         IQ(LADDR+MRLNFA)
         ENDIF
         IF(IQ(LADDR+MBLNFA).GT.FMXBLK) THEN
            IRC = 1
            IQUEST(16) = 1
            IF(JVERI) WRITE(LWRITE,*) 'FMVERI. error - block length ',
     +         'exceeds maximum of ',FMXBLK,' 32bit words: ',
     +         IQ(LADDR+MBLNFA)
         ENDIF
      ENDIF
#if defined(CERNLIB_TMS)
 
      IF ((IOPTQ .NE. 0) .AND. (IQ(LADDR+MMTPFA) .GT. 1)) THEN
*
*     Set IQUEST(11) to media type in case volume unknown or
*     TMS option not installed.
*
            IQUEST(11) = IQ(LADDR+MMTPFA)
            JP = IQ(LADDR+MVIPFA)
*
*     Convert VID & integer prefix to eXtended VID
*
*
**   V.Fine 19.09.97  this call contained nowhere defined variable 'VIP'
**   I had no idea and define it somehow as CHARACTER*8 VIP
**   Don't ask me why it is *8
*
            CALL FMXVID(VID,JP,XVID,VIP,'C',IRC)
            LXVID = LENOCC(XVID)
            CALL FMQTMS(XVID(1:LXVID),LIB,MODEL,DENS,MNTTYP,LABTYP,IC)
            IF(IC.EQ.100) THEN
               IQUEST(13) = 100
#endif
#if (!defined(CERNLIB_TMSTEST))&&(defined(CERNLIB_TMS))
               IRC        = 1
               IF (JVERI)
     +         WRITE(LWRITE,*) 'FMVERI. Error - VID unknown to TMS'
            ENDIF
#endif
#if (defined(CERNLIB_TMS))&&(defined(CERNLIB_TMSTEST))
               IF (IVERI)
     +         WRITE(LWRITE,*) 'FMVERI. Warning - VID unknown to TMS'
            ENDIF
 
#endif
#if defined(CERNLIB_TMS)
      ENDIF
#endif
 
      IF (IOPTS .NE. 0) THEN
*
*     Security attributes
*
      ENDIF
 
      IF (IOPTT .NE. 0) THEN
*
*     Time attributes
*
         CALL FMUPTM(IDATE,ITIME,IQ(LADDR+MCRTFA),IRET)
         IF(IRET.NE.0) THEN
            IQUEST(20) = 1
            IRC        = 1
            IF(JVERI) WRITE(LWRITE,*)
     +         'FMVERI. Error in date/time created',
     +                        IDATE,ITIME,IQ(LADDR+MCRTFA)
         ENDIF
 
         CALL FMUPTM(IDATE,ITIME,IQ(LADDR+MCTTFA),IRET)
         IF(IRET.NE.0) THEN
            IQUEST(20) = 1
            IRC        = 1
            IF(JVERI) WRITE(LWRITE,*)
     +         'FMVERI. Error in date/time cataloged ',
     +                        IDATE,ITIME,IQ(LADDR+MCTTFA)
         ENDIF
 
         CALL FMUPTM(IDATE,ITIME,IQ(LADDR+MLATFA),IRET)
         IF(IRET.NE.0) THEN
            IQUEST(20) = 1
            IRC        = 1
            IF(JVERI) WRITE(LWRITE,*)
     +         'FMVERI. Error in date/time of last access',
     +      IDATE,ITIME,IQ(LADDR+MLATFA)
         ENDIF
 
      ENDIF
 
      IF (IOPTU .NE. 0) THEN
*
*     User attributes
*
         IQUEST(21) = 1
         DO 50 I=0,9
         IF(IQ(LADDR+MUSWFA+I).EQ.0) GOTO 50
         IQUEST(21) = 0
   50    CONTINUE
         IF((IQUEST(21).NE.0).AND.(IVERI))
     +     WRITE(LWRITE,*) 'FMVERI. Warning - user words are all zero'
 
      ENDIF
 
      IF(IDEBFA.GE.2) WRITE(LWRITE,9005) (IQUEST(I),I=1,26),(I,I=1,26)
 9005 FORMAT(' IQUEST:',/,1X,26I3,/1X,26I3)

      IF(IFMVER.EQ.0.AND.IRC.NE.0) THEN

         WRITE(LWRITE,9006)

 9006 FORMAT(' FMVERI. *** WARNING *** ERRORS DETECTED BUT',
     +       ' VERIFICATION HAS BEEN SUPPRESSED',/,
     +       ' FMVERI. *** RETURN CODE WILL SET TO ZERO')
         IRC = 0

      ENDIF
 
      END
