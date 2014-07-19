*
* $Id: fatare.F,v 1.1.1.1 1996/03/07 15:18:07 mclareni Exp $
*
* $Log: fatare.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:07  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FATARE(GENAM,LFAT,KEYS,CHLINE,CHOPT,IRC)
#include "fatmen/fatpara.inc"
#include "fatmen/fatbank.inc"
#include "fatmen/fmnkeys.inc"
      CHARACTER*(*) GENAM,CHLINE,CHOPT
      DIMENSION     KEYS(LKEYFA)
      CHARACTER*1   CHACT,CHSTG
      CHARACTER*6   VSN,VID
      CHARACTER*20  CHDEV
      CHARACTER*15  XVID
      CHARACTER*80  CARD
      CHARACTER*255 EQUNAM
      LOGICAL       IEXIST
#if defined(CERNLIB_SHIFT)
#include "fatmen/fatshift.inc"
#endif
#include "fatmen/tmsdef0.inc"
#include "fatmen/fatmed.inc"
#include "fatmen/tmsdef1.inc"
 
      IRC   = 0
      LBANK = LFAT + KOFUFA
*
*     Query TMS for this VID - if TMS option is not installed
*     FMQTMS will return defaults.
*
      CALL UHTOC(IQ(LBANK+MVSNFA),4,VSN,6)
      CALL UHTOC(IQ(LBANK+MVIDFA),4,VID,6)
      IQUEST(11) = IQ(LBANK+MMTPFA)
      IFILE      = IQ(LBANK+MFSQFA)
#if defined(CERNLIB_PREFIX)
      CALL FMXVID(VID,IQ(LBANK+MVIPFA),XVID,VIP,'C',ITMS)
      LXVID = LENOCC(XVID)
      CALL FMQTMS(XVID(1:LXVID),LIB,MODEL,DENS,MNTTYP,LABTYP,ITMS)
#endif
#if !defined(CERNLIB_PREFIX)
      CALL FMQTMS(VID,LIB,MODEL,DENS,MNTTYP,LABTYP,ITMS)
#endif
#if !defined(CERNLIB_TMS)
      IF(ITMS.EQ.99) ITMS = 0
#endif
      IF(ITMS.EQ.100) THEN
         CHACT = '?'
      ELSEIF(ITMS.EQ.312.OR.ITMS.EQ.315) THEN
         CHACT = 'N'
      ELSE
         CHACT = 'Y'
      ENDIF
 
      CHSTG = 'N'
*
*     Check whether required device exists on this node
*
      LM = LENOCC(MODEL)
      IDEV = 1
      IF(ITMS.EQ.100) THEN
         CHDEV = 'unknown'
      ELSE
#if (defined(CERNLIB_VAXVMS))&&(defined(CERNLIB_VAXTAP))
         CALL FMGTLG('SETUP_'//MODEL(1:LM)//'S',EQUNAM, 'LNM$SYSTEM',
     +   IDEV)
         IF(IDEV.EQ.0) CHDEV = 'local'
*
*     If logical name does not exist, or in the case of remote
*     libraries, read also setup configuration file
*
         INQUIRE(FILE='SETUP_EXE:TPSERV.CONF',EXIST=IEXIST)
         IF(IDEV.NE.0.AND.IEXIST) THEN
 
            ISTAT = LIB$GET_LUN(LUNTAP)
#include "fatmen/fatvaxrc.inc"
            OPEN(LUNTAP,FILE='SETUP_EXE:TPSERV.CONF', FORM='FORMATTED',
     +      STATUS='OLD', READONLY,SHARED,IOSTAT=ISTAT)
            IF(ISTAT.NE.0) THEN
               IF(IDEBFA.GE.0) PRINT *,'FATARE. cannot open TPSERV',
     +         ' configuration file (SETUP_EXE:TPSERV.CONF)'
            ELSE
   10          CONTINUE
               READ(LUNTAP,'(A)',END=20) CARD
               LCARD = LENOCC(CARD)
               IF(INDEX(CARD(1:LCARD),'TPSERV').NE.0.AND.
     +         INDEX(CARD(1:LCARD),MODEL(1:LM)).NE.0) THEN
                  IF(IDEBFA.GE.1) PRINT *,'FATARE. ',CARD(1:LCARD)
                  CHDEV = 'served'
                  IDEV = 0
                  GOTO 20
               ENDIF
               GOTO 10
   20          CONTINUE
               CLOSE(LUNTAP)
            ENDIF
            ISTAT = LIB$FREE_LUN(LUNTAP)
#include "fatmen/fatvaxrc.inc"
 
         ENDIF
#endif
#if defined(CERNLIB_IBMVM)
*
*     Node specific restrictions - just check for defined media types
*
         JDEV = ICNTH(MODEL(1:LM),MFMGEN,NMEDIA)
         IF(JDEV.EQ.0) THEN
            IDEV = 1
            CHDEV = 'unavailable'
         ELSE
            IDEV = 0
            CHDEV = 'local'
         ENDIF
#endif
#if (defined(CERNLIB_IBMVM))&&(defined(CERNLIB_CERN))
*
*     SMCF, SMCG
*
         IF(MODEL(1:3).EQ.'SMC') THEN
            IDEV = 0
            CHDEV = 'local'
         ENDIF
#endif
#if defined(CERNLIB_SHIFT)||defined(CERNLIB_CRAY)
*
*     Read /etc/shift.conf file
*
         INQUIRE(FILE=SHCONF(1:LSHCONF),EXIST=IEXIST)
         IF(IEXIST) THEN
            CALL CIOPEN(LUNPTR,'r',SHCONF(1:LSHCONF),ISTAT)
            IF(ISTAT.NE.0) THEN
               IF(IDEBFA.GE.0) PRINT *,'FATARE. cannot open SHIFT ',
     +         'configuration file: ',SHCONF(1:LSHCONF)
            ELSE
   30          CONTINUE
               CALL FMCFGL(LUNPTR,CARD,LCARD,' ',ISTAT)
               IF(ISTAT.NE.0) GOTO 40
               IF(IDEBFA.GE.3) PRINT *,'FATARE. read line ', CARD(1:
     +         LCARD),' from: ',SHCONF(1:LSHCONF)
               IF(INDEX(CARD(1:LCARD),'TPSERV').NE.0.AND.
     +         INDEX(CARD(1 :LCARD),MODEL(1:LM)).NE.0) THEN
                  IF(IDEBFA.GE.1) PRINT *,'FATARE. ',CARD(1:LCARD)
                  IDEV = 0
                  CHDEV = 'served'
                  GOTO 40
               ENDIF
               GOTO 30
   40          CONTINUE
               CALL FMCFGL(LUNPTR,CARD,LCARD,'F',ISTAT)
               CALL CICLOS(LUNPTR)
            ENDIF
         ENDIF
 
#endif
#if defined(CERNLIB_SHIFT)
*
*      If device type not found, check in local TPCONFIG file
*
         IF(IDEV.NE.0) THEN
            INQUIRE(FILE='/etc/TPCONFIG',EXIST=IEXIST)
            IF(IEXIST) THEN
               CALL CIOPEN(LUNPTR,'r','/etc/TPCONFIG',ISTAT)
               IF(ISTAT.NE.0) THEN
                  IF(IDEBFA.GE.0) PRINT *,'FATARE. cannot open tape ',
     +            'daemon configuration file (/etc/TPCONFIG)'
               ELSE
   50             CONTINUE
                  CALL FMCFGL(LUNPTR,CARD,LCARD,' ',ISTAT)
                  IF(ISTAT.NE.0) GOTO 60
                  IF(IDEBFA.GE.3) PRINT *,'FATARE. read line ',
     +            CARD(1: LCARD),' from /etc/TPCONFIG'
                  IF(INDEX(CARD(1:LCARD),MODEL(1:LM)).NE.0) THEN
                     CHDEV = 'local'
                     IDEV = 0
                     GOTO 60
                  ENDIF
                  GOTO 50
   60             CONTINUE
                  CALL FMCFGL(LUNPTR,CARD,LCARD,'F',ISTAT)
                  CALL CICLOS(LUNPTR)
               ENDIF
            ENDIF
         ENDIF
#endif
         IF(IDEV.EQ.1) CHDEV = 'unavailable'
      ENDIF
*
*     For volumes in active libraries, check if staged
*
      CHSTG = 'N'
      ISTG  = 1
 
      IF(CHACT.EQ.'Y') THEN
         CALL FMSTGQ(VSN,VID,IFILE,LABTYP,ISTG)
         IF(ISTG.EQ.0) CHSTG = 'Y'
      ENDIF
 
      WRITE(CHLINE,9001) CHACT,CHSTG,CHDEV
9001  FORMAT('Active: ',A1,' staged: ',A1,' device: ',A)
 
      IRC = IABS(ITMS) + IABS(IDEV) + IABS(ISTG)
      END
