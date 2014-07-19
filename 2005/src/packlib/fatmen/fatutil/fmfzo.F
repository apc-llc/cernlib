*
* $Id: fmfzo.F,v 1.5 1996/06/26 12:34:40 jamie Exp $
*
* $Log: fmfzo.F,v $
* Revision 1.5  1996/06/26 12:34:40  jamie
* save lenf
*
* Revision 1.4  1996/04/12 07:55:40  cernlib
* new handling of title string
*
* Revision 1.3  1996/03/29 11:29:53  jamie
* qftitlch
*
* Revision 1.2  1996/03/28 10:28:53  jamie
* update idatqq/itimqq and remove check on old version in fminit
*
* Revision 1.1.1.1  1996/03/07 15:18:04  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMFZO(COMM,GNAME,LENTRY,KEYS,IRC)
*
*     Send update via FZOUT to local database server
*     Input is unit for FZ file, Command, Generic name and bank address
*     Command can be 'PUT, DEL, MDIR or DDIR'
*     LENTRY must be non zero for all operations except MDIR/DDIR
*
*     User header of FZ file:
*        IUHEAD(1)      = command
*        IUHEAD(2-70)   = generic name
*        IUHEAD(71-80)  = keys vector
*
*    Monitoring information:
*        IUHEAD(81)     = IHOWFA
*        IUHEAD(82)     = ITIMFA
*
*        IUHEAD(91-155) = CHFNFA
*
#include "fatmen/faust.inc"
#include "fatmen/fausto.inc"
#include "fatmen/fatmon.inc"
#include "fatmen/fatbank.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fatsys.inc"
#include "fatmen/fatupd.inc"
      DIMENSION     LENTRY(1)
      CHARACTER*80  CHFORM
#if defined(CERNLIB_IBMMVS)
      DIMENSION     DISP(3)
      DIMENSION     SPACE(4)
#include "fatmen/fattyp.inc"
#endif
      CHARACTER*(*) COMM, GNAME
      CHARACTER*20  FNAME
      CHARACTER*64  FUNAM
      CHARACTER*80  FILEDEF
      CHARACTER*132 FILEN,FILEM
      CHARACTER*1   OPT
      CHARACTER*2   FMODE
      CHARACTER*8   CHHOST,CHTYPE,CHSYS
#if defined(CERNLIB_UNIX)
      INTEGER       SYSTEMF
#endif
#if defined(CERNLIB_VAXVMS)
      CHARACTER*12  CHUSER
      INTEGER       FMVUSR
#endif
#if !defined(CERNLIB_VAXVMS)
      CHARACTER*8   CHUSER
#endif
      PARAMETER     (NW=80)
      PARAMETER     (MHEAD=200)
      INTEGER       FMHOST,FMUSER
      PARAMETER     (LKEYFA=10)
      DIMENSION     KEYS(LKEYFA)
      DIMENSION     IUHEAD(MHEAD),HEAD(MHEAD),IOCH(NW)
      DIMENSION     IQSAVE(100)
      LOGICAL       IEXIST,IOPEN
      EQUIVALENCE   (IUHEAD(1),HEAD(1))
      SAVE          NENTRY,NSEND,NBATCH,CHHOST,CHTYPE,CHSYS,CHUSER
      SAVE          FILEN,LENF
      DATA          IEV/1/
      DATA          NSEND/0/
      DATA          NBATCH/0/
      DATA          NENTRY/0/
 
      IF(NENTRY.EQ.0) THEN
         NENTRY = 1
         IC = FMHOST(CHHOST,CHTYPE,CHSYS)
#if defined(CERNLIB_VAXVMS)
         IC = FMVUSR(CHUSER)
#endif
#if !defined(CERNLIB_VAXVMS)
         IC = FMUSER(CHUSER)
#endif
         CALL CLTOU(CHHOST)
         CALL CLTOU(CHUSER)
      ENDIF
 
      IF(LUFZFA.EQ.0)
     +   CALL ZFATAM('FATMEN database is R/O - check call to FMINIT')
 
      NCH = LENOCC(GNAME)
*
*     Replace operation from FMOPEN or FMCLOS?
*
      IOPTC = 0
      IOPTO = 0
      IF(COMM(1:3).EQ.'MOD') THEN
         IF(COMM(4:4).EQ.'O') THEN
            IOPTO = 1
            CALL FMACL(CHUSER,CHHOST,GNAME(1:NCH),COMM,'U',IUP)
            IF(IUP.NE.0) THEN
               IF(IDEBFA.GE.2) WRITE(LPRTFA,9001) GNAME(1:NCH)
 9001 FORMAT(' FMFZO. updates turned off for path ',A)
               GOTO 999
            ENDIF
         ENDIF
         IF(COMM(4:4).EQ.'C') IOPTC = 1
         COMM(4:4) = ' '
      ENDIF
      IF(IDEBFA.GE.1) WRITE(LPRTFA,9002) COMM,GNAME(1:NCH)
 9002 FORMAT(' FMFZO. enter for ',A,1X,A)
      IF(IDEBFA.GE.3.AND.COMM.NE.'LOG'.AND.
     +   INDEX(COMM,'DIR').EQ.0) THEN
         WRITE(LPRTFA,9003)
 9003 FORMAT(' FMFZO. output bank...')
         CALL FMSHOW(GNAME,LENTRY,KEYS,'A',IRC)
      ENDIF
*
*     Security
*
      IF(IOPTC+IOPTO.EQ.0.AND.COMM.NE.'LOG') THEN
         CALL FMACL(CHUSER,CHHOST,GNAME(1:NCH),COMM,'A',IRC)
         IF(IRC.NE.0) THEN
            NVIOL = NVIOL + 1
            IF(NVIOL.GT.MAXVIO) CALL ZFATAM
     +        ('Maximum number of security violations exceeded')
            WRITE(LPRTFA,9004) COMM,GNAME(1:NCH)
 9004 FORMAT(' FMFZO. you are not authorised to issue ',A,1X,A)
            GOTO 999
         ENDIF
      ENDIF
*
*     Update protection
*
      NUPDT = NUPDT + 1
      IF(NUPDT.EQ.MAXUPD) THEN
         IF(IDEBFA.GE.0) WRITE(LPRTFA,9005)
 9005 FORMAT(' FMFZO. !!! warning - program will ',
     +      'crash if another FATMEN update is made !!!')
      ENDIF
 
      IF(NUPDT.GT.MAXUPD) CALL ZFATAM
     +  ('Maximum number of updates exceeded')
*
*     Header or header + bank?
*
      OPT = 'S'
      IF(COMM.EQ.'LOG'.OR.INDEX(COMM,'DIR').NE.0) OPT = 'Z'
*
*     Check generic name and build id of local service machine
*
      IF((NCH.LT.3).OR.(GNAME(1:2).NE.'//').OR.
     +   (GNAME(NCH:NCH).EQ.'/')) THEN
         WRITE (6, 9007) GNAME(1:NCH)
         IRC = 1
         GOTO 999
      ENDIF
 
      IFIRST = INDEX(GNAME(3:NCH),'/')
      IF(IFIRST.EQ.0) THEN
         WRITE(6,9007)GNAME(1:NCH)
         IRC = 1
         GOTO 999
      ENDIF
 
      IF(COMM.NE.'LOG') THEN
         ISEC  = INDEX(GNAME(3+IFIRST:NCH),'/')
         IF (ISEC.EQ.0) THEN
            WRITE (6,9007) GNAME(1:NCH)
            IRC = 1
            GOTO 999
         ENDIF
 
         SERNAM = 'FM'//GNAME(3+IFIRST:1+IFIRST+ISEC)
 
      ELSE
 
         SERNAM = 'FM'//GNAME(3+IFIRST:)
 
      ENDIF
 
      LSN    = LENOCC(SERNAM)
*
*     Fill header vector
*
      CALL UCTOH(COMM,IUHEAD,4,4)
*
*     Logging
*
      IF(COMM.EQ.'LOG') THEN
*
*     Set I/O characteristic of header and fill
*
         NHEAD = KLHOLL + KLREAL + KLDATE + KLCMOD +
     +           KLFILE + KLTMS  + KLCFAT
         WRITE(CHFORM,8001) KLHOLL,KLREAL,KLINT
8001  FORMAT(I2,'H',1X,I2,'F',1X,I3,'I')
         CALL MZIOCH(IOCH,NW,CHFORM)
         CALL VBLANK(IUHEAD(2),KLHOLL-1)
         CALL VZERO(IUHEAD(KOREAL),KLREAL+KLINT)
*
*     FATMEN system and group
*
         CALL UCTOH(GNAME(3:),IUHEAD(KFMSYS),4,IFIRST-1)
         CALL UCTOH(GNAME(3+IFIRST:),IUHEAD(KFMGRP),4,NCH-IFIRST-2)
*
*     PAM file title
*
#include "fatmen/qftitlch.inc"
         CALL UCTOH(
     + FatmenTitleFortranString
     +              ,IUHEAD(KFMTIT),4,62)
*
*     Username, node, type, OS
*
         CALL UCTOH(CHUSER,IUHEAD(KFMUSR),4,LENOCC(CHUSER))
         CALL UCTOH(CHHOST,IUHEAD(KFMHST),4,LENOCC(CHHOST))
         CALL UCTOH(CHTYPE,IUHEAD(KFMTYP),4,LENOCC(CHTYPE))
         CALL UCTOH(CHSYS ,IUHEAD(KFMOS ),4,LENOCC(CHSYS ))
*
*     MB read/written
*
           HEAD(KFMMBR) = FATMBR
           HEAD(KFMMBW) = FATMBW
           HEAD(KFZMBR) = FATMZR
           HEAD(KFZMBW) = FATMZW
*
*     MB copied
*
           HEAD(KFMMBC) = FATMBC
           HEAD(KFMMBN) = FATMBN
           HEAD(KFMMBQ) = FATMBQ
*
*     Time stamps
*
      IDATQQ = 960328
      ITIMQQ = 1100
         CALL FMPKTM(IDATQQ,ITIMQQ,IUHEAD(KFMIDQ),IRC)
         CALL DATIME(IDEND,ITEND)
         CALL FMPKTM(NFSTAD,NFSTAT,IUHEAD(KFMIDS),IRC)
         CALL FMPKTM(IDEND ,ITEND ,IUHEAD(KFMIDE),IRC)
*
*     All the other stuff
*
         IUHEAD(KFMADD) = NFADDD
         IUHEAD(KFMADL) = NFADDL
         IUHEAD(KFMADT) = NFADDT
         IUHEAD(KFMMDR) = NFMDIR
         IUHEAD(KFMRDR) = NFRDIR
         IUHEAD(KFMRLN) = NFRLNK
         IUHEAD(KFMRTR) = NFRTRE
         IUHEAD(KFMRMF) = NFRMFL
         IUHEAD(KFMCPF) = NFCPFL
         IUHEAD(KFMMVF) = NFMVFL
         IUHEAD(KFMMOD) = NFMODI
         IUHEAD(KFMTCH) = NFTOUC
         IUHEAD(KFMOPN) = NFOPEN
         IUHEAD(KFMCLS) = NFCLOS
         IUHEAD(KFMCPY) = NFCOPY
         IUHEAD(KFMCPQ) = NFCOPQ
         IUHEAD(KFMCPN) = NFCOPR
         IUHEAD(KFMSRQ) = NFSREQ
         IUHEAD(KFMQVL) = NFQVOL
         IUHEAD(KFMAVL) = NFAVOL
         IUHEAD(KFMASP) = NFASPC
         IUHEAD(KFMPOL) = NFPOOL
         IUHEAD(KFMLCK) = NFLOCK
         IUHEAD(KFMULK) = NFULOK
         IUHEAD(KFMDTG) = NFDTAG
         IUHEAD(KFMGTG) = NFGTAG
         IUHEAD(KFMSTG) = NFSTAG
         IUHEAD(KFMBNK) = NFBANK
         IUHEAD(KFMGET) = NFGET
         IUHEAD(KFMGTK) = NFGETK
         IUHEAD(KFMSHW) = NFSHOW
         IUHEAD(KFMSCN) = NFSCAN
         IUHEAD(KFMLOP) = NFLOOP
         IUHEAD(KFMLDR) = NFLDIR
         IUHEAD(KFMLFL) = NFLFIL
         IUHEAD(KFMSRT) = NFSORT
         IUHEAD(KFMRNK) = NFRANK
         IUHEAD(KFMSLK) = NFSELK
         IUHEAD(KFMMTC) = NFMTCH
 
         IF(IDEBFA.GE.3) CALL FMPLOG(LPRTFA,IUHEAD,NHEAD,IRC)
 
      ELSE
*
*     Fill with blanks for safety
*
      CALL VBLANK(IUHEAD(2),69)
 
      CALL UCTOH(GNAME,IUHEAD(2),4,NCH)
*
*     Keys
*
      LEND  = INDEXB(GNAME,'/') + 1
      FNAME = GNAME(LEND:NCH)
      IUHEAD(71) = KEYS(1)
      IUHEAD(77) = IQ(LENTRY(1)+MCPLFA+KOFUFA)
      IUHEAD(78) = IQ(LENTRY(1)+MMTPFA+KOFUFA)
      IUHEAD(79) = IQ(LENTRY(1)+MLOCFA+KOFUFA)
      IUHEAD(80) = LKEYFA
      LENFN = NCH-LEND+1
*
*     IUHEAD 71-80 contains the keys, which includes the filename
*
      FNAME(LENFN+1:) = ' '
      CALL UCTOH(FNAME,IUHEAD(72),4,20)
*
*     Set up descriptor of header vector
*
      IF(IOPTO.EQ.0) THEN
         CALL MZIOCH(IOCH,NW,'70H 1I 5H 4I')
         NHEAD = 80
      ELSE
         CALL MZIOCH(IOCH,NW,'70H 1I 5H 4I 1B 9I 64H')
*
*     Monitoring information
*
         NHEAD = 155
         IUHEAD(81) = IHOWFA
         IUHEAD(82) = ITIMFA
         CALL VZERO(IUHEAD(83),8)
         CALL VBLANK(IUHEAD(91),64)
         LNFNFA     = LENOCC(CHFNFA)
         CHFNFA(LNFNFA+1:) = ' '
         CALL UCTOH(CHFNFA,IUHEAD(91),4,LNFNFA)
         IF(IDEBFA.GE.3) WRITE(LPRTFA,9006) IHOWFA,ITIMFA,
     +      CHFNFA(1:LNFNFA)
 9006 FORMAT(' FMFZO. IHOWFA: ',Z8,' ITIMFA: ',I6,' CHFNFA: ',A)
      ENDIF
 
      ENDIF
 
 9007 FORMAT(' FMFZO - illegal generic name ',A)
#if defined(CERNLIB_CZ)
      GOTO 20
#endif
*
*     Is the output file already open?
*
      LUNFZ = IABS(LUFZFA)
 
      INQUIRE(LUNFZ,OPENED=IOPEN)
 
      IF(.NOT.IOPEN) THEN
*
*     Get a unique file name
*
   10    CONTINUE
#if defined(CERNLIB_IBMVM)
         IF(FATNOD.EQ.' '.AND.LUFZFA.GT.0) THEN
*
*     Use spool
*
            IF(INDEX(CHHOST,'CERNVMB').EQ.0) THEN
               IF(IDEBFA.GE.2) WRITE(LPRTFA,9008) SERNAM(1:LSN)
 9008 FORMAT(' FMFZO. issuing CP SPOOL PUNCH TO ',A)
               CALL VMCMS('CP SPOOL PUNCH TO '//SERNAM(1:LSN),IRC)
            ELSE
               IF(IDEBFA.GE.2) WRITE(LPRTFA,9008) 'RSCS'
               CALL VMCMS('CP SPOOL PUNCH TO RSCS',IRC)
               IF(IDEBFA.GE.2) WRITE(LPRTFA,9009) SERNAM(1:LSN)
 9009 FORMAT(' FMFZO. issuing CP TAG DEV PUNCH CERNVM ',A)
               CALL VMCMS('CP TAG DEV PUNCH CERNVM ' //SERNAM(1:LSN),
     +         IRC)
            ENDIF
 
            IF(IRC.NE.0) THEN
               WRITE(LPRTFA,*) 'FMFZO - Error from VMCMS, RC=',IRC
               GOTO 999
            ENDIF
 
            WRITE(FILEDEF,9010) LUFZFA
 9010 FORMAT('FILEDEF ',I3,' PUNCH')
            IF(IDEBFA.GE.2) WRITE(LPRTFA,9011) FILEDEF
 9011 FORMAT(' FMFZO. issuing ',A)
            CALL VMCMS(FILEDEF,IRC)
            OPEN(LUFZFA,STATUS='NEW')
            GOTO 20
         ENDIF
 
         CALL FMFNME(FUNAM)
         FILEN = FUNAM // ' ' // LOCALQ(1:1)
         IDOT  = INDEX(FILEN,'.')
         IF(IDOT.NE.0) FILEN(IDOT:IDOT) = ' '
#endif
#if !defined(CERNLIB_IBMVM)
         CALL FMJOUR(FUNAM)
         FILEN = LOCALQ(1:LENOCC(LOCALQ)) // FUNAM
         LENF  = LENOCC(FILEN)
#endif
#if defined(CERNLIB_UNIX)
         CALL CUTOL(FILEN(1:LENF))
#endif
*
*     Does file already exist?
*
#if !defined(CERNLIB_IBM)
         INQUIRE(FILE=FILEN(1:LENF),EXIST=IEXIST)
#endif
#if defined(CERNLIB_IBM)
         INQUIRE(FILE='/'//FILEN(1:LENF),EXIST=IEXIST)
#endif
         IF(IEXIST) THEN
            CALL SLEEPF(1)
            GOTO 10
         ENDIF
*
*     Open output file
*
#if defined(CERNLIB_IBMMVS)
*
*     Create new CARD file
*
         LUNIT = LENOCC(CHMGEN(1))
         CALL FILEINF(ISTAT,'DEVICE',CHMGEN(1)(1:LUNIT),'TRK',1,
     +   'SECOND',1,'DIR',0, 'RECFM','FB','LRECL',80,'BLKSIZE',9040)
#endif
#if defined(CERNLIB_IBM)
         OPEN(LUNFZ,ERR=30 ,STATUS='NEW',ACCESS='SEQUENTIAL', FILE='/'
     +   //FILEN(1:LENF), FORM='FORMATTED',ACTION='READWRITE')
#endif
#if defined(CERNLIB_VAXVMS)
         IF(STRMLF) THEN
*
*     Required to write over NFS from VMS to Unix systems
*
            OPEN(LUNFZ,STATUS='NEW',FILE=FILEN(1:LENF),ERR=30 ,
     +      RECORDTYPE='STREAM_LF', FORM='FORMATTED')
         ELSE
            OPEN(LUNFZ,STATUS='NEW',FILE=FILEN(1:LENF),ERR=30 )
         ENDIF
#endif
#if defined(CERNLIB_UNIX)
         OPEN(LUNFZ,STATUS='NEW',FILE=FILEN(1:LENF),ERR=30 )
#endif
      ENDIF
      NSEND = NSEND + 1
   20 CONTINUE
*
*     Write update
*
#if !defined(CERNLIB_CZ)
      CALL FZFILE(LUNFZ,0,'AO')
      CALL FZLOGL(LUNFZ,MAX(IDEBFA-2,-3))
      IF(IDEBFA.GE.2) WRITE(LPRTFA,9012) COMM,OPT
 9012 FORMAT(' FMFZO. call FZOUT for ',A,' opt ',A)
      CALL FZOUT(LUNFZ,IDIVFA,LENTRY(1),IEV,OPT,IOCH,NHEAD,IUHEAD)
      CALL FZENDO(LUNFZ,'T')
#endif
#if defined(CERNLIB_CZ)
*
*     Send command to remote server
*
      CALL CZPUTA('MESS :OU',ISTAT)
      IF(IDEBFA.GE.2) WRITE(LPRTFA,9012) COMM,OPT
      CALL FZOUT(998,IDIVFA,LENTRY(1),IEV,OPT,IOCH,NHEAD,IUHEAD)
      GOTO 999
#endif
*
*     Send file?
*
      IF(IDEBFA.GE.0) WRITE(LPRTFA,9013) COMM,GNAME(1:NCH)
 9013 FORMAT(' FMFZO - update queued for processing (',A,1X,A,')')
      IF(NSEND.EQ.NGROUP.OR.NGROUP.EQ.0) THEN
         CLOSE(LUFZFA)
         NSEND = 0
         NBATCH = NBATCH + 1
         IF(IDEBFA.GE.0.AND.NGROUP.GT.1) WRITE(LPRTFA,9014) NBATCH
 9014 FORMAT(' FMFZO. sending batch ',I6,' of updates to server')
*
*    Send/rename/mv/XZPUTA,XZMV,XZLRM
*
         CALL FMSEND(FILEN(1:LENF),IRC)
      ENDIF
      GOTO 999
   30 CONTINUE
      WRITE(LPRTFA,9015) FILEN(1:LENF)
 9015 FORMAT(' FMFZO - error opening temporary file, name=',A)
  999 END
 
