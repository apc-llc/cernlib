*
* $Id: fminit.F,v 1.9 2000/01/19 19:32:35 cernsoft Exp $
*
* $Log: fminit.F,v $
* Revision 1.9  2000/01/19 19:32:35  cernsoft
* Identify fatmen with y2k treatment
*
* Revision 1.8  1998/12/01 07:44:19  jamie
* bug in debug printout for unimplemented features
*
* Revision 1.7  1996/08/01 11:10:05  jamie
* Changes to support FMVERI On/Off
*
* Revision 1.6  1996/06/19 06:58:42  jamie
* nunlun->numlun
*
* Revision 1.5  1996/04/12 07:55:47  cernlib
* new handling of title string
*
* Revision 1.4  1996/03/29 11:29:56  jamie
* qftitlch
*
* Revision 1.3  1996/03/29 10:56:05  jamie
* print 'title' more like good-old patchy
*
* Revision 1.2  1996/03/28 10:28:57  jamie
* update idatqq/itimqq and remove check on old version in fminit
*
* Revision 1.1.1.1  1996/03/07 15:18:10  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMINIT(IUSTOR,LNRZ,LNFZ,PATH,IRC)
#if defined(CERNLIB_CZ)
      COMMON/CZSOCK/LUNCZ,IADTCP,LBUF,ISKIN,ISKOUT
#endif
#include "fatmen/fatveri.inc"
#include "fatmen/faust.inc"
#include "fatmen/fstate.inc"
#include "fatmen/fatbank.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fatsys.inc"
#include "fatmen/fatinfo.inc"
#include "fatmen/fatsel.inc"
#include "fatmen/fatout.inc"
#include "fatmen/fatuwd.inc"
#include "fatmen/fatkey.inc"
#include "fatmen/fatloc.inc"
#include "fatmen/fatcpl.inc"
#include "fatmen/fatmtp.inc"
#include "fatmen/fatlun.inc"
#include "fatmen/fattyp.inc"
#include "fatmen/slate.inc"
#include "fatmen/farnge.inc"
#include "fatmen/farstg.inc"
#include "fatmen/fatupd.inc"
#include "fatmen/fmdrep.inc"
#if defined(CERNLIB_CERN)
      DIMENSION MTP(99)
#endif
#if !defined(CERNLIB_CRAY)
      PARAMETER (IQCHAW=4)
#endif
#if defined(CERNLIB_CRAY)
      PARAMETER (IQCHAW=8)
#endif
#if defined(CERNLIB_IBMMVS)
      CHARACTER*8 CHDD
      DIMENSION   DDNAME(2)
      DIMENSION FATCAT(20)
#endif
#if defined(CERNLIB_IBMVM)
      CHARACTER*80  CHFILE
#endif
#if defined(CERNLIB_APOLLO)
#include "fatmen/fatapol3.inc"
#endif
#if defined(CERNLIB_UNIX)
#include "fatmen/fatclio.inc"
#include "fatmen/fatmss.inc"
#include "fatmen/fatget.inc"
#if defined(CERNLIB_SHIFT)
#include "fatmen/fatshift.inc"
#endif
      CHARACTER*255 CHPATH,CHPROG
#endif
      PARAMETER (ISLEEP=60)
      PARAMETER (MAXLEV=20)
      CHARACTER*(*) PATH
      CHARACTER*16  CHTOP
      CHARACTER*80  LINE
      CHARACTER*8   CHHOST,CHTYPE,CHSYS
      CHARACTER*255 CHTEMP
      INTEGER       FMHOST
      SAVE          NENTRY
#include "fatmen/fatsat0.inc"
#include "fatmen/fatmed.inc"
#include "fatmen/fatsat1.inc"
      DATA          NENTRY/0/
      IDATQQ = 991225
      ITIMQQ = 1200
*
*     Set (FM)VERIfication on (0=off)
*
      IFMVER = 1
*
*     Get host information
*
      IF(NENTRY.EQ.0) IC = FMHOST(CHHOST,CHTYPE,CHSYS)
*
*     Allow reentry only if FMEND has been called
*
      IF((MFPHAS.NE.3).AND.(NENTRY.NE.0)) THEN
         PRINT *,'FMINIT. Error - FATMEN has already been initialised'
         RETURN
      ENDIF
*
*     Check on input parameters
*
      IF(LNRZ.LE.0) THEN
         PRINT *,'FMINIT. invalid logical unit specified for ',
     +           'reading FATMEN RZ file - ',LNRZ
         CALL FMSTOP
      ENDIF
 
 
      LEND = LENOCC(PATH)
      IF(LEND.EQ.0) THEN
         PRINT *,'FMINIT. invalid FATMEN database specified'
         CALL FMSTOP
      ENDIF
*
*     Zero counters
*
      NFADDD = 0
      NFADDL = 0
      NFADDT = 0
      NFMDIR = 0
      NFRDIR = 0
      NFRLNK = 0
      NFRTRE = 0
      NFRMFL = 0
      NFCPFL = 0
      NFMVFL = 0
      NFMODI = 0
      NFTOUC = 0
      NFOPEN = 0
      NFCLOS = 0
      NFCOPY = 0
      NFCOPR = 0
      NFCOPQ = 0
      NFSREQ = 0
      NFQVOL = 0
      NFAVOL = 0
      NFASPC = 0
      NFPOOL = 0
      NFLOCK = 0
      NFULOK = 0
      NFDTAG = 0
      NFGTAG = 0
      NFSTAG = 0
      NFBANK = 0
      NFGET  = 0
      NFGETK = 0
      NFSHOW = 0
      NFSCAN = 0
      NFLOOP = 0
      NFLDIR = 0
      NFLFIL = 0
      NFSORT = 0
      NFRANK = 0
      NFSELK = 0
      NFMTCH = 0
      FATMBR = 0.
      FATMBW = 0.
      FATMZR = 0.
      FATMZW = 0.
      FATMBC = 0.
      FATMBN = 0.
      FATMBQ = 0.
 
      CALL DATIME(NFSTAD,NFSTAT)
 
      LENV   = 0
      NRETRY = 0
*
*     Set range count to zero (for [mm:nn] ranges in LD etc.)
*
      DO 10 I=1,MAXLEV
      NFRNGE(I) = 0
   10 CONTINUE
*
*     Clear user words
*
      DO 30 I=1,2
      DO 20 J=1,10
      IFUSER(I,J) = -1
   20 CONTINUE
   30 CONTINUE
*
*     Clear keys matrix and option
*
      CHKEY  = ' '
      NUMKEY = 0
*
*
*     Clear location code, copy level, media type and lun vectors
*
      NUMLOC = 0
      NUMCPL = 0
      NUMMTP = 0
      NUMLUN = 0
 
      DO 40 I=1,KMXLOC
      MFMLOC(I) = -1
   40 CONTINUE
 
      DO 50 I=1,KMXCPL
      MFMCPL(I) = -1
   50 CONTINUE
 
      DO 60 I=1,KMXMTP
      MFMMTP(I) = -1
   60 CONTINUE
 
      DO 70 I=1,KMXLUN
      MFMLUN(I) = -1
      MFMLUA(I) = -1
   70 CONTINUE
*
*     Set default media attributes
*
      DO 80 I=1,NMEDIA
      MFMMED(I) = I
   80 CONTINUE
 
      DO 90 I=1,NMTYP
      MEDSIZ(I) = -1
   90 CONTINUE
 
      LUNRZ  = LNRZ
      LUNFZ  = LNFZ
#if !defined(CERNLIB_IBMVM)
      LUNFZ = IABS(LNFZ)
#endif
      LPRTFA = 6
      OUTPUT = 'TTY'
*
*     Set default ranges of KEYS
*     Media type runs from disk to 3490
*
      MRMTFA(1) = 1
      MRMTFA(2) = 6
*
      MRCLFA(1) = -1
      MRCLFA(2) = -1
*
      MRLCFA(1) = -1
      MRLCFA(2) = -1
*
*     Set default updating parameters
*
      LUFZFA = LNFZ
      CALL FMUPDT(MAX,NGROUP,-1,IRC)
*
*     Set default times for stage server
*
      IWTNET = 60
      IWTACK = 10
      IWTPND = 600
      IWTEXE = 120
*
*     Maximum number of loops for network retries
*
      MAXNET = 60
*
*     Maximum number of loops for acknowledgement
*
      MAXACK = 100
*
*     Security
*
      MAXVIO = 10
      NVIOL  = 0
*
*     Set number of updates
*
      NUPDT = 0
*
*     CHEOPS
*
      LSRCST = 0
      LDSTST = 0
*
*     Data representations
*
      CHDREP(1) = 'IEEE fp, big endian, ascii'
      CHDREP(2) = 'IBM  fp, big endian, ebcdic'
      CHDREP(3) = 'VAX  fp, little endian, ascii'
      CHDREP(4) = 'IEEE fp, little endian, ascii'
      CHDREP(5) = 'CRAY fp, big endian, ascii'
 
      LPATH  = 0
#if defined(CERNLIB_APOLLO)||defined(CERNLIB_UNIX)
      CALL GETENVF('PATH',CHPATH)
      LPATH = IS(1)
#endif
 
      IAPOL3    = 0
#if defined(CERNLIB_APOLLO)
*
*     Determine if we are in the L3 Apollo environment (L3STAGE)
*
      IF(LPATH.GT.0) THEN
*
*     Look for 'l3stage' in the current path
*
         CALL WHICHF(CHPATH(1:LPATH),'l3stage',CHPROG)
         IAPOL3 = IS(1)
         IF(IDEBFA.GE.1.AND.IAPOL3.GT.0)
     +      PRINT *,'FMINIT. l3stage found: ',CHPROG(1:IS(1))
      ENDIF
 
#endif
      ICLIO     = 0
#if defined(CERNLIB_UNIX)
*
*     Determine if we are have the VMSTAGE/CLIO interface
*
      IF(LPATH.GT.0) THEN
*
*     Look for 'vmstage' in the current path
*
         CALL WHICHF(CHPATH(1:LPATH),'vmstage',CHPROG)
         ICLIO    = IS(1)
         IF(IDEBFA.GE.1.AND.ICLIO.GT.0)
     +      PRINT *,'FMINIT. vmstage found: ',CHPROG(1:IS(1))
      ENDIF
 
#endif
      IFMMSS    = 0
#if defined(CERNLIB_UNIX)
*
*     Determine if we are have the mssget/put interface
*
      IF(LPATH.GT.0) THEN
*
*     Look for 'mssget' in the current path
*
         CALL WHICHF(CHPATH(1:LPATH),'mssget',CHPROG)
         IFMMSS   = IS(1)
         IF(IDEBFA.GE.1.AND.IFMMSS.GT.0)
     +      PRINT *,'FMINIT. mssget found: ',CHPROG(1:IS(1))
      ENDIF
 
#endif
      ISFGET    = 0
#if defined(CERNLIB_UNIX)
*
*     Determine if we should use "SFGET"
*
      IF(LPATH.GT.0) THEN
*
*     Look for 'sfget' in the current path
*
         CALL WHICHF(CHPATH(1:LPATH),'sfget',CHPROG)
         ISFGET   = IS(1)
         IF(IDEBFA.GE.1.AND.ISFGET.GT.0)
     +      PRINT *,'FMINIT. sfget found: ',CHPROG(1:IS(1))
      ENDIF
 
#endif
 
#if defined(CERNLIB_SHIFT)
*
*     Location of shift configuration file
*
      CALL GETENVF('PATH_CONFIG',CHPATH)
      LPATH = IS(1)
 
      IF(LPATH.EQ.0) THEN
         SHCONF  = '/etc/shift.conf'
         LSHCONF = 15
         TPCONF  = '/etc/TPCONFIG'
         LTPCONF = 13
      ELSE
         SHCONF  = CHPATH(1:LPATH) // '/etc/shift.conf'
         LSHCONF = LPATH + 17
         TPCONF  = CHPATH(1:LPATH) // '/etc/TPCONFIG'
         LTPCONF = LPATH + 13
      ENDIF
#endif
 
*
*     For each media type (1,2,3,...) set
*         physical device type (disk, 3480, 3420,...) CHMTYP
*         generic  device type (disk, ct1,  tape,...) CHMGEN
*         capacity (MB)        (?, 200, 150,...)      CHMSIZ
*         density              (?, 38K, 6250,...)     CHMDEN
*         mount type           (manual/robotic)       CHMMNT
*         label type           (SL/NL/AL)             CHMLAB
*
#if defined(CERNLIB_FNAL)
*
*     Include 8500s
*
      MRMTFA(2) = 7
*
*     Media definitions for FNAL...
*
      MFMLAB(2) = 'AL'
      MFMLAB(3) = 'AL'
      MFMLAB(4) = 'AL'
      MFMLAB(5) = 'AL'
      MFMLAB(6) = 'AL'
      MFMLAB(7) = 'AL'
 
      MFMGEN(2) = 'CTR '
      MFMGEN(3) = '9TRK'
      MFMGEN(4) = '8200'
      MFMGEN(5) = '8500'
      MFMGEN(6) = '820R'
      MFMGEN(7) = '850R'
 
      MFMMNT(6) = 'R'
      MFMMNT(7) = 'R'
 
      MFMTYP(6) = '8200'
      MFMTYP(7) = '8200'
 
      MFMDEN(6) = '43200'
      MFMDEN(7) = '86400'
 
      NFTYPS    = 7
 
#endif
#if defined(CERNLIB_VMTAPE)
*
*     Generic names for VMTAPE...
*
      MFMGEN(2) = '18TR'
      MFMGEN(3) = '9TR '
#endif
#if defined(CERNLIB_GSI)
*
*     Generic names for GSI...
*
      MFMGEN(1) = 'SYSDA'
      MFMGEN(2) = 'T3480'
      MFMGEN(3) = 'T6250'
#endif
#if defined(CERNLIB_NEWLIB)
*
*     Generic names for DESY...
*
      MFMGEN(1) = 'FAST'
#endif
#if defined(CERNLIB_IBMMVS)
      MFMDEN(3) = '4'
#endif
#if defined(CERNLIB_FPACK)
*
*     Set default space allocation to zero
*
      CALL FMSPAC(0,0,IRC)
#endif
      CALL FMEDIA(MFMMED,MFMTYP,MFMGEN,MFMSIZ,MFMDEN,
     +            MFMMNT,MFMLAB,NFTYPS,IRC)
*
*     Set additional media attributes
*
      DO 100 I=1,NMEDIA
*
*     Maximum number of files
*
         MEDMFL(I) = 999
*
*     High water mark
*
         MEDHWM(I) = MEDSIZ(I) - 20
*
*     Maximum file size
*
         MEDMFS(I) = MEDHWM(I)
  100 CONTINUE
*
*     Get name of server
*
      LSTA   = INDEXB(PATH,'/') + 1
      SERNAM = 'FM'//PATH(LSTA:LEND)
      CALL CLTOU(SERNAM)
      LSN    = LENOCC(SERNAM)
      TOPDIR = PATH(1:LSTA-2)
      CHTOP  = TOPDIR
      FATTOP = PATH(1:LEND)
 
      NENTRY = 1
 
      FATNOD = ' '
 
      CALL VZERO(IDEV,16)
 
      CALL FMLOGL(999)
      IF(IDEBFA.GE.0) THEN
         PRINT *
         PRINT *,'FMINIT.  Initialisation of FATMEN package'
#include "fatmen/qftitlch.inc"
         PRINT *,
     + FatmenTitleFortranString
         PRINT *,'         This version created on ',IDATQQ,
     +           ' at ',ITIMQQ
#if defined(CERNLIB_CZ)
         PRINT *,'         Compiled with Zebra Server switch'
#endif
*
*     Check creation date
*
         CALL DATIME(ID,IT)
 
*        IF(ID.GT.IDATQQ+10000) THEN
*           PRINT *
*           PRINT *,'FMINIT. program is more than one year old'
*           PRINT *,'Please contact the CERN Program Librarian'//
*    +              ' for a new version'
*        ENDIF
      ENDIF
#if defined(CERNLIB_CZ)
      CALL CZOPEN('zserv','cernvm',ISTAT)
      IF (ISTAT .NE. 0) THEN
         IF(IDEBFA.GT.-3)
     +   PRINT *,'Error starting remote server, code = ',ISTAT
         STOP
         ENDIF
      CALL CZPUTA('MESS :   EXEC GIME '//SERNAM(1:LSN)
     +//' 191 F  ',ISTAT)
  110 CONTINUE
      CALL CZGETA(LINE,ISTAT)
      PRINT *,LINE(3:80)
      IF (LINE(1:1) .EQ. '2') GOTO 110
      CALL CZPUTA('MESS :FILE CERN.FATRZ.F',ISTAT)
  120 CONTINUE
      CALL CZGETA(LINE,ISTAT)
      PRINT *,LINE(3:80)
      IF (LINE(1:1) .EQ. '2') GOTO 120
#endif
#if (defined(CERNLIB_IBMMVS))&&(!defined(CERNLIB_CSPACK))
*
*     Get pathname from DDNAME
*
      WRITE(CHDD,9001) LUNRZ
 9001 FORMAT('FT',I2.2,'F001')
      CALL UCTOH(CHDD,DDNAME,4,8)
      CALL VBLANK(FATCAT,20)
      CALL FTINFO(DDNAME,-1,FATCAT,IRC)
      CALL UHTOC(FATCAT,4,DEFAULT,80)
      IF(IRC.EQ.0) THEN
         LDEF = INDEXB(DEFAULT,'.') -1
         LDEF = INDEXB(DEFAULT(1:LDEF),'.') -1
         DEFAULT(LDEF+1:) = ' '
      ELSE
         PRINT *,'FMINIT. ddname FATMEN not set. '
         PRINT *,'        FATMEN catalogue will be inaccessible '
         IRC = 28
         RETURN
      ENDIF
#endif
#if (defined(CERNLIB_IBMMVS))&&(defined(CERNLIB_CSPACK))
*
*     DEFAULT, FATNOD are hard-coded
*
#endif
#if (defined(CERNLIB_IBMMVS))&&(defined(CERNLIB_CSPACK))&&(defined(CERNLIB_DSYIBM))
*
*     Catalogue server is FATmen for HERa (father)
*
      FATNOD  = 'father'
      LFATND  = 6
      DEFAULT = '/fatmen/'//SERNAM(1:LSN)
      LDEF    = LSN + 8
#endif
#if (defined(CERNLIB_VAXVMS))&&(!defined(CERNLIB_CZ))
      IF(IDEBFA.GE.2) PRINT *,'FMINIT. getting value of symbol ',
     +   SERNAM(1:LSN)
#endif
#if (defined(CERNLIB_UNIX))&&(!defined(CERNLIB_CZ))
      IF(IDEBFA.GE.2) PRINT *,'FMINIT. getting value of variable ',
     +   SERNAM(1:LSN)
#endif
#if (defined(CERNLIB_UNIX)||defined(CERNLIB_VAXVMS))&&(!defined(CERNLIB_CZ))
      DEFAULT = ' '
      CALL GETENVF(SERNAM(1:LSN),DEFAULT)
      LDEF = IS(1)
      LENV = IS(1)
      IF(LDEF.EQ.0) THEN
#endif
#if (defined(CERNLIB_UNIX))&&(!defined(CERNLIB_CZ))
         PRINT *,'FMINIT. warning - environmental variable ',
#endif
#if (defined(CERNLIB_VAXVMS))&&(!defined(CERNLIB_CZ))
         PRINT *,'FMINIT. warning - symbol ',
#endif
#if (defined(CERNLIB_VAXVMS)||defined(CERNLIB_UNIX))&&(!defined(CERNLIB_CZ))
     +           SERNAM(1:LSN),' not set. '
         PRINT *,'        FATMEN catalogue will be inaccessible '
#endif
#if (defined(CERNLIB_VAXVMS)||defined(CERNLIB_UNIX))&&(!defined(CERNLIB_CZ))
     +          ,'unless in current directory'
         CALL GETWDF(DEFAULT)
         LDEF = IS(1)
      ELSE
*
*     Handle node:path case
*
         ICOLON = INDEX(DEFAULT(1:LDEF),':')
         IF(ICOLON.NE.0) THEN
            IF(DEFAULT(ICOLON:ICOLON+1).NE.'::'.AND.
     +         DEFAULT(ICOLON:ICOLON+1).NE.':[') THEN
#endif
#if (defined(CERNLIB_VAXVMS)||defined(CERNLIB_UNIX))&&(!defined(CERNLIB_CZ))&&(!defined(CERNLIB_CSPACK))
               WRITE(LPRTFA,9002) SERNAM(1:LSN),DEFAULT(1:LDEF)
 9002  FORMAT(' FMINIT. ',A,' points to ',A)
               WRITE(LPRTFA,9003)
 9003  FORMAT(' FMINIT. FATMEN has not been built with the CSPACK ',
     +        'option. Program stopped')
               STOP 16
#endif
#if (defined(CERNLIB_VAXVMS)||defined(CERNLIB_UNIX))&&(!defined(CERNLIB_CZ))&&(defined(CERNLIB_CSPACK))
               FATNOD = DEFAULT(1:ICOLON-1)
               LFATND = ICOLON - 1
               CHTEMP = DEFAULT(ICOLON+1:LDEF)
               DEFAULT = CHTEMP(1:LDEF-ICOLON)
               LDEF   = LDEF - ICOLON
               IF(IDEBFA.GE.1) WRITE(LPRTFA,9004) SERNAM(1:LSN),
     +            DEFAULT(1:LDEF),FATNOD(1:LFATND)
 9004 FORMAT(' FMINIT. ',A,' points to directory ',A,' on node ',A)
               CALL XZINIT(LPRTFA,IDEBFA,LUNFZ,LUNFZ)
#endif
#if (defined(CERNLIB_VAXVMS)||defined(CERNLIB_UNIX))&&(!defined(CERNLIB_CZ))
            ENDIF
         ELSE
            IF(IDEBFA.GE.1) PRINT *,'FMINIT. ',SERNAM(1:LSN),
     +         ' points to directory ',DEFAULT(1:LDEF)
         ENDIF
      ENDIF
#endif
#if (defined(CERNLIB_IBMVM))&&(!defined(CERNLIB_CZ))
*
*     Link to disk of specified service machine
*
      SERMOD = '?'
      CALL VMCMS('EXEC GIME '//SERNAM//
     +'(QUIET NONOTICE STACK)',IRC)
      IF (IRC .LE. 4) THEN
         CALL VMRTRM(LINE,LEN)
         SERMOD = LINE(1:1)
         IF(IDEBFA.GE.0)
     +   PRINT *,'Linked to ',SERNAM,' mode ',SERMOD
      ELSEIF(IRC.EQ.104) THEN
         IF(IDEBFA.GT.-3)
     +   PRINT *,'FMINIT. Invalid userid. Check call to FMINIT'
         NENTRY = 0
         RETURN
      ELSE
         IF(IDEBFA.GT.-3)
     +   PRINT *,'FMINIT. Error code ',IRC,' from EXEC GIME',
     +           ' type FIND GIME for a list of return codes'
         NENTRY = 0
         RETURN
      ENDIF
 
      CALL FMONIT('Init FATMEN')
#endif
 
#if !defined(CERNLIB_CSPACK)
*
*     Check if the catalogue exists. If not, give the user
*     another chance.
*
  130 CONTINUE
      CALL FAEXST(IRC)
      IF(IRC.NE.0.AND.LENV.EQ.0) THEN
         PRINT *,'FMINIT. FATMEN catalogue does not exist. ',
     +           'Check call to FMINIT.'
         NENTRY = 0
         RETURN
      ELSEIF(IRC.NE.0.AND.LENV.NE.0) THEN
         NRETRY = NRETRY + 1
         IF(IDEBFA.GE.1) THEN
            IF(NRETRY.EQ.1) THEN
               WRITE(LPRTFA,9005) DEFAULT(1:LDEF),NRETRY,ISLEEP
            ELSE
               WRITE(LPRTFA,9006) NRETRY,ISLEEP
            ENDIF
         ENDIF
 9005 FORMAT(' FMINIT. catalogue not found in ',A,'.',/,
     +       '         Retry number ',I10,' in ',I3,' seconds')
 9006 FORMAT(' FMINIT. Retry number ',I10,' in ',I3,' seconds')
         CALL SLEEPF(ISLEEP)
         GOTO 130
      ENDIF
#endif
*
*     Find place to write update files
*
      LOCALQ = ' '
#if defined(CERNLIB_VAXVMS)||defined(CERNLIB_UNIX)
*
*     Write directly to server directly unless
*     access remote catalogue using CSPACK
*
      IF(FATNOD.NE.' ') THEN
         CALL GETENVF('FATQUEUE',LOCALQ)
      ELSE
         LOCALQ = DEFAULT
         IS(1)  = LDEF
      ENDIF
      IF(IS(1).EQ.0) THEN
#endif
#if defined(CERNLIB_UNIX)
         CALL GETENVF('HOME',LOCALQ)
         LOCALQ(IS(1)+1:) = '/'
#endif
#if defined(CERNLIB_VAXVMS)
         LOCALQ = 'SYS$LOGIN:'
#endif
#if defined(CERNLIB_VAXVMS)||defined(CERNLIB_UNIX)
      ELSE
#endif
#if defined(CERNLIB_VAXVMS)
         LOCALQ = DEFAULT(1:LDEF-1) // '.TODO' // DEFAULT(LDEF:LDEF)
#endif
#if defined(CERNLIB_UNIX)
         LOCALQ = DEFAULT(1:LDEF)   // '/todo/'
#endif
#if defined(CERNLIB_VAXVMS)||defined(CERNLIB_UNIX)
      ENDIF
#endif
#if defined(CERNLIB_IBMVM)
      CALL MAXDSK(LOCALQ,NFREE,IRC)
#endif
#if defined(CERNLIB_IBMMVS)
      CALL KPREFI(LOCALQ,LQUEUE)
      LOCALQ(LQUEUE+1:) = '.FATMEN'
#endif
 
      IF(MFPHAS.EQ.3) THEN
         IJSTOR = -1
      ELSE
         IJSTOR = IUSTOR
      ENDIF
 
      CALL FATINI(IJSTOR,LUNRZ,LUNFZ,CHTOP,'    ')
      IRC=IQUEST(1)
#if (defined(CERNLIB_IBMVM))&&(!defined(CERNLIB_CSPACK))
      IF(IRC.EQ.0) THEN
         INQUIRE(LUNRZ,NAME=CHFILE)
         LCH = LENOCC(CHFILE)
         LBL = INDEXB(CHFILE(1:LCH),' ') + 1
*
*     Check that catalogue is on disk linked to by GIME
*
         IF(CHFILE(LBL:LBL).NE.SERMOD) THEN
            IF(IDEBFA.GE.-3) THEN
               PRINT *,'FMINIT. !!!!!!!!!! warning - ',
     +         'using FATMEN catalogue ',CHFILE(2:LCH),
     +         ' Disk mode returned by GIME = ',SERMOD
            ENDIF
         ENDIF
*
*     Check mode - should be 6 = update in place
*
         IF(CHFILE(LCH:LCH).NE.'6') THEN
            IF(IDEBFA.GE.-3) PRINT *,'FMINIT. warning - ',
     +         'FATMEN catalogue is not mode 6.  Mode = ',
     +         CHFILE(LBL:LCH)
            IF(IDEBFA.GE.-3) PRINT *,
     +         '        Updates may not be visible !!!'
         ENDIF
      ENDIF
#endif
#if defined(CERNLIB_CERN)
*
*     Fill media type vector (for selection)
*     The order is:
*        DISK, 3490, 3480, 3420, 8200, 8500, DAT60, DAT90, DLT2
*        1   , 6   , 2   , 3   , 4   , 5   , 7    , 8    , 12
*
      NMTP   = 9
      MTP(1) = 1
      MTP(2) = 6
      MTP(3) = 2
      MTP(4) = 3
      MTP(5) = 4
      MTP(6) = 5
      MTP(7) = 7
      MTP(8) = 8
      MTP(9) = 12
      CALL FMSETM(MTP,NMTP,ICODE)
#endif
*
*     Load location code definitions
*
      CALL FMLOCC(IRCODE)
*
*     and media type definitions
*
      CALL FMMEDT(IRCODE)
*
*     Set program phase
*
      MFPHAS = 1
      END
