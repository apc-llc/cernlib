*
* $Id: fmclos.F,v 1.1.1.1 1996/03/07 15:18:10 mclareni Exp $
*
* $Log: fmclos.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:10  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMCLOS(GNAME,CHLUN,LBANK,CHOPT,IRC)
*
*     Close logical unit opened by FMOPEN and optionally
*     write updates to Zebra bank associated with this file.
*
#include "fatmen/faust.inc"
#if defined(CERNLIB_VAXVMS)
      CHARACTER*16    RECTYP
#endif
#if defined(CERNLIB_APOLLO)
#include "fatmen/fatapol3.inc"
#endif
#include "fatmen/fatstg.inc"
#include "fatmen/fattyp.inc"
#include "fatmen/fatbank.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fatinfo.inc"
#include "fatmen/fatmon.inc"
#include "fatmen/fmnkeys.inc"
      DIMENSION     KEYS(LKEYFA)
      CHARACTER*80  STGCOM,CHLINE
      CHARACTER*255 GENAM,COMAND,CHFILE
      CHARACTER*(*) CHLUN,GNAME,CHOPT
      CHARACTER*4   FZOPT
      CHARACTER*8   DDNAME
      CHARACTER*6   CDEN
      CHARACTER*6   CHREC,CHBLK
      CHARACTER*4   RECFM
      CHARACTER*40  DCB
      CHARACTER*256 DSN
#if defined(CERNLIB_IBMMVS)
      CHARACTER*8   CHFUNC
      CHARACTER*255 CHPARM
      CHARACTER*20  CHLEVL
#endif
#if !defined(CERNLIB_CRAY)
      PARAMETER (IQCHAW=4)
#endif
#if defined(CERNLIB_CRAY)
      PARAMETER (IQCHAW=8)
#endif
      PARAMETER (MEGA=1024*1024)
      CHARACTER*6   VSN,VID,FSEQ
      CHARACTER*15  XVID
      CHARACTER*8   VIP
      CHARACTER*2   LABEL
      COMMON/FZSTAT/INFLUN,INFSTA,INFOFZ(40)
      CHARACTER*4   FFORM,FTEMP
      CHARACTER*12  CHDIR
      LOGICAL       IOPEN
#if defined(CERNLIB_UNIX)
      INTEGER       SYSTEMF
#endif
#include "fatmen/tmsdef0.inc"
#include "fatmen/fatlab0.inc"
#include "fatmen/fatoptd.inc"
#include "fatmen/tmsdef1.inc"
#include "fatmen/fatlab1.inc"
#include "fatmen/fatoptc.inc"
 
      NFCLOS = NFCLOS + 1
*
*     Check options
*
      LGN = LENOCC(GNAME)
      GENAM = GNAME(1:LGN)
      IF(IDEBFA.GE.3) WRITE(LPRTFA,9001) GENAM(1:LGN),CHLUN,LBANK,CHOPT
 9001 FORMAT(' FMCLOS. enter for ',A,1X,A,1X,I10,1X,A)
*
*     Is this entry a link?
*
      IF(LBANK.NE.0.AND.IQ(LBANK+KOFUFA+MLOCFA).EQ.0) THEN
         CALL UHTOC(IQ(LBANK+KOFUFA+MFQNFA),4,GENAM,NFQNFA)
         NCH = LENOCC(GENAM)
         IF(IDEBFA.GE.0) WRITE(LPRTFA,9002) GNAME(1:LGN),GENAM(1:NCH)
 9002 FORMAT(' FMCLOS. ',A,' --> ',A)
         LGN = NCH
         CALL VZERO(KEYS,LKEYFA)
         CALL FMGET(GENAM,LADDR,KEYS,IRC)
         IF (IRC.NE.0) GOTO 999
      ELSE
         LADDR = LBANK
      ENDIF
*
*     CHOPT: C - issue STAGE CLEAR
*            D - issue STAGE DROP / EXEC DROP / DEASSIGN / DISMOUNT
*            E - call FZENDx
*            F - call FZINFO
*            N - do not issue FORTRAN or other close statement
*            P - issue STAGE PUT
*            U - update FATMEN catalogue with bank at LADDR
*            W - wait for the STAGE PUT to complete
*            Z - call MZDROP
*
      CALL FMCHOP('FMCLOS. ',CHOPT,'CDEFNPUWZ',IC)
 
      IF(LBANK.EQ.0) IOPTP = 0
*
*     Monitoring information
*
      IHOWFA = 0
      ITIMFA = 0
      CHFNFA = ' '
      IRC    = 0
      ICLOSE = IOPTN
      IUNIT  = 0
      LCH    = LENOCC(CHLUN)
      IF(LCH.LE.2) THEN
         IF (LCH .EQ. 1) THEN
            READ(CHLUN,9003) IUNIT
         ELSEIF(LCH .EQ. 2) THEN
            READ(CHLUN,9004) IUNIT
         ENDIF
 
 9003 FORMAT(I1)
 9004 FORMAT(I2)
         CALL FML2DD(IUNIT,DDNAME,IRC)
*
*     FATMEN file format
*
         IF(LADDR.NE.0) THEN
            CALL UHTOC(IQ(LADDR+KOFUFA+MFLFFA),4,FFORM,4)
            LFORM = LENOCC(FFORM)
#if defined(CERNLIB_FPACK)
*
*     FPACK
*
            IF(FFORM(1:2).EQ.'FP') THEN
               CHFILE = CHLUN
               IF(IDEBFA.GE.0) WRITE(LPRTFA,9005) CHFILE(1:LCH)
 9005 FORMAT(' FMCLOS. call FPARM for CLOSE ',A)
               CALL FPARM('CLOSE '//CHFILE(1:LCH))
               CALL FERMES(COMAND,1)
               IRC    = LENOCC(COMAND)
               IF(IRC.NE.0.AND.IDEBFA.GE.-3) WRITE(LPRTFA,9006)
     +            COMMAND(1:IRC)
 9006 FORMAT(' FMOPEN. error from FPARM = ',A)
               RETURN
            ENDIF
 
*
*     New Zebra uses FORTRAN I/O as default
*
#endif
#if defined(CERNLIB_IBMVM)
            IF((FFORM(1:2).EQ.'FX').AND.
     +         ((IUNIT.NE.0).AND.(JFMODE(IUNIT).EQ.2))) THEN
               DDNAME = 'VM00F001'
               WRITE(DDNAME(3:4),'(I2.2)') IUNIT
            ENDIF
#endif
 
            IF((INDEX(CHLUN(1:LCH),'IOFILE').EQ.0).AND.
     +         ((IUNIT.NE.0).AND.(JFMODE(IUNIT).NE.3))) THEN
               IF(FFORM(1:2).EQ.'FX') THEN
                  FTEMP = FFORM
                  FFORM = 'F'//FTEMP(1:LFORM)
               ENDIF
            ENDIF
 
            IF((FFORM(1:2).EQ.'FX').OR.(FFORM(1:2).EQ.'EP')) THEN
               DDNAME = 'IOFILE00'
               WRITE(DDNAME(7:8),'(I2.2)') IUNIT
            ENDIF
         ENDIF
 
      ELSE
         DDNAME = CHLUN
      ENDIF
 
      LDD = LENOCC(DDNAME)
*
*     Get logical unit from DDNAME
*
      IF(DDNAME(1:2).EQ.'FT') THEN
*
*     IBM FORTRAN
*
         READ(DDNAME(3:4),'(I2)') LUN
      ELSEIF(DDNAME(1:6).EQ.'IOFILE') THEN
*
*     IOPACK
*
         READ(DDNAME(7:8),'(I2)') LUN
      ELSEIF(DDNAME(1:2).EQ.'VM') THEN
*
*     VMIO
*
         READ(DDNAME(3:4),'(I2)') LUN
      ELSEIF(DDNAME(1:3).EQ.'FOR') THEN
*
*     VAX FORTRAN
*
         READ(DDNAME(4:6),'(I3)') LUN
      ELSEIF(DDNAME(1:5).EQ.'fort.') THEN
*
*     Unix FORTRAN
*
         IF(LDD.EQ.6) THEN
            READ(DDNAME(6:6),'(I1)') LUN
         ELSE
            READ(DDNAME(6:7),'(I2)') LUN
         ENDIF
      ENDIF
 
      IF(JFMODE(LUN).EQ.1) ICLOSE = 1
 
      IF(IOPTF.NE.0) THEN
*
*     Get Zebra FZ information
*
         CALL FZINFO(LUN)
         IF(INFLUN.NE.LUN) THEN
            IF(IDEBFA.GE.0) WRITE(LPRTFA,9007) LUN
 9007 FORMAT(' FMCLOS. Error obtaining call FZINFO for LUN = ',I10)
            MBYTES = 0
            JSRDFA = 0
            JERDFA = 0
            JSBLFA = 0
            JEBLFA = 0
         ELSE
*
*     FZ defines a MB as a million bytes...
*
            MBYTES = (INFOFZ(19) + INFOFZ(20)/MEGA)*IQCHAW
            MBYTES = MBYTES * 1000000 / MEGA
            JSRDFA = 1
            JERDFA = INFOFZ(21)
            JSBLFA = 1
            JEBLFA = INFOFZ(22)
 
         ENDIF
 
         IF(IFMODE(LUN).EQ.0) THEN
            FATMZR = FATMZR + MBYTES
         ELSE
            FATMZW = FATMZW + MBYTES
         ENDIF
 
      ENDIF
 
      IF(IOPTE.NE.0) THEN
*
*     Terminate option for FZ. For C I/O, use X option for CFCLOS
*
         FZOPT = 'T'
 
         IF(JFMODE(LUN).EQ.1) THEN
            FZOPT = 'TX'
         ENDIF
 
         IF(IFMODE(LUN).EQ.0) THEN
            IF(IDEBFA.GE.2) WRITE(LPRTFA,9008) LUN,FZOPT
 9008 FORMAT(' FMCLOS. call FZENDI for LUN=',I6,' CHOPT = ',A)
            CALL FZENDI(LUN,FZOPT)
         ELSEIF(IFMODE(LUN).EQ.1) THEN
            IF(IDEBFA.GE.2) WRITE(LPRTFA,9009) LUN,FZOPT
 9009 FORMAT(' FMCLOS. call FZENDO for LUN=',I6,' CHOPT = ',A)
            CALL FZENDO(LUN,FZOPT)
         ELSEIF(IFMODE(LUN).EQ.2) THEN
            IF(LUN.LT.10)WRITE(CHDIR,9010)LUN
            IF(LUN.GE.10)WRITE(CHDIR,9011)LUN
 9010       FORMAT('LUN',I1,'    ')
 9011       FORMAT('LUN',I2,'   ')
            IF(IDEBFA.GE.2) WRITE(LPRTFA,9012) LUN,CHDIR
 9012 FORMAT(' FMCLOS. call RZEND for LUN=',I6,' CHDIR=',A)
            CALL RZEND(CHDIR)
         ENDIF
      ENDIF
 
      IF(ICLOSE.EQ.0) THEN
 
         IF(IDEBFA.GE.0) WRITE(LPRTFA,9013) LUN
 9013    FORMAT(' FMCLOS. closing file on unit ',I5)
*
*     Issue VMCLOS if CHLUN has a VMIO like pattern
*
         IF(DDNAME(1:2).EQ.'VM') THEN
#if defined(CERNLIB_IBMVM)
            IF(IDEBFA.GE.2) WRITE(LPRTFA,9014)
 9014 FORMAT(' FMCLOS. issuing VMCLOS')
            CALL VMCLOS(LUN,IRC,NBLOCK)
            IF(IRC.NE.0) WRITE(LPRTFA,9015) IRC
 9015 FORMAT(' FMCLOS. return code ',I6,' from VMCLOS')
*
*     Issue IOCLOS if CHLUN has a IOPACK like pattern
*
         ELSEIF(DDNAME(1:6).EQ.'IOFILE') THEN
            CALL IOCLOS(LUN,IRC)
#endif
         ELSE
#if defined(CERNLIB_VAXVMS)
*
*     Check if we are using C I/O
*
            CALL FMGDSN(LADDR,DSN,LDSN,IRC)
            INQUIRE(FILE=DSN(1:LDSN),RECORDTYPE=RECTYP)
            IF(INDEX(RECTYP,'STREAM_LF').EQ.0.AND.
     +         INDEX(FFORM,'FX').NE.0) THEN
*
*     Medium
*
               IF(LFMODE(LUN).EQ.3) THEN
                  MEDIUM = 1
               ELSE
                  MEDIUM = 0
               ENDIF
               IF(IDEBFA.GE.2) WRITE(LPRTFA,9016)
 9016 FORMAT(' FMCLOS. issuing CFCLOS')
               CALL CFCLOS(IFPNTR(LUN),MEDIUM)
            ELSE
               INQUIRE(LUN,OPENED=IOPEN)
               IF(IOPEN) THEN
                  IF(IDEBFA.GE.2) WRITE(LPRTFA,9017)
 9017 FORMAT(' FMCLOS. issuing FORTRAN close')
                  CLOSE(UNIT=LUN)
#endif
#if (defined(CERNLIB_VAXVMS))&&(defined(CERNLIB_CSPACK))
               ELSE
                  IF(IDEBFA.GE.2) WRITE(LPRTFA,9018)
 9018 FORMAT(' FMCLOS. issuing XZCLOS')
                  CALL XZCLOS(LUN,' ',IRC)
#endif
#if defined(CERNLIB_VAXVMS)
               ENDIF
            ENDIF
#endif
#if (!defined(CERNLIB_SHIFT))&&(!defined(CERNLIB_VAXVMS))
            INQUIRE(LUN,OPENED=IOPEN)
            IF(IOPEN) THEN
               IF(IDEBFA.GE.2) WRITE(LPRTFA,9019)
 9019 FORMAT(' FMCLOS. issuing FORTRAN close')
               CLOSE(UNIT=LUN)
#endif
#if (defined(CERNLIB_CSPACK))&&(!defined(CERNLIB_SHIFT))&&(!defined(CERNLIB_VAXVMS))
            ELSE
               IF(IDEBFA.GE.2) WRITE(LPRTFA,9020)
 9020 FORMAT(' FMCLOS. issuing XZCLOS')
               CALL XZCLOS(LUN,' ',IRC)
#endif
#if (!defined(CERNLIB_SHIFT))&&(!defined(CERNLIB_VAXVMS))
            ENDIF
#endif
#if defined(CERNLIB_SHIFT)
            IF(IDEBFA.GE.2) WRITE(LPRTFA,9021)
 9021 FORMAT(' FMCLOS. issuing XYCLOS')
            CALL XYCLOS(LUN,' ',IRC)
            IF(IDEBFA.GE.0) WRITE(LPRTFA,9022) IRC
 9022 FORMAT(' FMCLOS. return code ',I6,' from XYCLOS')
#endif
         ENDIF
 
      ENDIF
 
*
*     IOPTC
*
      IF(IOPTC.NE.0) THEN
         IF(LFMODE(LUN).EQ.2) THEN
#if (defined(CERNLIB_UNIX))&&(defined(CERNLIB_SHIFT))
            WRITE(COMAND,9023) DDNAME
 9023 FORMAT(' stageclr -P ',A)
            LCOMM = LENOCC(COMAND)
            IF(IDEBFA.GE.0) WRITE(LPRTFA,9024) COMAND(1:LCOMM)
 9024 FORMAT(' FMCLOS. executing ',A)
            IRC = SYSTEMF(COMAND(1:LCOMM))
            IF(IRC.NE.0) GOTO 999
#endif
#if defined(CERNLIB_IBMVM)
            IF(IDEBFA.GE.0) PRINT *,
     +      'FMCLOS. Executing EXEC STAGE DROP '//DDNAME//' CLEAR'
            CALL VMCMS('EXEC STAGE DROP '//DDNAME//' (CLEAR',IRC)
*
*     For input tapes, we have to say STAGE CLEAR as well
*
            IF((IFMODE(LUN).EQ.0).AND.(LADDR.NE.0)) THEN
               CALL UHTOC(IQ(LADDR+KOFUFA+MVSNFA),4,VSN,6)
               LVSN = LENOCC(VSN)
               CALL CLTOU(VSN)
               CALL UHTOC(IQ(LADDR+KOFUFA+MVIDFA),4,VID,6)
               LVID = LENOCC(VID)
               CALL CLTOU(VID)
               CALL FMITOC(IQ(LADDR+KOFUFA+MFSQFA),FSEQ,LFSEQ)
*
*     Set IQUEST(11) to media type in case volume unknown or
*     TMS option not installed.
*
               IQUEST(11) = IQ(LADDR+KOFUFA+MMTPFA)
#endif
#if (defined(CERNLIB_PREFIX))&&(defined(CERNLIB_IBMVM))
               CALL FMXVID(VID,IQ(LADDR+KOFUFA+MVIPFA),XVID,VIP,'C',IC)
               LXVID = LENOCC(XVID)
               CALL FMQTMS(XVID(1:LXVID),
     +                     LIB,MODEL,DENS,MNTTYP,LABTYP,IC)
#endif
#if (!defined(CERNLIB_PREFIX))&&(defined(CERNLIB_IBMVM))
               CALL FMQTMS(VID(1:LVID),LIB,MODEL,DENS,MNTTYP,LABTYP,IC)
#endif
#if defined(CERNLIB_IBMVM)
               CALL CLTOU(LABTYP)
               LLAB = LENOCC(LABTYP)
               CALL VMCMS('EXEC STAGE CLEAR '//VSN(1:LVSN) // '.'
     +   // FSEQ(1:LFSEQ)
#endif
#if (defined(CERNLIB_PREFIX))&&(defined(CERNLIB_IBMVM))
     +   // '.' // LABTYP(1:LLAB) // '.' // VID(1:LVID)
     +   // '.' // VIP(1:LENOCC(VIP)),IRC)
#endif
#if (!defined(CERNLIB_PREFIX))&&(defined(CERNLIB_IBMVM))
     +   // '.' // LABTYP(1:LLAB) // '.' // VID(1:LVID),IRC)
#endif
#if defined(CERNLIB_IBMVM)
            ENDIF
#endif
#if defined(CERNLIB_VAXVMS)
 
            IF(IDEBFA.GE.0) PRINT *,
     +      'FMCLOS. Executing LIB$DELETE_FILE of '//DDNAME(1:LDD)
            ISTAT = LIB$DELETE_FILE(DDNAME(1:LDD))
#include "fatmen/fatvaxrc.inc"
#endif
#if (defined(CERNLIB_IBMMVS))&&(defined(CERNLIB_DSYIBM))
*
*     Issue STAGE DELETE call
*
            CHFUNC = 'DELETE  '
            CALL UHTOC(IQ(LADDR+KOFUFA+MFQNFA),4,DSN,NFQNFA)
            LDSN = LENOCC(DSN)
            LBLANK = INDEX(DSN,' ')
            IF(LBLANK.NE.0) LDSN = LBLANK
 
            IF(IDEBFA.GE.3) THEN
               CHLEVL = 'PRINTLEV=ALL       ;'
            ELSEIF(IDEBFA.EQ.2) THEN
               CHLEVL = 'PRINTLEV=TRACE     ;'
            ELSEIF(IDEBFA.EQ.1) THEN
               CHLEVL = 'PRINTLEV=WARNING   ;'
            ELSEIF(IDEBFA.LE.-3) THEN
               CHLEVL = 'PRINTLEV=NOMESSAGE ;'
            ELSE
               CHLEVL = 'PRINTLEV=ERROR     ;'
            ENDIF
 
            CHPARM(LPARM:LPARM) = ';'
 
            CALL STAGE(CHFUNC,IRC,ICODE,
     +                 'DSN='//DSN(1:LDSN)//' ;',CHLEVL)
            IF(IRC.NE.0) THEN
               IF(IDEBFA.GE.-3) PRINT *,'FMCLOS. return code ',IRC,
     +            ' from STAGE, reason code = ',ICODE
            ENDIF
 
#endif
         ELSEIF(LFMODE(LUN).EQ.3) THEN
#if defined(CERNLIB_IBMVM)
            IF(IDEBFA.GE.0) PRINT *,'FMCLOS. Detaching tape unit'
            ITAPE = 179 + IVADDR(LUN)
            IF(IVADDR(LUN).GT.8) ITAPE = 287 + IVADDR(LUN)
            CALL CFILL(' ',STGCOM,1,80)
            WRITE(STGCOM,'(A,I3)') 'CP DETACH ',ITAPE
            CALL VMCMS(STGCOM,IRC)
*
*     Flag tape unit as no longer used
*
            IDEV(IVADDR(LUN)) = 0
#endif
#if (defined(CERNLIB_IBMVM))&&(defined(CERNLIB_TAPESYS))
            IF(LADDR.NE.0) THEN
               CALL UHTOC(IQ(LADDR+KOFUFA+MVSNFA),4,VSN,6)
               LVSN = LENOCC(VSN)
               CALL CLTOU(VSN)
               CALL UHTOC(IQ(LADDR+KOFUFA+MVIDFA),4,VID,6)
               LVID = LENOCC(VID)
               STGCOM = 'EXEC TAPESYS CANCEL '//VID(1:LVID)
               IF(VSN(1:LVSN).NE.VID(1:LVID)) THEN
                  LCOM = LENOCC(STGCOM)
                  STGCOM = STGCOM(1:LCOM) // ' ( EXTID'
               ENDIF
 
               LCOM = LENOCC(STGCOM)
 
               IF(IDEBFA.GE.0) PRINT *,'FMCLOS. executing ',
     +            STGCOM(1:LCOM)
               CALL VMCMS(STGCOM(1:LCOM),IRC)
            ENDIF
#endif
#if (defined(CERNLIB_IBMVM))&&(defined(CERNLIB_SETUP))
            IF(IDEBFA.GE.0) PRINT *,'FMCLOS. executing SETUP CLEAR'
            CALL VMCMS('SETUP CLEAR',IRC)
#endif
         ELSE
            IF(IDEBFA.GE.0)
     +      PRINT *,'FMCLOS. Clear option not valid for disk datasets'
         ENDIF
 
      ENDIF
*
*     IOPTP
*
      IF(IOPTP.NE.0.AND.IQ(LADDR+KOFUFA+MMTPFA).GT.1) THEN
         IF(LFMODE(LUN).EQ.2) THEN
#if (defined(CERNLIB_UNIX))&&(defined(CERNLIB_SHIFT))
               WRITE(COMAND,9025) IUNIT
 9025 FORMAT(' stageput -G -U ',I2)
               LCOMM = LENOCC(COMAND)
               IF(IDEBFA.GE.0) WRITE(LPRTFA,9026) COMAND(1:LCOMM)
 9026 FORMAT(' FMCLOS. executing ',A)
               IRC = SYSTEMF(COMAND(1:LCOMM))
               IF(IRC.NE.0) GOTO 999
#endif
#if (defined(CERNLIB_VAXVMS))&&(defined(CERNLIB_VAXTAP))
               CALL UHTOC(IQ(LADDR+KOFUFA+MFQNFA),4,DSN,NFQNFA)
               LDSN = LENOCC(DSN)
               LBLANK = INDEX(DSN,' ')
               IF(LBLANK.NE.0) LDSN = LBLANK
               CALL UHTOC(IQ(LADDR+KOFUFA+MVSNFA),4,VSN,6)
               LVSN = LENOCC(VSN)
               CALL CLTOU(VSN)
               CALL UHTOC(IQ(LADDR+KOFUFA+MVIDFA),4,VID,6)
               LVID = LENOCC(VID)
               CALL CLTOU(VID)
               CALL FMITOC(IQ(LADDR+KOFUFA+MFSQFA),FSEQ,LFSEQ)
*
*     Get DCB information
*
               CALL UHTOC(IQ(LADDR+KOFUFA+MRFMFA),4,RECFM,4)
               LRECL = IQ(LADDR+KOFUFA+MRLNFA)*4
               LBLOCK = IQ(LADDR+KOFUFA+MBLNFA)*4
*
*     Set IQUEST(11) to media type in case volume unknown or
*     TMS option not installed.
*
               IQUEST(11) = IQ(LADDR+KOFUFA+MMTPFA)
#endif
#if (defined(CERNLIB_PREFIX))&&(defined(CERNLIB_VAXVMS))&&(defined(CERNLIB_VAXTAP))
               CALL FMXVID(VID,IQ(LADDR+KOFUFA+MVIPFA),XVID,VIP,'C',IC)
               LXVID = LENOCC(XVID)
               CALL FMQTMS(XVID(1:LXVID),LIB,MODEL,DENS,MNTTYP,LABTYP,
     +                     IC)
               IF(IDEBFA.GE.3) THEN
                  PRINT *,'FMCLOS. return from FMQTMS with ', XVID,'/',
     +            LIB,'/',MODEL,'/',DENS,'/',MNTTYP,'/', LABTYP,'/',IC
               ENDIF
#endif
#if (!defined(CERNLIB_PREFIX))&&(defined(CERNLIB_VAXVMS))&&(defined(CERNLIB_VAXTAP))
               CALL FMQTMS(VID(1:LVID),LIB,MODEL,DENS,MNTTYP,LABTYP,IC)
               IF(IDEBFA.GE.3) THEN
                  PRINT *,'FMCLOS. return from FMQTMS with ', VID,'/',
     +            LIB,'/',MODEL,'/',DENS,'/',MNTTYP,'/', LABTYP,'/',IC
               ENDIF
#endif
#if (defined(CERNLIB_VAXVMS))&&(defined(CERNLIB_VAXTAP))
 
               CDEN = CHMDEN(IQ(LADDR+KOFUFA+MMTPFA))
               IF(IC.EQ.0) CDEN = DENS
               CALL CLTOU(LABTYP)
*
*     Translate IBM to VAX labels (SL->EBCDIC etc.)
*
               JL = ICNTH(LABTYP,IBMLAB,3)
               STGCOM = '$STAGE/WRITE ' // VSN(1:LVSN) // ' '
     +                  // VID(1:LVID) // ' '
     +                  // '/NAME=' // DSN(1:LDSN)
     +                  // '/NUMBER=' // FSEQ(1:LFSEQ)
     +                  // '/GENERIC='// MODEL
     +                  // '/LABEL='//   VAXLAB(JL)
               IF(INDEX(RECFM,'F').NE.0) THEN
                  STGCOM = STGCOM(1:LENOCC(STGCOM)) // '/FIXED'
               ELSEIF(INDEX(RECFM,'V').NE.0) THEN
                  STGCOM = STGCOM(1:LENOCC(STGCOM)) // '/VARIABLE'
               ENDIF
               WRITE(CHREC,'(I6.6)')  LRECL
               WRITE(CHBLK, '(I6.6)') LBLOCK
               IF(LRECL.GT.0) STGCOM = STGCOM(1:LENOCC(STGCOM)) //
     +           '/RECORDSIZE='//CHREC
               IF(LBLOCK.GT.0) STGCOM = STGCOM(1:LENOCC(STGCOM)) //
     +           '/BLOCKSIZE='//CHBLK
*
               LENCOM = LENOCC(STGCOM)
*
 
#endif
#if (defined(CERNLIB_VAXVMS))&&(defined(CERNLIB_VAXTAP))&&(!defined(CERNLIB_TMS))
*
*     Add density
*
               STGCOM = STGCOM(1:LENCOM) // '/DENSITY=' // DENS
               LENCOM = LENOCC(STGCOM)
#endif
#if (defined(CERNLIB_VAXVMS))&&(defined(CERNLIB_VAXTAP))
               CALL CSQMBL(COMAND,1,LENCOM)
               LENCOM = LENOCC(COMAND)
               IF(IDEBFA.GE.0) PRINT *,'FMCLOS. executing ',
     +            COMAND(1:LENCOM)
*
               IC = SYSTEMF(COMAND(1:LENCOM))
 
#endif
#if defined(CERNLIB_SHIFT)
               CALL UHTOC(IQ(LADDR+KOFUFA+MFQNFA),4,DSN,NFQNFA)
               LDSN = LENOCC(DSN)
               LBLANK = INDEX(DSN,' ')
               IF(LBLANK.NE.0) LDSN = LBLANK
               CALL UHTOC(IQ(LADDR+KOFUFA+MVSNFA),4,VSN,6)
               LVSN = LENOCC(VSN)
               CALL CLTOU(VSN)
               CALL UHTOC(IQ(LADDR+KOFUFA+MVIDFA),4,VID,6)
               LVID = LENOCC(VID)
               CALL CLTOU(VID)
*
*     Get DCB information
*
               CALL UHTOC(IQ(LADDR+KOFUFA+MRFMFA),4,RECFM,4)
               LRECL = IQ(LADDR+KOFUFA+MRLNFA)*4
               LBLOCK = IQ(LADDR+KOFUFA+MBLNFA)*4
*
*     Set IQUEST(11) to media type in case volume unknown or
*     TMS option not installed.
*
               IQUEST(11) = IQ(LADDR+KOFUFA+MMTPFA)
#endif
#if (defined(CERNLIB_PREFIX))&&(defined(CERNLIB_SHIFT))
               CALL FMXVID(VID,IQ(LADDR+KOFUFA+MVIPFA),XVID,VIP,'C',IC)
               LXVID = LENOCC(XVID)
               CALL FMQTMS(XVID(1:LXVID),LIB,MODEL,DENS,MNTTYP,LABTYP,
     +                     IC)
               IF(IDEBFA.GE.3) THEN
                  PRINT *,'FMCLOS. return from FMQTMS with ', XVID,'/',
     +            LIB,'/',MODEL,'/',DENS,'/',MNTTYP,'/', LABTYP,'/',IC
               ENDIF
#endif
#if (!defined(CERNLIB_PREFIX))&&(defined(CERNLIB_SHIFT))
               CALL FMQTMS(VID(1:LVID),LIB,MODEL,DENS,MNTTYP,LABTYP,IC)
               IF(IDEBFA.GE.3) THEN
                  PRINT *,'FMCLOS. return from FMQTMS with ', VID,'/',
     +            LIB,'/',MODEL,'/',DENS,'/',MNTTYP,'/', LABTYP,'/',IC
               ENDIF
#endif
#if defined(CERNLIB_SHIFT)
 
               CALL CUTOL(LABTYP)
               CDEN = CHMDEN(IQ(LADDR+KOFUFA+MMTPFA))
               IF(IC.EQ.0) CDEN = DENS
               CALL FMITOC(IQ(LADDR+KOFUFA+MFSQFA),FSEQ,LFSEQ)
               COMAND = 'stageout  -G -U '//CHLUN(1:LCH)
     +            // ' -v '//VSN(1:LVSN)// ' -V '
     +            //VID(1:LVID)//' -l '//LABTYP//' -g '//MODEL
     +            // ' -q ' //FSEQ(1:LFSEQ) // ' -f '//DSN(1:LDSN)
               LENCOM = LENOCC(COMAND)
               WRITE(DCB,9027) RECFM,LRECL,LBLOCK
 9027          FORMAT(' -F ',A,' -L ',I5,' -b ',I5)
               COMAND = COMAND(1:LENOCC(COMAND)) // DCB
               LENCOM = LENOCC(COMAND)
 
#endif
#if (defined(CERNLIB_SHIFT))&&(!defined(CERNLIB_TMS))
*
*     Add density
*
               COMAND = COMAND(1:LENCOM) // ' -d ' // DENS
               LENCOM = LENOCC(COMAND)
#endif
#if defined(CERNLIB_SHIFT)
               CALL CSQMBL(COMAND,1,LENCOM)
               LENCOM = LENOCC(COMAND)
               IF(IDEBFA.GE.0) PRINT *,'FMCLOS. executing ',
     +            COMAND(1:LENCOM)
*
               IRC = SYSTEMF(COMAND(1:LENCOM))
 
#endif
#if defined(CERNLIB_IBMVM)
         IF(IOPTW.NE.0) THEN
         IF(IDEBFA.GE.0) PRINT *, 'FMCLOS. Executing EXEC STAGE PUT '
     +         // DDNAME,' (WAIT)'
            CALL VMCMS('EXEC STAGE PUT '//DDNAME//' (WAIT)',IRC)
         ELSE
            IF(IDEBFA.GE.0) PRINT *, 'FMCLOS. Executing EXEC STAGE PUT '
     +         // DDNAME
            CALL VMCMS('EXEC STAGE PUT '//DDNAME,IRC)
         ENDIF
 
         IF(IRC.NE.0) THEN
            IF(IDEBFA.GE.-3) WRITE(LPRTFA,9028) IRC
 9028 FORMAT(' FMCLOS. return code ',I10,' from STAGE PUT')
            GOTO 999
         ENDIF
*
*     STAGE PUT does a drop
*
         IOPTD = 0
#endif
#if defined(CERNLIB_CRAY)
 
         IF(IDEBFA.GE.0) PRINT *, 'FMCLOS. Executing stageput '//
     +   DDNAME
         IC = SYSTEMF('stageput '//DDNAME)
#endif
#if defined(CERNLIB_APOLLO)
         IF(IAPOL3.NE.0) THEN
      CALL UHTOC(IQ(LADDR+KOFUFA+MFQNFA),4,DSN,NFQNFA)
      LDSN = LENOCC(DSN)
      LBLANK = INDEX(DSN,' ')
      IF(LBLANK.NE.0) LDSN = LBLANK
            CALL UHTOC(IQ(LADDR+KOFUFA+MVSNFA),4,VSN,6)
            LVSN = LENOCC(VSN)
            CALL CLTOU(VSN)
            CALL UHTOC(IQ(LADDR+KOFUFA+MVIDFA),4,VID,6)
            LVID = LENOCC(VID)
            CALL CLTOU(VID)
            CALL FMITOC(IQ(LADDR+KOFUFA+MFSQFA),FSEQ,LFSEQ)
*
*     Get DCB information
*
            CALL UHTOC(IQ(LADDR+KOFUFA+MRFMFA),4,RECFM,4)
            LRECL = IQ(LADDR+KOFUFA+MRLNFA)*4
            LBLOCK = IQ(LADDR+KOFUFA+MBLNFA)*4
*
*     Set IQUEST(11) to media type in case volume unknown or
*     TMS option not installed.
*
            IQUEST(11) = IQ(LADDR+KOFUFA+MMTPFA)
#endif
#if (defined(CERNLIB_PREFIX))&&(defined(CERNLIB_APOLLO))
            CALL FMXVID(VID,IQ(LADDR+KOFUFA+MVIPFA),XVID,VIP,'C',IC)
            LXVID = LENOCC(XVID)
            CALL FMQTMS(XVID(1:LXVID),LIB,MODEL,DENS,MNTTYP,LABTYP,IC)
            IF(IDEBFA.GE.3) THEN
               PRINT *,'FMCLOS. return from FMQTMS with ', XVID,'/',LIB,
     +         '/',MODEL,'/',DENS,'/',MNTTYP,'/', LABTYP,'/',IC
            ENDIF
#endif
#if (!defined(CERNLIB_PREFIX))&&(defined(CERNLIB_APOLLO))
            CALL FMQTMS(VID(1:LVID),LIB,MODEL,DENS,MNTTYP,LABTYP,IC)
            IF(IDEBFA.GE.3) THEN
               PRINT *,'FMCLOS. return from FMQTMS with ', VID,'/',LIB,
     +         '/',MODEL,'/',DENS,'/',MNTTYP,'/', LABTYP,'/',IC
            ENDIF
#endif
#if defined(CERNLIB_APOLLO)
 
            CDEN = CHMDEN(IQ(LADDR+KOFUFA+MMTPFA))
            IF(IC.EQ.0) CDEN = DENS
            CALL CLTOU(LABTYP)
            STGCOM = 'l3stage -o '
     +           // ' -v '
     +           //VID(1:LVID)//' -l '//LABTYP//' -t '//MODEL
     +           // ' -f ' //FSEQ(1:LFSEQ)// ' -d '//CDEN
     +           // ' -n ' //DSN(1:LDSN)
            LENSTG = LENOCC(STGCOM)
            LENPTH = LENOCC(STGPTH)
            WRITE(DCB,9029) RECFM,LRECL,LBLOCK
 9029       FORMAT(' -r ',A,' -c ',I5,' -b ',I5)
            CALL FMFNME(CHFILE)
            LCHF = LENOCC(CHFILE)
            IF(IDEBFA.GE.3) PRINT *,'FMCLOS. output of STAGE command ',
     +      'will be sent to /tmp/'//CHFILE(1:LCHF)
            COMAND =
     +         STGCOM(1:LENSTG)//DCB//STGPTH(1:LENPTH)
     +            // ' > /tmp/'//CHFILE(1:LCHF)
            LENCOM = LENOCC(COMAND)
 
            CALL CSQMBL(COMAND,1,LENCOM)
            LENCOM = LENOCC(COMAND)
            IF(IDEBFA.GE.0) PRINT *,'FMCLOS. executing ',
     +         COMAND(1:LENCOM)
            ISTAT = 0
   10       CONTINUE
*
            IC = SYSTEMF(COMAND(1:LENCOM))
 
*     Check output of stage command
*
            OPEN(LUN,FILE='/tmp/'//CHFILE(1:LCHF),STATUS='OLD',
     +         FORM='FORMATTED')
   20       READ(LUN,'(A)',END=30 ) CHLINE
            LCHL = LENOCC(CHLINE)
            IF(IDEBFA.GE.0) PRINT *,'FMCLOS. ',CHLINE(1:LCHL)
            IF(INDEX(CHLINE(1:LCHL),'path : ').NE.0) THEN
               ISTART = INDEX(CHLINE(1:LCHL),'/')
               STGPTH = CHLINE(ISTART:)
            ELSEIF(INDEX(CHLINE(1:LCHL),'stat : ').NE.0) THEN
               ISTAT = 1
               IF(INDEX(CHLINE(1:LCHL),'ABORTED').NE.0) THEN
                  IRC = -1
                  RETURN
               ELSEIF(INDEX(CHLINE(1:LCHL),'ENDED_OK').NE.0) THEN
                  GOTO 30
               ENDIF
            ENDIF
            GOTO 20
   30       CLOSE(LUN)
            IF(ISTAT.EQ.0) THEN
               IF(IDEBFA.GE.-3) PRINT *,'FMCLOS. no status return ',
     +            'from stage. Will retry in 60 seconds'
               CALL SLEEPF(60)
               GOTO 10
            ENDIF
 
      ENDIF
#endif
      ENDIF
      ENDIF
*
*     IOPTD
*
      IF(IOPTD.NE.0) THEN
         IF(LFMODE(LUN).EQ.1) THEN
#if defined(CERNLIB_IBMVM)
            IF(INDEX(CHMODE(LUN),'A').NE.0) THEN
               IF(IDEBFA.GE.0) PRINT *,
     +         'FMCLOS. DROP ignored for mode '//CHMODE(LUN)
            ELSE
               IF(IDEBFA.GE.0) PRINT *,
     +         'FMCLOS. Executing DROP '//CHMODE(LUN)
               CALL VMCMS('EXEC DROP '//CHMODE(LUN),IRC)
            ENDIF
#endif
#if defined(CERNLIB_VAXVMS)
            IF(IDEBFA.GE.0) PRINT *,
     +      'FMCLOS. Executing $DEASSIGN '//DDNAME(1:LDD)
            ISTAT = LIB$SPAWN('$DEASSIGN '//DDNAME(1:LDD))
#include "fatmen/fatvaxrc.inc"
#endif
         ELSEIF(LFMODE(LUN).EQ.2) THEN
#if (defined(CERNLIB_SHIFT))&&(defined(CERNLIB_UNIX))
            IF(IDEBFA.GE.0) PRINT 9030
 9030 FORMAT(' FMCLOS. removing symbolic link(s)')
               CALL UNLINKF(DDNAME)
               IF(LADDR.NE.0) THEN
                  CALL UHTOC(IQ(LADDR+KOFUFA+MVIDFA),4,VID,6)
                  LVID = LENOCC(VID)
                  CALL CLTOU(VID)
                  CALL FMITOC(IQ(LADDR+KOFUFA+MFSQFA),FSEQ,LFSEQ)
                  CALL UNLINKF('T'//VID(1:LVID)//'.FSEQ'//FSEQ(1:LFSEQ))
               ENDIF
#endif
#if defined(CERNLIB_IBMVM)
            IF(IDEBFA.GE.0) PRINT *, 'FMCLOS. Executing STAGE DROP '//
     +      DDNAME
            CALL VMCMS('EXEC STAGE DROP '//DDNAME,IRC)
#endif
         ELSEIF(LFMODE(LUN).EQ.3) THEN
#if defined(CERNLIB_IBMVM)
            IF(IDEBFA.GE.0) PRINT *,'FMCLOS. Detaching tape unit'
            ITAPE = 179 + IVADDR(LUN)
            IF(IVADDR(LUN).GT.8) ITAPE = 287 + IVADDR(LUN)
            CALL CFILL(' ',STGCOM,1,80)
            WRITE(STGCOM,'(A,I3)') 'CP DETACH ',ITAPE
            CALL VMCMS(STGCOM,IRC)
*
*     Flag tape unit as no longer used
*
            IDEV(IVADDR(LUN)) = 0
            IF(IDEBFA.GE.0) PRINT *,'FMCLOS. executing SETUP CLEAR'
            CALL VMCMS('SETUP CLEAR',IRC)
#endif
#if defined(CERNLIB_VAXVMS)
            ISTAT = LIB$SPAWN('$DISMOUNT '//DDNAME(1:LDD))
#include "fatmen/fatvaxrc.inc"
#endif
         ENDIF
 
      ENDIF
*
*     IOPTU
*
      IF(IOPTU.NE.0) THEN
 
         IF (LADDR.NE.0) THEN
            IF(IOPTF.NE.0) THEN
               IQ(LADDR+KOFUFA+MFSZFA) = MBYTES
               IQ(LADDR+KOFUFA+MSRDFA) = JSRDFA
               IQ(LADDR+KOFUFA+MERDFA) = JERDFA
               IQ(LADDR+KOFUFA+MSBLFA) = JSBLFA
               IQ(LADDR+KOFUFA+MEBLFA) = JEBLFA
            ENDIF
            CALL FMREPL(GENAM,LADDR,'C',IRC)
         ELSE
            IF(IDEBFA.GE.0) PRINT *,
     +      'FMCLOS. Bank address is zero, update request ignored'
         ENDIF
 
      ENDIF
 
      IF(IOPTZ.NE.0) THEN
 
         IF (LADDR.NE.0) THEN
            IF(IDEBFA.GE.0) PRINT *,
     +      'FMCLOS. dropping bank at address ',LBANK
            CALL MZDROP(IDIVFA,LBANK,' ')
            LADDR = 0
         ELSE
            IF(IDEBFA.GE.0) PRINT *,
     +      'FMCLOS. Bank address is zero, MZDROP request ignored'
         ENDIF
 
      ENDIF
 
  999 END
