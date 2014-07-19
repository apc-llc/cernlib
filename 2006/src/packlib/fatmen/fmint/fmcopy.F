*
* $Id: fmcopy.F,v 1.1.1.1 1996/03/07 15:18:10 mclareni Exp $
*
* $Log: fmcopy.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:10  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMCOPY(GENAM1,LBANK1,KEYS1,GENAM2,LBANK2,KEYS2,
     +CHOPT,IRC)
*
*     Subroutine to copy the dataset referenced by GENAM1 to GENAM2.
*     If LBANKn not zero, GENAMn is not used.
*     Otherwise the bank is fetched from the RZ file using FMGETK
*
*     Options:
*              A - input already staged (i.e. from FMSMCF)
*                  if IOPTA.NE.0 cannot call FMOPEN!
*              C - use STAGE CHANGE, implies S
*              K - queue to CHEOPS
*              S - STAGE the input file
*              W - with option Z - WAIT for output stage to complete
*              Z - STAGE the output file
*              F - use FZIN/FZOUT to permit conversion of FZ formats
*                  (triggered automatically if file format is F*
*                   but different in input/output banks)
*              R - skip Zebra start-of-run/end-of-run records
*              U - update catalogue with bank at LBANK2
*              P - physical copy - this is the default
*
#include "fatmen/faust.inc"
#include "fatmen/fatupd.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fatbank.inc"
#include "fatmen/fatstg.inc"
#include "fatmen/fattyp.inc"
#if defined(CERNLIB_CRAY)
      CHARACTER*255 CHFIN,CHFOUT
#endif
      CHARACTER*(*) CHOPT
      INTEGER       SYSTEMF,FMNODE,FMHOST,FMUSER
      CHARACTER*8   DDNAM1,DDNAM2
      CHARACTER*4   CHOPE,CHOPI,CHOPO,CHOPTF
      CHARACTER*20  FN1,FN2
      CHARACTER*(*) GENAM1,GENAM2
      CHARACTER*80  COMAND
      CHARACTER*6   VSN1,VID1,FSEQ1,VSN2,VID2,FSEQ2
      CHARACTER*15  XVID1,XVID2
      CHARACTER*8   VIP1,VIP2
      CHARACTER*6   VAXLAB,CHRECL,CHBLK
      CHARACTER*256 DSN1,DSN2
      CHARACTER*4   DEV1,DEV2,LAB1,LAB2
      CHARACTER*8   USER1,ADDR1,USER2,ADDR2
      CHARACTER*8   HOST1,HOST2
      CHARACTER*40  DCB1,DCB2
      CHARACTER*4   RECFM1,RECFM2
      CHARACTER*4   FFORM1,FFORM2
      CHARACTER*6   CDEN1,CDEN2
      CHARACTER*8   LIB1,LIB2
      CHARACTER*8   CHUSER,CHHOST,CHTYPE,CHSYS
      DIMENSION     BUFFER(8172)
      PARAMETER     (LKEYFA=10)
      DIMENSION KEYS1(LKEYFA),KEYS2(LKEYFA)
#include "fatmen/tmsdef0.inc"
#include "fatmen/fatvid0.inc"
#include "fatmen/fatoptd.inc"
#include "fatmen/fatvid1.inc"
#include "fatmen/tmsdef1.inc"
#include "fatmen/fatoptc.inc"
 
      NFCOPY = NFCOPY + 1
      IRC  = 0
      LGN1 = LENOCC(GENAM1)
      LGN2 = LENOCC(GENAM2)
      IF(IDEBFA.GE.0) THEN
         PRINT *,'FMCOPY. enter for ',GENAM1(1:LGN1),
     +      ' options = ',CHOPT
         IF(GENAM2(1:LGN2).NE.GENAM1(1:LGN1)) PRINT *,'FMCOPY. ',
     +      'output generic name is ',GENAM2(1:LGN2)
         IF(LBANK1.NE.0) PRINT *,'FMCOPY. user-supplied bank for ',
     +      'input generic name'
         IF(LBANK2.NE.0) PRINT *,'FMCOPY. user-supplied bank for ',
     +      'output generic name'
      ENDIF
*
*     Check authorisation
*
      IC = FMUSER(CHUSER)
      IC = FMHOST(CHHOST,CHTYPE,CHSYS)
      CALL CLTOU(CHUSER)
      CALL CLTOU(CHHOST)
      CALL FMACL(CHUSER,CHHOST,GENAM2(1:LGN2),'COPY','A',IRC)
      IF(IRC.NE.0) THEN
         NVIOL = NVIOL + 1
         IF(NVIOL.GT.MAXVIO) CALL ZFATAM
     +   ('Maximum number of security violations exceeded')
         PRINT *,'FMCOPY. you are not authorised to copy to ',
     +   GENAM2(1:LGN2)
         RETURN
      ENDIF
 
      IF(IOPTC.NE.0) IOPTS = 1
*
*     Save bank addresses in link area
*
      IF(LBANK1.NE.0) LOLDFA = LBANK1
      IF(LBANK2.NE.0) LNEWFA = LBANK2
      LTDSFA = 0
 
      IRC = 0
 
      IF(LBANK1.EQ.0) THEN
         CALL FMGETK(GENAM1,LBANK1,KEYS1,IRC)
         IF(IRC.NE.0) THEN
            IF(IDEBFA.GE.2)
     +      PRINT *,'FMCOPY. Return code ',IRC,' from FMGETK'
            IRC = 1
            RETURN
         ELSE
            LOLDFA = LBANK1
            LTDSFA = 0
         ENDIF
      ENDIF
 
      IF(LBANK2.EQ.0) THEN
         CALL FMGETK(GENAM2,LBANK2,KEYS2,IRC)
         IF(IRC.NE.0) THEN
            IF(IDEBFA.GE.2)
     +      PRINT *,'FMCOPY. Return code ',IRC,' from FMGETK'
            IRC = 1
            RETURN
         ELSE
            LNEWFA = LBANK2
         ENDIF
      ENDIF
 
      LBANK1 = LOLDFA
*
*     Update KEYS vectors from BANKs
*
      CALL FMUPKY(GENAM1,LBANK1,KEYS1,IRC)
      CALL FMUPKY(GENAM2,LBANK2,KEYS2,IRC)
*
*     Check that TARGET ^= SOURCE
*
      CALL FMCOMP(GENAM1,LBANK1,KEYS1,GENAM2,LBANK2,KEYS2,IRC)
      IF(IRC.EQ.0) THEN
         IF(IDEBFA.GT.-3)
     +   PRINT *,'FMCOPY. Error - output and input are identical'
         IRC = 1
         RETURN
      ELSE
         IRC = 0
      ENDIF
*
      IF(IDEBFA.GE.0) THEN
         CALL FMSHOW(GENAM1,LBANK1,KEYS1,'A',IRET)
         CALL FMSHOW(GENAM2,LBANK2,KEYS2,'A',IRET)
      ENDIF
 
      FATMBC = FATMBC + Q(LBANK1+KOFUFA+MFSZFA)
*
*     Queue to CHEOPS?
*
      IF(IOPTK.NE.0) THEN
*
*     File size and DCB must be specified
*
         CALL UHTOC(IQ(LBANK1+KOFUFA+MRFMFA),4,RECFM1,4)
         CALL UHTOC(IQ(LBANK2+KOFUFA+MRFMFA),4,RECFM2,4)
 
         IF(IQ(LBANK1+KOFUFA+MFSZFA).LE.0) THEN
            IF(IDEBFA.GE.-3) PRINT *,'FMCOPY. the input file size',
     +         ' must be set to perform a copy via CHEOPS'
            IRC = -1
         ENDIF
 
         IF(IQ(LBANK1+KOFUFA+MRLNFA).LE.0) THEN
            IF(IDEBFA.GE.-3) PRINT *,'FMCOPY. the input record length',
     +         ' must be set to perform a copy via CHEOPS'
            IRC = -1
         ENDIF
 
         IF(IQ(LBANK1+KOFUFA+MBLNFA).LE.0) THEN
            IF(IDEBFA.GE.-3) PRINT *,'FMCOPY. the input block length',
     +         ' must be set to perform a copy via CHEOPS'
            IRC = -1
         ENDIF
 
         IF(RECFM1(1:1).EQ.' ') THEN
            IF(IDEBFA.GE.-3) PRINT *,'FMCOPY. the input record format',
     +         ' must be set to perform a copy via CHEOPS'
            IRC = -1
         ENDIF
 
         IF(IQ(LBANK2+KOFUFA+MFSZFA).LE.0) THEN
            IF(IDEBFA.GE.-3) PRINT *,'FMCOPY. the output file size',
     +         ' must be set to perform a copy via CHEOPS'
            IRC = -2
         ENDIF
 
         IF(IQ(LBANK2+KOFUFA+MRLNFA).LE.0) THEN
            IF(IDEBFA.GE.-3) PRINT *,'FMCOPY. the output record length',
     +         ' must be set to perform a copy via CHEOPS'
            IRC = -2
         ENDIF
 
         IF(IQ(LBANK2+KOFUFA+MBLNFA).LE.0) THEN
            IF(IDEBFA.GE.-3) PRINT *,'FMCOPY. the output block length',
     +         ' must be set to perform a copy via CHEOPS'
            IRC = -2
         ENDIF
 
         IF(RECFM2(1:1).EQ.' ') THEN
            IF(IDEBFA.GE.-3) PRINT *,'FMCOPY. the output record format',
     +         ' must be set to perform a copy via CHEOPS'
            IRC = -2
         ENDIF
 
         IF(IRC.NE.0) THEN
            IF(IDEBFA.GE.-3) PRINT *,'FMCOPY. please correct the above',
     +         ' problems, e.g. using the shell MODIFY command'
            RETURN
         ENDIF
 
         IF(IDEBFA.GE.0) PRINT *,'FMCOPY. your request will be ',
     +      'queued to CHEOPS'
         CALL FMCOPQ(GENAM1,LBANK1,KEYS1,GENAM2,LBANK2,KEYS2,
     +               CHOPT,IRC)
         RETURN
      ENDIF
*
*     Get host information
*
      CALL UHTOC(IQ(LBANK1+KOFUFA+MHSNFA),4,HOST1,8)
      CALL UHTOC(IQ(LBANK2+KOFUFA+MHSNFA),4,HOST2,8)
      LHOST1 = LENOCC(HOST1)
      LHOST2 = LENOCC(HOST2)
      CALL CLTOU(HOST1(1:LHOST1))
      CALL CLTOU(HOST2(1:LHOST2))
*
*     Get input and output DSNs
*
      CALL FMGDSN(LBANK1,DSN1,LDSN1,IRC)
      CALL FMGDSN(LBANK2,DSN2,LDSN2,IRC)
*
*     Do we need to perform a remote copy?
*
      IF(KEYS1(MKMTFA).EQ.1.AND.KEYS2(MKMTFA).EQ.1) THEN
         IF(FMNODE(HOST1(1:LHOST1))+FMNODE(HOST2(1:LHOST2)).NE.0) THEN
            IF(IDEBFA.GE.0) PRINT *,'FMCOPY. a remote copy is required'
            CALL FMRCOP(GENAM1,LBANK1,KEYS1,GENAM2,LBANK2,KEYS2,CHOPT,
     +      IRC)
            IF(IRC.NE.0) GOTO 40
            GOTO 30
#if defined(CERNLIB_UNIX)||defined(CERNLIB_VAXVMS)
         ELSEIF(IOPTF.EQ.0) THEN
            IF(IDEBFA.GE.0) PRINT *,'FMCOPY. a local disk copy is '
     +      //'required'
            CALL FMLCOP(DSN1(1:LDSN1),DSN2(1:LDSN2),' ',IRC)
            IF(IRC.NE.0) GOTO 40
            GOTO 30
#endif
         ENDIF
      ENDIF
*
*     Determine whether IOPTF (FMFZCP) should be turned on
*     N.B. This will not work when FPACK files are supported!
*
      CALL UHTOC(IQ(LBANK1+KOFUFA+MFLFFA),4,FFORM1,4)
      CALL UHTOC(IQ(LBANK2+KOFUFA+MFLFFA),4,FFORM2,4)
      IF((FFORM1.NE.FFORM2).AND.(FFORM1(1:1).EQ.'F')
     +                     .AND.(FFORM2(1:1).EQ.'F')) THEN
         IOPTF = 1
      ENDIF
*
*     Copy using STAGE CHANGE (stagewrt, WRTAPE)
*
      IF((IOPTC.NE.0).AND.
     +   (KEYS1(MKMTFA).GT.1).AND.(KEYS2(MKMTFA).GT.1)) THEN
#if defined(CERNLIB_IBMVM)
         IF(IDEBFA.GE.0) PRINT *,'FMCOPY. copy will be performed using '
     +   //'STAGE CHANGE'
#endif
#if defined(CERNLIB_CRAY)
         IF(IDEBFA.GE.0) PRINT *,'FMCOPY. copy will be performed using '
     +   //'stagewrt'
#endif
#if defined(CERNLIB_VAXVMS)
         IF(IDEBFA.GE.0) PRINT *,'FMCOPY. copy will be performed using '
     +   //'WRTAPE'
#endif
*
*     Get DCB information
*
         CALL UHTOC(IQ(LBANK1+KOFUFA+MRFMFA),4,RECFM1,4)
         LRECL1 = IQ(LBANK1+KOFUFA+MRLNFA)*4
         LBLCK1 = IQ(LBANK1+KOFUFA+MBLNFA)*4
         WRITE(DCB1,9001) RECFM1,LRECL1,LBLCK1
         CALL UHTOC(IQ(LBANK2+KOFUFA+MRFMFA),4,RECFM2,4)
         LRECL2 = IQ(LBANK2+KOFUFA+MRLNFA)*4
         LBLCK2 = IQ(LBANK2+KOFUFA+MBLNFA)*4
         WRITE(DCB2,9001) RECFM2,LRECL2,LBLCK2
 9001 FORMAT(' RECFM ',A4,' LRECL ',I5,' BLOCK ',I5)
         CALL UHTOC(IQ(LBANK1+KOFUFA+MVSNFA),4,VSN1,6)
         LVSN1 = LENOCC(VSN1)
         CALL CLTOU(VSN1)
         CALL UHTOC(IQ(LBANK1+KOFUFA+MVIDFA),4,VID1,6)
         LVID1 = LENOCC(VID1)
         CALL CLTOU(VID1)
         WRITE(FSEQ1,9002) IQ(LBANK1+KOFUFA+MFSQFA)
 9002    FORMAT(I6)
         JFSEQ1 = INDEXB(FSEQ1,' ') + 1
         CDEN1 = CHMDEN(IQ(LBANK1+KOFUFA+MMTPFA))
*
*        Generate eXtended VID - with VID prefix
*
         JP = IQ(LBANK1+KOFUFA+MVIPFA)
         IF(JP.NE.0) THEN
            LVIP1 = LENOCC(PREVID(JP))
            VIP1 = PREVID(JP)(1:LVIP1)
            XVID1 = PREVID(JP)(1:LENOCC(PREVID(JP))) // '.' // VID1(1:
     +      LVID1)
            LXVID1 = LENOCC(XVID1)
 
         ELSE
            XVID1 = VID1
            LXVID1 = LVID1
            LVIP1 = 0
         ENDIF
 
#if defined(CERNLIB_PREFIX)
         VID1 = XVID1
         LVID1 = LXVID1
#endif
*
*     Set IQUEST(11) to media type in case volume unknown or
*     TMS option not installed.
*
         IQUEST(11) = IQ(LBANK1+KOFUFA+MMTPFA)
         CALL FMQTMS(VID1(1:LVID1),LIB1,MODEL,DENS,MNTTYP,LAB1,IC)
*
*     Believe density from TMS if tape is known
*
         IF(IC.NE.0) CDEN1 = DENS
         CALL CLTOU(LAB1)
         LLAB1 = LENOCC(LAB1)
 
         CALL UHTOC(IQ(LBANK2+KOFUFA+MVSNFA),4,VSN2,6)
         LVSN2 = LENOCC(VSN2)
         CALL CLTOU(VSN2)
         CALL UHTOC(IQ(LBANK2+KOFUFA+MVIDFA),4,VID2,6)
         LVID2 = LENOCC(VID2)
         CALL CLTOU(VID2)
         WRITE(FSEQ2,9002) IQ(LBANK2+KOFUFA+MFSQFA)
         JFSEQ2 = INDEXB(FSEQ2,' ') + 1
         CDEN2 = CHMDEN(IQ(LBANK2+KOFUFA+MMTPFA))
*
*        Generate eXtended VID - with VID prefix
*
         JP = IQ(LBANK2+KOFUFA+MVIPFA)
         IF(JP.NE.0) THEN
            LVIP2 = LENOCC(PREVID(JP))
            VIP2 = PREVID(JP)(1:LVIP2)
            XVID2 = PREVID(JP)(1:LENOCC(PREVID(JP))) // '.' // VID2(1:
     +      LVID2)
            LXVID2 = LENOCC(XVID2)
 
         ELSE
            XVID2 = VID2
            LXVID2 = LVID2
            LVIP2 = 0
         ENDIF
 
#if defined(CERNLIB_PREFIX)
         VID2 = XVID2
         LVID2 = LXVID2
#endif
*
*     Set IQUEST(11) to media type in case volume unknown or
*     TMS option not installed.
*
         IQUEST(11) = IQ(LBANK2+KOFUFA+MMTPFA)
         CALL FMQTMS(VID2(1:LVID2),LIB2,MODEL,DENS,MNTTYP,LAB2,IC)
*
*     Believe density from TMS if tape is known
*
         IF(IC.NE.0) CDEN2 = DENS
         CALL CLTOU(LAB2)
         LLAB2 = LENOCC(LAB2)
*
*     Ensure that input file is STAGEd
*
         IF(IOPTA.EQ.0) THEN
            CHOPE = 'RU'
#if defined(CERNLIB_IBMVM)
            LURZFA = IQ(LTOPFA+KOFUFA+MLUNFA)
            WRITE(DDNAM1,9004) LURZFA
            IF(DDNAM1(3:3).EQ.' ') DDNAM1(3:3) = '0'
            CALL FMOPEN(GENAM1,DDNAM1,LBANK1,CHOPE,IRC)
#endif
#if defined(CERNLIB_CRAY)||defined(CERNLIB_VAXVMS)
            WRITE(DDNAM1,'(I2.2)') LUFZFA
            CALL FMOPEN(GENAM1,DDNAM1,LBANK1,CHOPE,IRC)
#endif
            IF(IRC.NE.0) THEN
               IF(IDEBFA.GE.0) PRINT *,'FMCOPY. Return code ',IRC,' '
     +         //'from FMOPEN for GENAM1'
               GOTO 40
            ENDIF
         ENDIF
*
*     Build STAGE Change command
*
#if defined(CERNLIB_PREFIX)
         IF(LVIP1.NE.0) THEN
            VID1 = VID1(1:LVID1) // '.' // VIP1(1:LVIP1)
            LVID1 = LVID1 + LVIP1 + 1
         ENDIF
         IF(LVIP2.NE.0) THEN
            VID2 = VID2(1:LVID2) // '.' // VIP2(1:LVIP2)
            LVID2 = LVID2 + LVIP2 + 1
         ENDIF
#endif
#if defined(CERNLIB_CRAY)
*
*     Set IQUEST(11) to media type in case volume unknown or
*     TMS option not installed.
*
         IQUEST(11) = IQ(LBANK2+KOFUFA+MMTPFA)
         CALL FMQTMS(VID2(1:LVID2),LIB,MODEL,DENS,MNTTYP,LABTYP,IC)
         IF(IDEBFA.GE.3) THEN
            PRINT *,'FMCOPY. return from FMQTMS with ',
     +              VID2,'/',LIB,'/',MODEL,'/',DENS,'/',MNTTYP,'/',
     +              LABTYP,'/',IC
         ENDIF
*
*     Believe density from TMS if tape is known
*
         IF(IC.NE.0) CDEN2 = DENS
         CALL CUTOL(LABTYP)
         LLAB = LENOCC(LABTYP)
 
         DDNAM2 = 'fort.   '
         IF(LUFZFA.LT.10) THEN
            WRITE(DDNAM2(6:6),'(I1)') LUFZFA
         ELSE
            WRITE(DDNAM2(6:7),'(I2)') LUFZFA
         ENDIF
 
         WRITE(FSEQ2,9002) IQ(LBANK2+KOFUFA+MFSQFA)
 
*        "stagein fort.lun -v vsn -V vid -l sl|nl|al|blp
*                          -g TAPE|CART|SMCF -d 6250|1600"
         COMAND = 'stagewrt '//DDNAM2 // ' -v '//VSN2(1:LVSN2)// ' -V '
     +   //VID2(1:LVID2)//' -l '//LABTYP//' -g '//MODEL // ' -q ' //
     +   FSEQ2 // ' -K -S sbin'
 
         LENCOM = LENOCC(COMAND)
*
*     Add DSN if IOPTN not specified
*
         IF(IOPTN.EQ.0) THEN
            COMAND = COMAND(1:LENCOM) // ' -f '//DSN2(1:LDSN2)
            LENCOM = LENOCC(COMAND)
         ENDIF
 
*
*     Add DCB information
*
         WRITE(DCB2,9003) RECFM2(1:1),LRECL2,LBLCK2
 9003       FORMAT(' -F ',A1,' -L ',I5,' -b ',I5)
         COMAND = COMAND(1:LENOCC(COMAND)) // DCB2
         LENCOM = LENOCC(COMAND)
 
         IF(IDEBFA.GE.0) PRINT *,COMAND(1:LENCOM)
         IC = SYSTEMF(COMAND(1:LENCOM))
 
#endif
#if defined(CERNLIB_IBMVM)
         CALL FMGDSN(LBANK2,DSN2,LDSN2,IRC)
         COMAND = ' '
         COMAND = 'EXEC STAGE CHANGE '
     +   // VSN1(1:LVSN1) // '.'
     +   // FSEQ1(JFSEQ1:LEN(FSEQ1))
     +   // '.' // LAB1(1:LLAB1) // '.' // VID1(1:LVID1) // ' '
     +   // VSN2(1:LVSN2) // '.'
     +   // FSEQ2(JFSEQ2:LEN(FSEQ2))
     +   // '.' // LAB2(1:LLAB2) // '.' // VID2(1:LVID2)
     +   // ' (STAGEOUT DSN ' // DSN2(1:LDSN2)
 
         IF(IOPTW.NE.0) THEN
            COMAND = COMAND(1:LENOCC(COMAND)) // ' WAIT'
         ENDIF
 
#endif
#if (defined(CERNLIB_IBMVM))&&(!defined(CERNLIB_TMS))
         COMAND = COMAND(1:LENOCC(COMAND)) // ' DEVTYPE '//MODEL
#endif
#if (defined(CERNLIB_IBMVM))&&(!defined(CERNLIB_CERN))&&(defined(CERNLIB_TMS))
         COMAND = COMAND(1:LENOCC(COMAND)) // ' DEVTYPE '//MODEL
#endif
#if defined(CERNLIB_IBMVM)
 
         LENCOM = LENOCC(COMAND)
 
         IF(IDEBFA.GE.0) PRINT *,'FMCOPY. running ',COMAND(1:LENCOM)
 
         CALL VMCMS(COMAND(1:LENCOM),IRC)
         IF(IRC.NE.0) THEN
            IF(IDEBFA.GE.0)
     +      PRINT *,'FMCOPY. Return code ',IRC,' from STAGE'
            RETURN
         ENDIF
 
#endif
#if defined(CERNLIB_VAXVMS)
         VAXLAB = 'EBCDIC'
         IF(LAB2(1:LLAB2).EQ.'AL') THEN
            VAXLAB = 'ASCII'
         ELSEIF(LAB2(1:LLAB2).EQ.'NL') THEN
            VAXLAB = 'NONE'
         ENDIF
         COMAND = ' '
         WRITE(CHRECL,'(I6.6)') LRECL2
         WRITE(CHBLK, '(I6.6)') LBLCK2
         COMAND = '$WRTAPE '//VSN2(1:LVSN2)//' '//VID2(1:LVID2)//
     +            ' /NAME='//DSN2(1:LDSN2)//'/NUMBER='//
     +            FSEQ2(JFSEQ2:LEN(FSEQ2))//'/INFILE='//DDNAM1
     +            //'/LABEL='//VAXLAB
     +            //'/GENERIC='//MODEL
         IF(LRECL2.GT.0) COMAND = COMAND(1:LENOCC(COMAND)) //
     +     '/RECORDSIZE='//CHRECL
         IF(LBLCK2.GT.0) COMAND = COMAND(1:LENOCC(COMAND)) //
     +     '/BLOCKSIZE='//CHBLK
 
         IF(INDEX(RECFM2,'F').NE.0) THEN
            COMAND = COMAND(1:LENOCC(COMAND)) // '/FIXED'
         ELSEIF(INDEX(RECFM2,'V').NE.0) THEN
            COMAND = COMAND(1:LENOCC(COMAND)) // '/VARIABLE'
         ENDIF
*
         LENCOM = LENOCC(COMAND)
 
         IF(IDEBFA.GE.0) PRINT *,COMAND(1:LENCOM)
 
         ISTAT = LIB$SPAWN(COMAND(1:LENCOM))
#include "fatmen/fatvaxrc.inc"
 
#endif
 
      ELSEIF(IOPTF.NE.0) THEN
*
*     Perform copy using FZIN/FZOUT
*
         CALL FMGLUN(LUNI,IRC)
         IF(IRC.NE.0) THEN
            PRINT *,'FMCOPY. unable to allocate input unit for copy'
            GOTO 40
         ENDIF
         CALL FMGLUN(LUNO,IRC)
         IF(IRC.NE.0) THEN
            PRINT *,'FMCOPY. unable to allocate input unit for copy'
            GOTO 40
         ENDIF
 
         CALL FMDDNM(LUNI,DDNAM1,IRC)
         CALL FMDDNM(LUNO,DDNAM2,IRC)
 
         CHOPI = 'FR'
         IF(KEYS1(MKMTFA).GT.1) THEN
            IF(KEYS2(MKMTFA).GT.1) THEN
               CHOPI = 'FRT'
            ELSE
               CHOPI = 'FRTE'
            ENDIF
         ELSE
            CHOPI = 'RU'
         ENDIF
         CALL FMOPEN(GENAM1,DDNAM1,LBANK1,CHOPI,IRC)
         IF(IRC.NE.0) THEN
            IF(IDEBFA.GE.0)
     +      PRINT *,'FMCOPY. Return code ',IRC,' from FMOPEN for GENAM1'
            GOTO 40
         ENDIF
         CHOPO = 'FW'
         IF(KEYS2(MKMTFA).GT.1) CHOPO = 'TFWE'
         CALL FMOPEN(GENAM2,DDNAM2,LBANK2,CHOPO,IRC)
         IF(IRC.NE.0) THEN
            IF(IDEBFA.GE.0)
     +      PRINT *,'FMCOPY. Return code ',IRC,' from FMOPEN for GENAM2'
            GOTO 40
         ENDIF
         CHOPTF = ' '
         IF(IOPTR.NE.0) CHOPTF = 'R'
         CALL FMFZCP(LUNI,LUNO,CHOPTF,IRC)
         CALL FMFLUN(LUNI,IC)
         CALL FMFLUN(LUNO,IC)
         CHOPI = 'E'
         CALL FMCLOS(GENAM1,DDNAM1,LBANK1,CHOPI,IC)
         CHOPO = 'EFP'
         CALL FMCLOS(GENAM2,DDNAM2,LBANK2,CHOPO,IC)
 
         IF(IRC.NE.0) THEN
            IF(IDEBFA.GE.0) PRINT *,'FMCOPY. Return code ',IRC,' from '
     +      //'FMFZCP'
            GOTO 40
         ENDIF
      ELSE
 
#if !defined(CERNLIB_IBMVM)
*
*     Perform physical copy - switch IOPTS & IOPTZ on
*
         IOPTS  = 1
         IOPTZ  = 1
 
         CALL FMGLUN(LUNI,IRC)
         IF(IRC.NE.0) THEN
            PRINT *,'FMCOPY. unable to allocate input unit for copy'
            GOTO 40
         ENDIF
         CALL FMGLUN(LUNO,IRC)
         IF(IRC.NE.0) THEN
            PRINT *,'FMCOPY. unable to allocate input unit for copy'
            GOTO 40
         ENDIF
 
         CALL FMDDNM(LUNI,DDNAM1,IRC)
         CALL FMDDNM(LUNO,DDNAM2,IRC)
*
*     Set FMOPEN options
*
         CHOPI = 'RU'
         IF(IOPTS.EQ.0.AND.KEYS1(MKMTFA).GT.1) THEN
            IF(KEYS2(MKMTFA).GT.1) THEN
               CHOPI = 'RTU'
            ELSE
               CHOPI = 'RTEU'
            ENDIF
         ELSE
            CHOPI = 'RU'
         ENDIF
 
         IF(IDEBFA.GE.3) PRINT *,'FMCOPY. call FMOPEN for input'
         CALL FMOPEN(GENAM1,DDNAM1,LBANK1,CHOPI,IRC)
         IF(IRC.NE.0) THEN
            IF(IDEBFA.GE.0)
     +      PRINT *,'FMCOPY. Return code ',IRC,' from FMOPEN for GENAM1'
            GOTO 40
         ENDIF
 
         CHOPO = 'WU'
         IF(IOPTZ.EQ.0.AND.KEYS2(MKMTFA).GT.1) CHOPO = 'WTEU'
         IF(IDEBFA.GE.3) PRINT *,'FMCOPY. call FMOPEN for output'
         CALL FMOPEN(GENAM2,DDNAM2,LBANK2,CHOPO,IRC)
         IF(IRC.NE.0) THEN
            IF(IDEBFA.GE.0)
     +      PRINT *,'FMCOPY. Return code ',IRC,' from FMOPEN for GENAM2'
            GOTO 40
         ENDIF
 
#endif
#if (!defined(CERNLIB_IBMVM))&&(defined(CERNLIB_CRAY))
*
*     Find the real file name(s)
*
         CALL FMASSN(DDNAM1,CHFIN,'G',IRC)
         CALL FMASSN(DDNAM2,CHFOUT,'G',IRC)
         LCHFIN = LENOCC(CHFIN)
         LCHFOU = LENOCC(CHFOUT)
*
*     Now do the copy
*
         CALL FMLCOP(CHFIN(1:LCHFIN),CHFOUT(1:LCHFOU),'C',IRC)
#endif
#if (!defined(CERNLIB_IBMVM))&&(!defined(CERNLIB_CRAY))
*
*     Now do the copy
*
         CALL FMLCOP(DDNAM1,DDNAM2,' ',IRC)
#endif
#if !defined(CERNLIB_IBMVM)
*
*     Free logical units
*
         CALL FMFLUN(LUNI,IC)
         CALL FMFLUN(LUNO,IC)
 
         CHOPI = 'N'
         CALL FMCLOS(GENAM1,DDNAM1,LBANK1,CHOPI,IC)
 
         CHOPO = 'N'
         IF(KEYS2(MKMTFA).GT.1) CHOPO = 'NP'
         CALL FMCLOS(GENAM2,DDNAM2,LBANK2,CHOPO,IC)
 
         IF(IRC.NE.0) THEN
            IF(IDEBFA.GE.0) PRINT *,'FMCOPY. Return code ',IRC,' from '
     +      //'FMLCOP'
            GOTO 40
         ENDIF
 
#endif
#if defined(CERNLIB_IBMVM)
 
         CHOPE = 'RU'
         IF((KEYS1(MKMTFA).GT.1).AND.(IOPTS.EQ.0)) THEN
            IF(KEYS2(MKMTFA).GT.1) THEN
               CHOPE = 'RTU'
            ELSE
               CHOPE = 'RTUE'
            ENDIF
         ELSE
            CHOPE = 'RU'
         ENDIF
         LURZFA = IQ(LTOPFA+KOFUFA+MLUNFA)
         WRITE(DDNAM1,9004) LURZFA
 9004    FORMAT('VM',I2,'F001')
         IF(DDNAM1(3:3).EQ.' ') DDNAM1(3:3) = '0'
         CALL FMOPEN(GENAM1,DDNAM1,LBANK1,CHOPE,IRC)
         IF(IRC.NE.0) THEN
            IF(IDEBFA.GE.0) PRINT *,'FMCOPY. Return code ',IRC,' from '
     +      //'FMOPEN for GENAM1'
            GOTO 40
         ENDIF
 
         CHOPE = 'UW'
         IF(KEYS2(MKMTFA).GT.1) CHOPE = 'TUWE'
         IF(IOPTZ.NE.0)         CHOPE = 'UW'
         WRITE(DDNAM2,9004) LUFZFA
         IF(DDNAM2(3:3).EQ.' ') DDNAM2(3:3) = '0'
         CALL FMOPEN(GENAM2,DDNAM2,LBANK2,CHOPE,IRC)
         IF(IRC.NE.0) THEN
            IF(IDEBFA.GE.0) PRINT *,'FMCOPY. Return code ',IRC,' from '
     +      //'FMOPEN for GENAM2'
            GOTO 40
         ENDIF
*
*     Get DCB information
*
         CALL UHTOC(IQ(LBANK1+KOFUFA+MRFMFA),4,RECFM1,4)
         LRECL1 = IQ(LBANK1+KOFUFA+MRLNFA)*4
         LBLCK1 = IQ(LBANK1+KOFUFA+MBLNFA)*4
         CALL UHTOC(IQ(LBANK2+KOFUFA+MRFMFA),4,RECFM2,4)
         LRECL2 = IQ(LBANK2+KOFUFA+MRLNFA)*4
         LBLCK2 = IQ(LBANK2+KOFUFA+MBLNFA)*4
 
         CALL VMINIT
         FN1   = ' '
         LREC1 = LRECL1
         LBLK1 = LBLCK1
         IF(IDEBFA.GE.2) PRINT *,
     +   'FMCOPY. call VMOPEN for input  dataset on unit ',LURZFA,
     +   ' with DCB ',RECFM1,LREC1,LBLK1
         CALL VMOPEN(LURZFA,FN1,'R',RECFM1,LREC1,LBLK1,IRC,INFO)
         IF(IDEBFA.GE.2) PRINT *,
     +   'FMCOPY. return from VMOPEN ',
     +   ' with DCB ',RECFM1,LREC1,LBLK1
         IF(IABS(IRC).GT.1) THEN
            PRINT *,'FMCOPY. return code ',IRC,
     +              ' from VMOPEN for input file, INFO = ',INFO
            CALL VMEND
            CALL FMCLOS(GENAM1,DDNAM1,LBANK1,'C',IRET)
            GOTO 40
         ENDIF
 
         IF(IDEBFA.GE.0) PRINT *,'FMCOPY. Opened input  unit'
         FN2   = ' '
         LREC2 = LRECL2
         LBLK2 = LBLCK2
         IF(IDEBFA.GE.2) PRINT *,
     +   'FMCOPY. call VMOPEN for output dataset on unit ',LUFZFA,
     +   ' with DCB ',RECFM2,LREC2,LBLK2
         CALL VMOPEN(LUFZFA,FN2,'W',RECFM2,LREC2,LBLK2,IRC,INFO)
         IF(IDEBFA.GE.2) PRINT *,
     +   'FMCOPY. return from VMOPEN ',
     +   ' with DCB ',RECFM2,LREC2,LBLK2
         IF(IABS(IRC).GT.1) THEN
            PRINT *,'FMCOPY. return code ',IRC,
     +              ' from VMOPEN for output file, INFO = ',INFO
            CALL VMEND
            CALL FMCLOS(GENAM1,DDNAM1,LBANK1,'C',IRET)
            CALL FMCLOS(GENAM2,DDNAM2,LBANK2,'C',IRET)
            GOTO 40
         ENDIF
         IF(IDEBFA.GE.0) PRINT *,'FMCOPY. Opened output unit'
         NRECS  = 0
         NLONG  = 0
         NSHORT = 32768
         NREAD  = LREC1
         IF(LREC1.EQ.0) NREAD = LBLK1
         IF(NREAD.EQ.0) THEN
            IF(IDEBFA.GT.-3) PRINT *,
     +      'FMCOPY. record length and blocksize of input dataset are ',
     +      'both zero - cannot perform copy'
            CALL VMEND
            IRC = 1
            GOTO 40
         ENDIF
 
   10    CONTINUE
         LDAT = 32768
         CALL VMREAD(LURZFA,BUFFER,NREAD,LDAT,IRC,INFO)
         IF(IABS(IRC).EQ.1) GOTO 20
         IF(IABS(IRC).GT.1) THEN
            IF(IDEBFA.GE.0) PRINT *,'FMCOPY. return code ',IRC,
     +                              ' from VMREAD'
            IF(IABS(IRC).GT.4) THEN
               CALL VMEND
               GOTO 40
            ENDIF
         ENDIF
         IF(LDAT.GT.NLONG)  NLONG  = LDAT
         IF(LDAT.LT.NSHORT) NSHORT = LDAT
         CALL VMRITE(LUFZFA,BUFFER,LDAT,IRC,INFO)
         IF(IRC.NE.0) THEN
            IF(IDEBFA.GE.0) PRINT *,'FMCOPY. return code ',IRC,
     +                              ' from VMRITE'
            CALL VMEND
            GOTO 40
         ENDIF
         NRECS = NRECS + 1
         GOTO 10
   20    CONTINUE
         IF(IDEBFA.GE.0) PRINT *,'FMCOPY. ',NRECS,
     +   ' records written, shortest/longest = ',NSHORT,NLONG
*
*     Options for FMCLOS
*
         IF(IOPTS.NE.0) THEN
            CHOPE = 'D'
         ELSE
            CHOPE = ' '
         ENDIF
 
         CALL FMCLOS(GENAM1,DDNAM1,LBANK1,CHOPE,IRC)
 
         CHOPE = 'DU'
 
         IF(IOPTZ.NE.0) THEN
            IF(IOPTW.EQ.0) THEN
               CHOPE = 'DUP'
            ELSE
               CHOPE = 'DUPW'
            ENDIF
         ENDIF
 
         CALL FMCLOS(GENAM2,DDNAM2,LBANK2,CHOPE,IRC)
         IF(IRC.NE.0) THEN
            IF(IDEBFA.GE.-3) PRINT *,'FMCOPY. return code ',IRC,
     +         ' from FMCLOS. Entry will not be added to catalogue'
            GOTO 40
         ENDIF
 
         CALL VMEND
 
#endif
      ENDIF
 
   30 CALL FMPUT(GENAM2,LBANK2,IRC)
   40 RETURN
      END
