*
* $Id: fmshow.F,v 1.2 1999/08/11 13:06:49 jamie Exp $
*
* $Log: fmshow.F,v $
* Revision 1.2  1999/08/11 13:06:49  jamie
* print date in I6.6 for y2k
*
* Revision 1.1.1.1  1996/03/07 15:18:10  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMSHOW(GNAME,LFAT,KEYS,CHOPT,IRC)
#include "fatmen/faust.inc"
#include "fatmen/fattyp.inc"
#include "fatmen/fmdrep.inc"
#include "fatmen/fatlcc.inc"
#include "fatmen/fmpath.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fatbank.inc"
#include "fatmen/fatout.inc"
#include "zebra/zunit.inc"
#include "fatmen/fatusr.inc"
      CHARACTER*(*)   GNAME,CHOPT
      CHARACTER*255   GENAM
      CHARACTER*255   CHTAG
      PARAMETER       (LKEYFA=10)
      DIMENSION       KEYS(LKEYFA)
      CHARACTER*20    FILE
      CHARACTER*80    COMM
      CHARACTER*4     FFORM,FLFRM,UFORM
      CHARACTER*256   DSN
      CHARACTER*6     VSN,VID
      CHARACTER*8     VIP
      CHARACTER*15    XVID
      CHARACTER*8     OWNER,ACCT,JOB,NODE
      CHARACTER*16    TYPE
      CHARACTER*12    CHOS
      CHARACTER*80    CHLINE
      DIMENSION       IUSER(10)
      INTEGER         FMISET
      LOGICAL         IOPEN
 
#include "fatmen/tmsdef0.inc"
#include "fatmen/fatmed0.inc"
#include "fatmen/fatvid0.inc"
#include "fatmen/fatoptd.inc"
#include "fatmen/tmsdef1.inc"
#include "fatmen/fatmed1.inc"
#include "fatmen/fatvid1.inc"
#include "fatmen/fatoptc.inc"
 
      NFSHOW = NFSHOW + 1
 
      LCHOPT = LENOCC(CHOPT)
      IF(IOPTA.NE.0) THEN
#include "fatmen/fatoset.inc"
         IF(INDEX(CHOPT,'B').EQ.0) IOPTB = 0
         IF(INDEX(CHOPT,'D').EQ.0) IOPTD = 0
         IF(INDEX(CHOPT,'E').EQ.0) IOPTE = 0
         IF(INDEX(CHOPT,'G').EQ.0) IOPTG = 0
         IF(INDEX(CHOPT,'Q').EQ.0) IOPTQ = 0
         IF(INDEX(CHOPT,'J').EQ.0) IOPTJ = 0
         IF(INDEX(CHOPT,'R').EQ.0) IOPTR = 0
         IF(INDEX(CHOPT,'Y').EQ.0) IOPTY = 0
         IF(INDEX(CHOPT,'Z').EQ.0) IOPTZ = 0
      ENDIF
*
*     Print the bank entry corresponding to the input generic name
*
      IRC = 0
      LGEN = LENOCC(GNAME)
      GENAM = GNAME(1:LGEN)
      CALL CLTOU(GENAM(1:LGEN))
 
      INQUIRE(3,OPENED=IOPEN)
      IF((OUTPUT(1:3).EQ.'TTY').OR.
     +   (LENOCC(OUTPUT).EQ.0) .OR.
     +   (.NOT.IOPEN))               THEN
         LWRITE = LPRTFA
      ELSE
         LWRITE = 3
         IF(IDEBFA.GE.3) PRINT *,'FMSHOW. output will be ',
     +      'redirected to ',OUTPUT
      ENDIF
 
      IF(LFAT.EQ.0) THEN
         CALL FMGETK(GENAM(1:LGEN),LBANK,KEYS,IRC)
         IF(IRC.NE.0) THEN
            WRITE (LWRITE,*) 'FMSHOW. Return code ',IRC,' from FMGETK'
            RETURN
         ENDIF
         LBANK = LBANK + KOFUFA
      ELSEIF(LFAT.EQ.-1) THEN
         CALL FMUPKY(GENAM(1:LGEN),LADDBK,KEYS,IRC)
         LBANK = LADDBK + KOFUFA
      ELSE
         CALL FMUPKY(GENAM(1:LGEN),LFAT,KEYS,IRC)
         IF(IDEBFA.GE.1)
     +   WRITE(LWRITE,* )
     +        'FMSHOW. Enter for user-supplied bank at address ',LFAT
         LBANK = LFAT + KOFUFA
      ENDIF
 
      ICH   = INDEXB(GENAM(1:LGEN),'/')
      FILE  = GENAM(ICH+1:LGEN)
      LFILE = LENOCC(FILE)
*
*     Option J : just files that are accessible
*
      IF(IOPTJ.NE.0.OR.IOPTR.NE.0) THEN
*
*     Show where the data Reside
*
         CHLINE = ' '
 
         IF(IQ(LBANK+MMTPFA).EQ.1) THEN
*
*     Disk files: show how the data would be accessed
*                 (direct, VAXcluster, DFS, NFS, RFIO, FPACK, AFS, SFS, GIME)
*
            CALL FADARE(GENAM(1:LGEN),LBANK-KOFUFA,KEYS,CHLINE,CHOPT,
     +                  IRC)
         ELSE
*
*     Tape files: is volume in an active library?
*                 does a device of the required type exist on this node?
*                 a served device?
*                 is it staged?
*
            CALL FATARE(GENAM(1:LGEN),LBANK-KOFUFA,KEYS,CHLINE,CHOPT,
     +                  IRC)
         ENDIF
 
         IF(IOPTJ.NE.0.AND.IRC.NE.0) GOTO 10
 
      ENDIF
*
*     Check whether we have any options for FMSHOW...
*
      ISHOW = FMISET(CHOPT,'BDEIX')
*
*     Unpack user comment for call to FMUPRT
*
      COMM = ' '
      CALL UHTOC(IQ(LBANK+MUCMFA),4,COMM,80)
*
*     Options B(rief) for 80 column listing, E(xtended) for 132 column listing
*
      IF((IOPTB.NE.0).OR.(IOPTE.NE.0)) THEN
         CALL FMSHO2(GENAM,LBANK-KOFUFA,KEYS,CHOPT,IC)
      ELSEIF(IOPTD.NE.0) THEN
         WRITE(LWRITE,9001) GENAM(1:LGEN),KEYS(1)
 9001    FORMAT(' rm ',A,1X,I10)
      ELSE
         IF(ISHOW.NE.0) THEN
            IF(CHOPT(1:LCHOPT).NE.'G') WRITE(LWRITE,'(A)') ' '
 
            IF(IOPTG.NE.0) THEN
               WRITE(LWRITE,9002) GENAM(1:LGEN)
            ELSE
               WRITE(LWRITE,9003) FILE(1:LFILE)
            ENDIF
         ELSE
            WRITE(LWRITE,9002) FILE(1:LFILE)
         ENDIF
 
 9002 FORMAT(' ',A)
 9003 FORMAT(' Generic filename: ',A)
      ENDIF
 
      IF(ISHOW.EQ.0)           GOTO 10
 
      LDSN = 0
*
*     Keys
*
      IF (IOPTK .NE. 0) THEN
*
*     Integer representation
*
         WRITE (LWRITE,9004)
     +   KEYS(MKCLFA),KEYS(MKMTFA),KEYS(MKLCFA),
     +              KEYS(MKSRFA)
 9004    FORMAT(' Data repr.: ',I2,' Media type: ',I2,
     +   ' Location code: ',I6,' File serial number: ',I6)
*
*     Character representation
*
         IF(KEYS(MKCLFA).GT.0.AND.KEYS(MKCLFA).LE.NWFFAT)
     +      WRITE (LWRITE,9005) CHDREP(KEYS(MKCLFA))
 9005    FORMAT(' Word format : ',A)
         IF(KEYS(MKMTFA).GT.0.AND.KEYS(MKMTFA).LE.NMTYP)
     +      WRITE(LWRITE,9006) CHMGEN(KEYS(MKMTFA))
 9006    FORMAT(' Device group: ',A8)
         IF(KEYS(MKLCFA).EQ.0) THEN
            CALL UHTOC(IQ(LBANK+MFQNFA),4,DSN,NFQNFA)
            LDSN = LENOCC(DSN)
            WRITE(LWRITE,9007) DSN(1:LDSN)
 9007    FORMAT(' Link-->     : ',A)
         ELSE
            JKEY = IUFIND(KEYS(MKLCFA),NLCCFA,1,NKLCFA)
            IF(JKEY.LE.NKLCFA) THEN
               LLOC = LENOCC(CHLOCF(JKEY))
               WRITE(LWRITE,9008)
     +         CHLOCF(JKEY)(1:LENOCC(CHLOCF(JKEY)))
            ENDIF
 9008    FORMAT(' Location    : ',A)
         ENDIF
         IF (LCHOPT.EQ.1.AND.CHOPT(1:1).NE.'A') GOTO 10
      ENDIF
*
*     Display specified fields
*
      IF (IOPTC .NE. 0) THEN
*
*     Comment field
*
         LCOMM = LENOCC(COMM)
         IF(LCOMM.EQ.0) THEN
            COMM  = '(no comment)'
            LCOMM = LENOCC(COMM)
         ENDIF
         WRITE (LWRITE,9009) COMM(1:LCOMM)
 9009    FORMAT(' Comment: ',A)
      ENDIF
 
      IF (IOPTF .NE. 0) THEN
*
*     File attributes
*
         WRITE (LWRITE,9010) IQ(LBANK+MSRDFA),IQ(LBANK+MERDFA)
         WRITE (LWRITE,9011) IQ(LBANK+MSBLFA),IQ(LBANK+MEBLFA)
 9010    FORMAT(' Start record: ',I10,' End record: ',I10)
 9011    FORMAT(' Start block : ',I10,' End block : ',I10)
      ENDIF
 
      IF (IOPTL .NE. 0) THEN
*
*     Logical attributes
*
         CALL UHTOC(IQ(LBANK+MFLFFA),4,FFORM,4)
         CALL UHTOC(IQ(LBANK+MFUTFA),4,UFORM,4)
         WRITE(LWRITE,9012) FFORM,UFORM
 9012    FORMAT(' File format: ',A4,' user format: ',A4)
      ENDIF
 
      IF (IOPTM .NE. 0) THEN
*
*     Media attributes
*
         IF (IQ(LBANK+MMTPFA) .EQ. 1) THEN
*
*        Disk dataset, show MHSNFA, MHSTFA, MHOSFA
*
            CALL UHTOC(IQ(LBANK+MHSNFA),4,NODE,NHSNFA)
            CALL UHTOC(IQ(LBANK+MHSTFA),4,TYPE,NHSTFA)
            CALL UHTOC(IQ(LBANK+MHOSFA),4,CHOS,NHOSFA)
            WRITE (LWRITE,9013) NODE,TYPE,CHOS
 9013       FORMAT(' Host name:   ',A8,' host type: ',A8,
     +             ' host OS: ',A12,' (Disk dataset)')
         ELSE
*
*        Tape dataset, show VSN/VID, FSEQ
*
            CALL UHTOC(IQ(LBANK+MVSNFA),4,VSN,6)
            CALL UHTOC(IQ(LBANK+MVIDFA),4,VID,6)
            JPRE = IQ(LBANK+MVIPFA)
            IF(JPRE.EQ.0) THEN
               WRITE (LWRITE,9014) VSN,VID,IQ(LBANK+MFSQFA)
            ELSE
               WRITE (LWRITE,9015) VSN,VID,PREVID(JPRE),IQ(LBANK+MFSQFA)
            ENDIF
 9014       FORMAT(' VSN: ',A6,' VID: ',A6,' FSEQ: ',I4)
 9015       FORMAT(' VSN: ',A6,' VID: ',A6,' VID-prefix: ',A8,
     +             ' FSEQ: ',I4)
         ENDIF
      ENDIF
 
      IF (IOPTN .NE. 0) THEN
*
*     Name attributes (DSN on disk/tape)
*
         IF(LDSN.EQ.0) THEN
            CALL UHTOC(IQ(LBANK+MFQNFA),4,DSN,NFQNFA)
            LDSN = LENOCC(DSN)
         ENDIF
         WRITE (LWRITE,9016) DSN(1:LDSN)
 9016    FORMAT(' Fileid: ',5X,A)
      ENDIF
 
      IF (IOPTO .NE. 0) THEN
*
*     Owner attributes
*
         CALL CFILL(' ',OWNER,1,8)
         CALL UHTOC(IQ(LBANK+MCURFA),4,OWNER,8)
         CALL CFILL(' ',ACCT,1,8)
         CALL UHTOC(IQ(LBANK+MCIDFA),4,ACCT,8)
         CALL CFILL(' ',NODE,1,8)
         CALL UHTOC(IQ(LBANK+MCNIFA),4,NODE,8)
         CALL CFILL(' ',JOB,1,8)
         CALL UHTOC(IQ(LBANK+MCJIFA),4,JOB,8)
         WRITE (LWRITE,9017) OWNER,ACCT,NODE,JOB
 9017    FORMAT(' Created by:  ',A8,' ACCT: ',A8,' on node: ',A8,
     +           ' by job: ',A8)
      ENDIF
 
      IF (IOPTP .NE. 0) THEN
*
*     Physical attributes
*
         CALL UHTOC(IQ(LBANK+MRFMFA),4,FLFRM,4)
         WRITE(LWRITE,9018)
     +   FLFRM,IQ(LBANK+MRLNFA),IQ(LBANK+MBLNFA),IQ(LBANK+MFSZFA),
     +   IQ(LBANK+MUSCFA)
 9018    FORMAT(' RECFM: ',A5,' LRECL: ',I5,' BLKSIZE: ',I5,
     +          ' Filesize: ',I5,' Use count: ',I5)
      ENDIF
 
      IF((IOPTQ .NE. 0).AND.(IQ(LBANK+MMTPFA).GT.1)) THEN
*
*        Query TMS for this VID - if TMS option is not installed
*        FMQTMS will return defaults.
*        Set IQUEST(11) to media type in case volume unknown or
*        TMS option not installed.
*
           IF(IOPTM.EQ.0) THEN
              CALL UHTOC(IQ(LBANK+MVSNFA),4,VSN,6)
              CALL UHTOC(IQ(LBANK+MVIDFA),4,VID,6)
           ENDIF
           IQUEST(11) = IQ(LBANK+MMTPFA)
#if defined(CERNLIB_PREFIX)
           CALL FMXVID(VID,IQ(LBANK+MVIPFA),XVID,VIP,'C',IC)
           LXVID = LENOCC(XVID)
           CALL FMQTMS(XVID(1:LXVID),LIB,MODEL,DENS,MNTTYP,LABTYP,IC)
#endif
#if !defined(CERNLIB_PREFIX)
            CALL FMQTMS(VID,LIB,MODEL,DENS,MNTTYP,LABTYP,IC)
#endif
            IF(IC.EQ.100) THEN
               WRITE(LWRITE,*) 'Volume unknown to TMS'
            ELSE
               ITYPE = 1
               IF(MNTTYP.EQ.'M') ITYPE = 2
               WRITE(LWRITE,* ) 'Library = ',LIB,' model = ',MODEL//
     +         ROBMAN(ITYPE) ,' density = ',DENS,' label type = ',
     +         LABTYP
*
*     If tape volume found, display TMS tag fields
*
               CALL FMTAGS(GENAM(1:LGEN),LBANK-KOFUFA,KEYS,
     +                     CHTAG,'GT',IRC)
               IF(IRC.EQ.0) THEN
                  LTG = LENOCC(CHTAG)
                  IF(LTG.NE.0) WRITE (LWRITE,*) 'TMS text tag = ',
     +               CHTAG(1:LTG)
               ENDIF
               CALL FMTAGS(GENAM(1:LGEN),LBANK-KOFUFA,KEYS,
     +                     CHTAG,'GB',IRC)
               IF(IRC.EQ.0) THEN
                  LTG = LENOCC(CHTAG)
                  IF(LTG.NE.0) WRITE (LWRITE,*) 'TMS binary tag = ',
     +               CHTAG(1:LTG)
               ENDIF
               CALL FMTAGS(GENAM(1:LGEN),LBANK-KOFUFA,KEYS,
     +                     CHTAG,'GV',IRC)
               IF(IRC.EQ.0) THEN
                  LTG = LENOCC(CHTAG)
                  IF(LTG.NE.0) WRITE (LWRITE,*) 'TMS volinfo tag = ',
     +               CHTAG(1:LTG)
               ENDIF
            ENDIF
         ENDIF
 
      IF (IOPTR .NE. 0) THEN
         LLINE = LENOCC(CHLINE)
         WRITE(LWRITE,*) CHLINE(1:LLINE)
      ENDIF
 
      IF (IOPTS .NE. 0) THEN
*
*     Security attributes
*
         WRITE(LWRITE,9019) IQ(LBANK+MFPRFA)
 9019    FORMAT(' File protection mask: ',9X,Z8)
      ENDIF
 
      IF (IOPTT .NE. 0) THEN
*
*     Time attributes
*
         CALL FMUPTM(IDATE,ITIME,IQ(LBANK+MCRTFA),IRC)
         WRITE(LWRITE,9020) IDATE,ITIME
 9020    FORMAT(' Date and time of creation:     ',I6.6,1X,I4)
         CALL FMUPTM(IDATE,ITIME,IQ(LBANK+MCTTFA),IRC)
         WRITE(LWRITE,9021) IDATE,ITIME
 9021    FORMAT(' Date and time catalogued:      ',I6.6,1X,I4)
         CALL FMUPTM(IDATE,ITIME,IQ(LBANK+MLATFA),IRC)
         WRITE(LWRITE,9022) IDATE,ITIME
 9022    FORMAT(' Date and time last accessed:   ',I6.6,1X,I4)
      ENDIF
 
      IF (IOPTU .NE. 0) THEN
*
*     User attributes
*
         WRITE (LWRITE,9023) (IQ(LBANK+MUSWFA+J),J=0,9)
 9023    FORMAT(' User words (HEX):  ',5(2X,Z8,1X),/,20X,5(2X,Z8,1X))
 
         WRITE (LWRITE,9024) (IQ(LBANK+MUSWFA+J),J=0,9)
 9024    FORMAT(' User words (DEC):  ',5(I10,1X),/,20X,5(I10,1X))
      ENDIF
 
      IF (IOPTY .NE. 0) THEN
*
*     Verify bank contents
*
         IF(LWRITE.EQ.LPRTFA) THEN
            CALL FMVERI(GENAM(1:LGEN),LBANK-KOFUFA,KEYS,'A',IRC)
         ELSE
*
*     redirect output
*
            CALL FMVERI(GENAM(1:LGEN),LBANK-KOFUFA,KEYS,'AR',IRC)
         ENDIF
 
      ENDIF
 
      IF (IOPTZ .NE. 0) THEN
*
*     DZSHOW of bank
*
         IQPSAV = IQPRNT
         IQPRNT = LWRITE
         CALL DZSHOW('Contents of FATMEN bank',IDIVFA,LBANK-KOFUFA,
     +               'L',0,0,0,0)
         IQPRNT = IQPSAV
      ENDIF
 
   10 CALL UCOPY(IQ(LBANK+MUSWFA),IUSER,10)
      IF(IOPTU.NE.0)
     +CALL FMUPRT(GENAM,LBANK-KOFUFA,KEYS,IUSER,COMM,IRC)
 
      END
