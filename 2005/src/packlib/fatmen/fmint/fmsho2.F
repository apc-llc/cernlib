*
* $Id: fmsho2.F,v 1.1.1.1 1996/03/07 15:18:11 mclareni Exp $
*
* $Log: fmsho2.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:11  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMSHO2(GNAME,LBANK,KEYS,CHOPT,IRC)
*CMZ :          23/01/91  13.28.01  by  Jamie Shiers
*-- Author :    Jamie Shiers   23/01/91
*
*     Print alternative listing. Either 80 or 132 column format with
*     following details:
*
*     Trimmed generic name, VID/Hostname, FFORM, DCB, FSIZE, NACCESS,
*     NDAYS since last access, NCOPIES, Type of this entry (D/R/M)
*
*     Extended listing also includes Owner and DSN
*
#include "fatmen/fatsho.inc"
      CHARACTER*(*) GNAME,CHOPT
      CHARACTER*256 DSN
      CHARACTER*8   OWNER
      CHARACTER*4   RECFM,FFORM
      CHARACTER*3   FSIZE
      CHARACTER*1   FTYPE
      CHARACTER*6   VID
      CHARACTER*15  XVID
      CHARACTER*8   VIP
      CHARACTER*8   HOST
      CHARACTER*3   CHFSEQ
#include "fatmen/fmnkeys.inc"
      DIMENSION KEYS(LKEYFA)
      DIMENSION NDAYS(3)
      LOGICAL       IOPEN
#include "fatmen/fatout.inc"
#include "fatmen/fatbank.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/tmsdef0.inc"
#include "fatmen/fatoptd.inc"
#include "fatmen/tmsdef1.inc"
#include "fatmen/fatoptc.inc"
      IRC    = 0
      LGN    = LENOCC(GNAME)
      LCH    = INDEXB(GNAME(1:LGN),'/') + 1
 
      INQUIRE(3,OPENED=IOPEN)
      IF((OUTPUT(1:3).EQ.'TTY').OR.
     +   (LENOCC(OUTPUT).EQ.0) .OR.
     +   (.NOT.IOPEN))               THEN
         LWRITE = LPRTFA
      ELSE
         LWRITE = 3
      ENDIF
 
      IF(GNAME(1:LCH-2).NE.CWDSHO) THEN
         IF(IDEBFA.GE.0) THEN
            WRITE(LWRITE ,*)
            WRITE(LWRITE,*) 'Directory: ',GNAME(1:LCH-2)
            WRITE(LWRITE,*)
            IF(IOPTB.NE.0) WRITE(LWRITE,9007)
     +         'Generic name','KSN','Host/VID','FSQ','FORM',
     +         'RECF','LRECL','LBLCK','#MB','#AC','L/A',
     +          '#GN','T'
            IF(IOPTE.NE.0) WRITE(LWRITE,9008)
     +         'Generic name','KSN','Host/VID','FORM',
     +         'RECF','LRECL','LBLCK','#MB','#AC','L/A',
     +          '#GN','T','Owner','FSQ','DSN'
            WRITE(LWRITE,*)
         ENDIF
      ENDIF
      CWDSHO = GNAME(1:LCH-2)
      ISIZE  = IQ(LBANK+KOFUFA+MFSZFA)
      IUSE   = IQ(LBANK+KOFUFA+MUSCFA)
      CALL UHTOC(IQ(LBANK+KOFUFA+MCURFA),4,OWNER,8)
      CALL UHTOC(IQ(LBANK+KOFUFA+MFLFFA),4,FFORM,4)
      CALL UHTOC(IQ(LBANK+KOFUFA+MRFMFA),4,RECFM,4)
      IF(IQ(LBANK+KOFUFA+MMTPFA).EQ.1) THEN
         FTYPE  = 'D'
         CHFSEQ = '  '
         CALL UHTOC(IQ(LBANK+KOFUFA+MHSNFA),4,HOST,8)
      ELSE
         CALL UHTOC(IQ(LBANK+KOFUFA+MVIDFA),4,VID,6)
         HOST   = VID
         FTYPE  = 'M'
         IF(IQ(LBANK+KOFUFA+MFSQFA).GT.999) THEN
            CHFSEQ = '***'
         ELSE
            WRITE(CHFSEQ,'(I3.3)') IQ(LBANK+KOFUFA+MFSQFA)
         ENDIF
*
*        Query TMS for this VID
*        Set IQUEST(11) to media type in case volume unknown or
*        TMS option not installed.
*
         IQUEST(11) = IQ(LBANK+KOFUFA+MMTPFA)
#if defined(CERNLIB_PREFIX)
         CALL FMXVID(VID,IQ(LBANK+KOFUFA+MVIPFA),XVID,VIP,'C',IC)
         LXVID = LENOCC(XVID)
         CALL FMQTMS(XVID(1:LXVID),LIB,MODEL,DENS,MNTTYP,LABTYP,IC)
#endif
#if !defined(CERNLIB_PREFIX)
         CALL FMQTMS(VID,LIB,MODEL,DENS,MNTTYP,LABTYP,IC)
#endif
         IF(IC.EQ.0) THEN
            FTYPE = MNTTYP
         ENDIF
      ENDIF
*     Number of days since last access = NDAYS(3)
      CALL FMDAYS(GNAME(1:LGN),LBANK,KEYS,NDAYS,' ',IC)
*
      DSN = ' '
      CALL UHTOC(IQ(LBANK+KOFUFA+MFQNFA),4,DSN,NFQNFA)
      LDSN = MIN(LENOCC(DSN),40)
      CALL FMEXST(GNAME(1:LGN),NCOP)
      IFILE = IQ(LBANK+KOFUFA+MFSQFA)
*
*     File size
*
      IF(ISIZE.LE.999) THEN
         WRITE(FSIZE,9001) ISIZE
      ELSE
         WRITE(FSIZE,9002) ISIZE/1000
      ENDIF
 9001 FORMAT(I3)
 9002 FORMAT(I1,'GB')
*
*     Option B: Brief listing
*
      IF(IOPTB.NE.0) THEN
         IF(KEYS(1).LE.9999) THEN
            WRITE(LWRITE,9003) GNAME(LCH:LGN),KEYS(1),HOST,CHFSEQ,
     +      FFORM, RECFM,IQ(LBANK+KOFUFA+MRLNFA),IQ(LBANK+KOFUFA+
     +      MBLNFA), FSIZE,IUSE,NDAYS(3),NCOP,FTYPE
         ELSE
            WRITE(LWRITE,9003) GNAME(LCH:LGN),KEYS(1),HOST,CHFSEQ,
     +      FFORM, RECFM,IQ(LBANK+KOFUFA+MRLNFA),IQ(LBANK+KOFUFA+
     +      MBLNFA), FSIZE,IUSE,NDAYS(3),NCOP,FTYPE
         ENDIF
*
*     Option E: Extended listing
*
      ELSEIF(IOPTE.NE.0) THEN
         IF(KEYS(1).LE.9999) THEN
            WRITE(LWRITE,9005) GNAME(LCH:LGN),KEYS(1),HOST,FFORM,
     +      RECFM, IQ(LBANK+KOFUFA+MRLNFA),IQ(LBANK+KOFUFA+MBLNFA),
     +      FSIZE,IUSE,NDAYS(3),NCOP,FTYPE,OWNER,IFILE,DSN(1:LDSN)
         ELSE
            WRITE(LWRITE,9006) GNAME(LCH:LGN),KEYS(1),HOST,FFORM,
     +      RECFM, IQ(LBANK+KOFUFA+MRLNFA),IQ(LBANK+KOFUFA+MBLNFA),
     +      FSIZE,IUSE,NDAYS(3),NCOP,FTYPE,OWNER,IFILE,DSN(1:LDSN)
         ENDIF
      ENDIF
 9003 FORMAT(1X,A20,1X,I4,1X,A8,1X,A3,1X,A4,1X,A4,1X,I5,1X,I5,1X,A3,1X,
     +       I3,1X,I3,1X,I3,1X,A1)
 9004 FORMAT(1X,A20,1X,I6,1X,A8,1X,A3,1X,A4,1X,A4,1X,I5,1X,I5,1X,A3,1X,
     +       I3,1X,I3,1X,I3,1X,A1)
 9005 FORMAT(1X,A20,1X,I4,1X,A8,1X,A4,1X,A4,1X,I5,1X,I5,1X,A3,1X,
     +       I3,1X,I3,1X,I3,1X,A1,1X,A8,1X,I3,1X,A40)
 9006 FORMAT(1X,A20,1X,I6,1X,A8,1X,A4,1X,A4,1X,I5,1X,I5,1X,A3,1X,
     +       I3,1X,I3,1X,I3,1X,A1,1X,A8,1X,I3,1X,A40)
 9007 FORMAT(1X,A20,1X,A4,1X,A8,1X,A3,1X,A4,1X,A4,1X,A5,1X,A5,1X,A3,1X,
     +       A3,1X,A3,1X,A3,1X,A1)
 9008 FORMAT(1X,A20,1X,A4,1X,A8,1X,A4,1X,A4,1X,A5,1X,A5,1X,A3,1X,
     +       A3,1X,A3,1X,A3,1X,A1,1X,A8,1X,A3,1X,A40)
      END
