*
* $Id: fmaddt.F,v 1.1.1.1 1996/03/07 15:18:03 mclareni Exp $
*
* $Log: fmaddt.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:03  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMADDT(GNAME,VSN,VID,FSEQ,DSN,FFORM,CPLEV,HOSTN,
     +                  RECFM,LRECL,BLOCK,FSIZE,MEDIA,
     +                  COMM,IVECT,CHOPT,IRC)
#include "fatmen/faust.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fatbug.inc"
#include "fatmen/fatusr.inc"
#include "fatmen/fatloc.inc"
#include "fatmen/fat.inc"
      COMMON/TRON/ LTHR,THRONG
      COMMON /QUEST/IQUEST(100)
      CHARACTER*8  THRONG
      CHARACTER*(*) GNAME,DSN,CHOPT
      CHARACTER*255 PATHN
      CHARACTER*20  FNAME
      CHARACTER*(*) FFORM
      CHARACTER*(*) HOSTN
      INTEGER       CPLEV,FSEQ
      CHARACTER*(*) VSN
      CHARACTER*(*) VID
      CHARACTER*8   VIP
      CHARACTER*255 PREDIR
      CHARACTER*(*) RECFM
      INTEGER       LRECL,BLOCK,FSIZE
      CHARACTER*(*) COMM
#include "fatmen/fmnkeys.inc"
      DIMENSION     KEYSI(LKEYFA)
      DIMENSION     KEYSO(LKEYFA,1)
*
      DIMENSION KEYS(LKEYFA)
      DIMENSION IVECT(10)
      SAVE     NENTRY
#include "fatmen/fatvid0.inc"
#include "fatmen/fatoptd.inc"
#include "fatmen/fatvid1.inc"
      DATA     NENTRY/0/
#include "fatmen/fatoptc.inc"
 
      NFADDT = NFADDT + 1
 
      IRC    = 0
      LVSN   = LENOCC(VSN)
      LVID   = LENOCC(VID)
      LGNAME = LENOCC(GNAME)
      LDSN   = LENOCC(DSN)
      LFFORM = LENOCC(FFORM)
      LHOSTN = LENOCC(HOSTN)
      LRECFM = LENOCC(RECFM)
      LCOMM  = LENOCC(COMM)
*
*     Has a VID prefix been given?
*
      JVIDP = INDEX(VID(1:LVID),'.')
      IF(JVIDP.NE.0) THEN
         VIP  = VID(1:JVIDP-1)
         LVIP = JVIDP - 1
         VID  = VID(JVIDP+1:LVID)
         LVID = LVID-JVIDP
         CALL CLTOU(VIP(1:LVIP))
         JP   = ICNTH(VIP(1:LVIP),PREVID,NTMS)
      ELSE
         JP   = 0
      ENDIF
 
 
      CALL VBLANK(KEYS(2),5)
      FNAME = GNAME(INDEXB(GNAME(1:LGNAME),'/')+1:LGNAME)
      LF    = LENOCC(FNAME)
      CALL UCTOH(FNAME,KEYS(2),4,LF)
*
*     Save current directory
*
      CALL FACDIR(PREDIR,'R')
 
      IF(NENTRY.EQ.0) THEN
         JBIAS = 2
         CALL FMBOOK(GNAME,KEYS,LADDBK,LSUP,JBIAS,IRC)
         NENTRY = 1
      ELSE
         CALL FMFILL(GNAME,LADDBK,KEYS,'A',IRC)
      ENDIF
 
      IF(IOPTE.NE.0) THEN
         CALL FMSELK(GNAME,KEYSI,KEYSO,NKEYS,1,IRET)
         IF(IRET.NE.0) THEN
            PRINT *,'FMADDT. Error from FMSELK for ',
     +              GNAME(1:LGNAME)
            IRC = IRET
            GOTO 99
         ENDIF
         CALL FMGETK(GNAME,LADDBK,KEYSO(1,1),IRET)
         IF(IRET.NE.0) THEN
            PRINT *,'FMADDT. Error from FMGETK for ',
     +              GNAME(1:LGNAME)
            IRC = IRET
            GOTO 99
         ENDIF
      IF(IDEBFA.GE.3)
     +   CALL FMSHOW(GNAME,LADDBK,KEYS,'A',IRC)
      ENDIF
*
*     Override various fields as required
*
      IF ((DSN(1:4) .NE. 'NONE') .AND. (LDSN.NE.0)) THEN
         CALL VBLANK(IQ(LADDBK+MFQNFA),MHSNFA-MFQNFA)
         CALL UCTOH(DSN,IQ(LADDBK+MFQNFA),4,LDSN)
      ENDIF
 
      IF ((HOSTN(1:8) .NE. 'THISNODE') .AND. (LHOSTN.NE.0)) THEN
         CALL CLTOU(HOSTN(1:LHOSTN))
         CALL VBLANK(IQ(LADDBK+MHSNFA),MCPLFA-MHSNFA)
         CALL UCTOH(HOSTN,IQ(LADDBK+MHSNFA),4,LHOSTN)
      ENDIF
 
      IF(LFFORM.NE.0) THEN
         CALL CLTOU(FFORM(1:LFFORM))
         CALL UCTOH(FFORM,IQ(LADDBK+MFLFFA),4,LFFORM)
      ENDIF
      IQ(LADDBK+MCPLFA) = CPLEV
      IQ(LADDBK+MMTPFA) = 1
      IQ(LADDBK+MLOCFA) = 1
      KEYS(MKCLFA)      = CPLEV
 
      IF(NUMLOC.EQ.1) THEN
         IQ(LADDBK+MLOCFA) = MFMLOC(1)
         KEYS(MKLCFA)      = MFMLOC(1)
      ENDIF
 
 
      IF(LCOMM.NE.0) THEN
         CALL VBLANK(IQ(LADDBK+MUCMFA),NWDSFA-MUCMFA+1)
         CALL UCTOH(COMM,IQ(LADDBK+MUCMFA),4,LCOMM)
      ENDIF
 
      IF(LRECFM.NE.0) THEN
         CALL CLTOU(RECFM(1:LRECFM))
         CALL UCTOH(RECFM,IQ(LADDBK+MRFMFA),4,LRECFM)
      ENDIF
      IF(LRECL.NE.0) IQ(LADDBK+MRLNFA) = LRECL
      IF(BLOCK.NE.0) IQ(LADDBK+MBLNFA) = BLOCK
      IF(FSIZE.NE.0) IQ(LADDBK+MFSZFA) = FSIZE
 
      IF(LVSN.NE.0)
     +CALL UCTOH(VSN,IQ(LADDBK+MVSNFA),4,LVSN)
      IF(LVID.NE.0)
     +CALL UCTOH(VID,IQ(LADDBK+MVIDFA),4,LVID)
      IF(JP.NE.0)    IQ(LADDBK+MVIPFA) = JP
      IF(FSEQ.NE.0)  IQ(LADDBK+MFSQFA) = FSEQ
      IQ(LADDBK+MMTPFA) = MEDIA
 
*     Set user words
*
      DO 20 I=1,10
         IF(IVECT(I).NE.0) IQ(LADDBK+MUSWFA+I-1) = IVECT(I)
   20 CONTINUE
*
*     Display entry
*
      IF((IDEBFA.GE.3).OR.(IOPTS.NE.0))
     +   CALL FMSHOW(GNAME,LADDBK,KEYS,'A',IRC)
*
*     Output this entry
*
      IF(IOPTN.EQ.0) THEN
         IF((IOPTM.EQ.0).AND.(IOPTR.EQ.0)) THEN
            CALL FMPUT(GNAME,LADDBK,IRC)
         ELSEIF(IOPTM.NE.0) THEN
            CALL FMMOD(GNAME,LADDBK,0,IRC)
         ELSEIF(IOPTR.NE.0) THEN
            CALL FMMOD(GNAME,LADDBK,1,IRC)
         ENDIF
      ENDIF
 
*
*     Reset current directory
*
      CALL FACDIR(PREDIR(1:LENOCC(PREDIR)),' ')
 
   99 END
