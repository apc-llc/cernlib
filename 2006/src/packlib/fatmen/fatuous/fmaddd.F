*
* $Id: fmaddd.F,v 1.1.1.1 1996/03/07 15:18:03 mclareni Exp $
*
* $Log: fmaddd.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:03  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMADDD(GNAME,DSN,FFORM,CPLEV,HOSTN,
     +                  RECFM,LRECL,BLOCK,FSIZE,
     +                  COMM,IVECT,CHOPT,IRC)
#include "fatmen/faust.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fatbug.inc"
#include "fatmen/fatusr.inc"
#include "fatmen/fatloc.inc"
#include "fatmen/fat.inc"
      COMMON/TRON/ LTHR,THRONG
#include "zebra/quest.inc"
      CHARACTER*8  THRONG
      CHARACTER*(*) GNAME,DSN,CHOPT
      CHARACTER*255 PATHN
      CHARACTER*20  FNAME
      CHARACTER*(*) FFORM
      CHARACTER*(*) HOSTN
      INTEGER       CPLEV,FSEQ
      CHARACTER*255 PREDIR
      CHARACTER*(*) RECFM
      INTEGER       LRECL,BLOCK,FSIZE
      CHARACTER*(*) COMM
#include "fatmen/fmnkeys.inc"
      DIMENSION     KEYSI(LKEYFA)
      DIMENSION     KEYSO(LKEYFA,1)
      DIMENSION KEYS(LKEYFA)
      DIMENSION IVECT(10)
      SAVE     NENTRY
#include "fatmen/fatvid0.inc"
#include "fatmen/fatoptd.inc"
#include "fatmen/fatvid1.inc"
      DATA     NENTRY/0/
#include "fatmen/fatoptc.inc"
 
      NFADDD = NFADDD + 1
*
*     Add a new file to the FATMEN file catalogue
*
      IRC    = 0
      LGNAME = LENOCC(GNAME)
      LDSN   = LENOCC(DSN)
      LFFORM = LENOCC(FFORM)
      LHOSTN = LENOCC(HOSTN)
      LCOMM  = LENOCC(COMM)
      LRECFM = LENOCC(RECFM)
*
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
            PRINT *,'FMADDD. Error from FMSELK for ',
     +              GNAME(1:LGNAME)
            IRC = IRET
            GOTO 99
         ENDIF
         CALL FMGETK(GNAME,LADDBK,KEYSO(1,1),IRET)
         IF(IRET.NE.0) THEN
            PRINT *,'FMADDD. Error from FMGETK for ',
     +              GNAME(1:LGNAME)
            IRC = IRET
            GOTO 99
         ENDIF
      ENDIF
*
*     Override various fields as required
*
      IF ((DSN(1:4) .NE. 'NONE') .AND. (LDSN.NE.0)) THEN
         IF(IDEBFA.GE.3) PRINT *,'FMADDD. setting DSN to ',
     +      DSN(1:LDSN)
         CALL VBLANK(IQ(LADDBK+MFQNFA),NFQNFA/4)
         CALL UCTOH(DSN,IQ(LADDBK+MFQNFA),4,LDSN)
      ENDIF
      IF(IDEBFA.GE.3) CALL FMSHOW(GNAME,LADDBK,KEYS,'A',IRC)
 
      IF ((HOSTN(1:8) .NE. 'THISNODE') .AND. (LHOSTN.NE.0)) THEN
         IF(IDEBFA.GE.3) PRINT *,'FMADDD. setting host name to ',
     +      HOSTN(1:LHOSTN)
         CALL CLTOU(HOSTN(1:LHOSTN))
         CALL VBLANK(IQ(LADDBK+MHSNFA),MCPLFA-MHSNFA)
         CALL UCTOH(HOSTN,IQ(LADDBK+MHSNFA),4,LHOSTN)
      ENDIF
 
      IF(LFFORM.NE.0) THEN
         IF(IDEBFA.GE.3) PRINT *,'FMADDD. setting file format to ',
     +      FFORM(1:LFFORM)
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
         IF(IDEBFA.GE.3) PRINT *,'FMADDD. setting comment to ',
     +      COMM(1:LCOMM)
         CALL VBLANK(IQ(LADDBK+MUCMFA),NWDSFA-MUCMFA+1)
         CALL UCTOH(COMM,IQ(LADDBK+MUCMFA),4,LCOMM)
      ENDIF
 
      IF(LRECFM.NE.0) THEN
         IF(IDEBFA.GE.3) PRINT *,'FMADDD. setting RECFM to ',
     +      RECFM(1:LRECFM)
         CALL CLTOU(RECFM(1:LRECFM))
         CALL UCTOH(RECFM,IQ(LADDBK+MRFMFA),4,LRECFM)
      ENDIF
      IF(LRECL.NE.0) IQ(LADDBK+MRLNFA) = LRECL
      IF(BLOCK.NE.0) IQ(LADDBK+MBLNFA) = BLOCK
      IF(FSIZE.NE.0) IQ(LADDBK+MFSZFA) = FSIZE
*
*     User words
*
      DO 20 I=1,10
         IF(IVECT(I).NE.0) IQ(LADDBK+MUSWFA-1+I) = IVECT(I)
   20 CONTINUE
*
*     Display entry
*
      IF(IDEBFA.GE.3) CALL FMSHOW(GNAME,LADDBK,KEYS,'A',IRC)
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
