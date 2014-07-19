*
* $Id: fmfill.F,v 1.1.1.1 1996/03/07 15:18:09 mclareni Exp $
*
* $Log: fmfill.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:09  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMFILL(GENAM,LFAT,KEYS,CHOPT,IRC)
#include "fatmen/fmpath.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fatbank.inc"
      INTEGER FMUSER,FMHOST,FMJOB,FMACNT
      CHARACTER*(*) GENAM,CHOPT
      PARAMETER       (LKEYFA=10)
      DIMENSION       KEYS(LKEYFA)
      CHARACTER*20    FILE
      CHARACTER*80    COMM
      CHARACTER*4     FFORM,FLFRM,UFORM
      CHARACTER*256   DSN
      CHARACTER*6     VSN,VID
      CHARACTER*8     CNAME,CTYPE,CSYS,CUSER,CJOB,CACCT
#include "fatmen/tmsdef0.inc"
#include "fatmen/fatoptd.inc"
#include "fatmen/tmsdef1.inc"
#include "fatmen/fatoptc.inc"
      IF(IOPTA.NE.0) THEN
#include "fatmen/fatoset.inc"
      ENDIF
      IF(INDEX(CHOPT,'Z').EQ.0) IOPTZ = 0
*
*     Fill the bank entry corresponding to the input generic name
*
      IRC = 0
      LGEN = LENOCC(GENAM)
      IF(LFAT.EQ.0) THEN
         CALL FMGETK(GENAM(1:LGEN),LBANK,KEYS,IRC)
         IF(IRC.NE.0) THEN
            IF(IDEBFA.GE.2)
     +      PRINT *,'FMFILL. Return code ',IRC,' from FMGETK'
            RETURN
         ENDIF
      ELSE
         IF(IDEBFA.GE.1)
     +   PRINT *,'FMFILL. Enter for user-supplied bank at address ',LFAT
         LBANK = LFAT + KOFUFA
      ENDIF
 
      ICH   = INDEXB(GENAM(1:LGEN),'/')
      FILE  = GENAM(ICH+1:LGEN)
      LFILE = LENOCC(FILE)
*
      IC = FMHOST(CNAME,CTYPE,CSYS)
      IC = FMJOB(CJOB)
      IC = FMUSER(CUSER)
      IC = FMACNT(CACCT)
*
*     Keys
*
      IF (IOPTK.NE.0) THEN
*
*     Fill KEYS vector
*
         CALL VBLANK(KEYS(2),5)
         CALL UCTOH(FILE(1:LFILE),KEYS(2),4,LFILE)
         KEYS(MKSRFA) = 0
         KEYS(MKCLFA) = 0
         KEYS(MKLCFA) = 1
         KEYS(MKMTFA) = 1
         KEYS(MKNBFA) = MKNBFA
         IF(IDEBFA.GE.2)
     +   PRINT *,'FMFILL. Setting KEYS vector to default values'
         IF(IDEBFA.GE.2) CALL FMPKEY(KEYS,10)
      ENDIF
 
      IF (IOPTC .NE. 0) THEN
*
*     Comment field
*
         IF(IDEBFA.GE.2)
     +   PRINT *,'FMFILL. comment field will be set to blanks'
         CALL CFILL(' ',COMM,1,80)
         CALL UCTOH(COMM,IQ(LBANK+MUCMFA),4,80)
      ENDIF
 
      IF (IOPTF.NE.0) THEN
*
*     File attributes
*
         IQ(LBANK+MSRDFA) = 0
         IQ(LBANK+MERDFA) = 0
         IQ(LBANK+MSBLFA) = 0
         IQ(LBANK+MEBLFA) = 0
      ENDIF
 
      IF (IOPTL.NE.0) THEN
*
*     Logical attributes
*
         CALL UCTOH('UN  ',IQ(LBANK+MFLFFA),4,4)
         CALL UCTOH('    ',IQ(LBANK+MFUTFA),4,4)
         IF(IDEBFA.GE.2)
     +   PRINT *,'FMFILL. Setting logical attributes to default values'
      ENDIF
 
      IF (IOPTM.NE.0) THEN
*
*     Media attributes
*
         IF (IQ(LBANK+MMTPFA) .EQ. 1) THEN
*
*        Disk dataset, set host type and O/S
*
*
            CALL VBLANK(IQ(LBANK+MHSNFA),NHSNFA/4)
            CALL VBLANK(IQ(LBANK+MHSTFA),NHSTFA/4)
            CALL VBLANK(IQ(LBANK+MHOSFA),NHOSFA/4)
            CALL UCTOH(CNAME,IQ(LBANK+MHSNFA),4,LENOCC(CNAME))
            CALL UCTOH(CTYPE,IQ(LBANK+MHSTFA),4,LENOCC(CTYPE))
            CALL UCTOH(CSYS ,IQ(LBANK+MHOSFA),4,LENOCC(CSYS))
         ELSE
*
*        Tape dataset
*
*
*        Update to indicate a tape file...
*
            KEYS(MKMTFA) = 2
            IQ(LBANK+MMTPFA) = 2
 
            CALL UCTOH('        ',IQ(LBANK+MVSNFA),4,8)
            CALL UCTOH('        ',IQ(LBANK+MVIDFA),4,8)
            IQ(LBANK+MDENFA) = 38000
            IQ(LBANK+MFSQFA) = 1
            IQ(LBANK+MVSQFA) = 1
            IQ(LBANK+MVIPFA) = 0
         ENDIF
      ENDIF
 
      IF (IOPTN.NE.0) THEN
*
*     Name attributes (DSN on disk/tape)
*
         CALL VBLANK(IQ(LBANK+MFQNFA),NFQNFA/4)
      ENDIF
 
      IF (IOPTO.NE.0) THEN
*
*     Owner attributes
*
         IF(IDEBFA.GE.2)
     +   PRINT *,'FMFILL. Setting owner attributes'
         IF(IDEBFA.GE.3)
     +   PRINT *,'FMFILL. Owner will be set to ',
     +            CUSER(1:LENOCC(CUSER))
         CALL VBLANK(IQ(LBANK+MCURFA),NCURFA/4)
         CALL UCTOH(CUSER,IQ(LBANK+MCURFA),4,LENOCC(CUSER))
         IF(IDEBFA.GE.3)
     +   PRINT *,'FMFILL. Current node will be set to ',
     +            CNAME(1:LENOCC(CNAME))
         CALL VBLANK(IQ(LBANK+MCNIFA),NCNIFA/4)
         CALL UCTOH(CNAME,IQ(LBANK+MCNIFA),4,LENOCC(CNAME))
         IF(IDEBFA.GE.3)
     +   PRINT *,'FMFILL. Account will be set to ',
     +            CACCT(1:LENOCC(CACCT))
         CALL VBLANK(IQ(LBANK+MCIDFA),NCIDFA/4)
         CALL UCTOH(CACCT,IQ(LBANK+MCIDFA),4,LENOCC(CACCT))
         IF(IDEBFA.GE.3)
     +   PRINT *,'FMFILL. Job will be set to ',
     +           CJOB(1:LENOCC(CJOB))
         CALL VBLANK(IQ(LBANK+MCJIFA),NCJIFA/4)
         CALL UCTOH(CJOB, IQ(LBANK+MCJIFA),4,LENOCC(CJOB))
      ENDIF
 
      IF (IOPTP.NE.0) THEN
*
*     Physical attributes
*
         IF(IDEBFA.GE.2)
     +   PRINT *,'FMFILL. Setting physical attributes to zero/blank'
         CALL UCTOH('    ',IQ(LBANK+MRFMFA),4,4)
         IQ(LBANK+MRLNFA) = 0
         IQ(LBANK+MBLNFA) = 0
         IQ(LBANK+MFSZFA) = 0
         IQ(LBANK+MUSCFA) = 0
      ENDIF
 
      IF (IOPTS.NE.0) THEN
*
*     Security attributes
*
         IF(IDEBFA.GE.2) PRINT *,'FMFILL. security word will be zeroed'
         IQ(LBANK+MFPRFA) = 0
      ENDIF
 
      IF (IOPTT.NE.0) THEN
*
*     Time attributes
*
         CALL DATIME(IDATE,ITIME)
         CALL FMPKTM(IDATE,ITIME,IPACK,IRC)
         IF(IDEBFA.GE.2)
     +   PRINT *,'FMFILL. date & time fields will be set to ',
     +           IDATE,ITIME
         IQ(LBANK+MCRTFA) = IPACK
         IQ(LBANK+MCTTFA) = IPACK
         IQ(LBANK+MLATFA) = IPACK
      ENDIF
 
      IF (IOPTU.NE.0) THEN
*
*     User words
*
         IF(IDEBFA.GE.2) PRINT *,'FMFILL. User words will be zeroed'
         CALL VZERO(IQ(LBANK+MUSWFA),10)
      ENDIF
*
*     Display bank if option Z specfied
*
      IF(IOPTZ.NE.0) CALL FMSHOW(GENAM,LBANK-KOFUFA,KEYS,'A',IRC)
 
      END
