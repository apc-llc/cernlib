*
* $Id: fmkmod.F,v 1.1.1.1 1996/03/07 15:17:43 mclareni Exp $
*
* $Log: fmkmod.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:43  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMKMOD
#include "fatmen/faust.inc"
#include "fatmen/fmnkeys.inc"
#include "fatmen/fmaxcop.inc"
#include "fatmen/fatbank.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/slate.inc"
      DIMENSION     KEYS(LKEYFA)
      DIMENSION     KEYSIN(LKEYFA)
      DIMENSION     KEYSOU(LKEYFA,MAXCOP)
      CHARACTER*255 GNAME,GNAM1,CHDIR,CHPROM
      CHARACTER*4   FFORM,RECFM
      CHARACTER*80  CHVAL1,CHVAL2
      CHARACTER*80  COMM
      CHARACTER*36  CHOPT
      CHARACTER*20  FNAME
#include "fatmen/fatoptd.inc"
 
      CALL KUGETC(GNAM1,LGNAM1)
      CALL FMFIXF(GNAM1,GNAME)
      LGN = LENOCC(GNAME)
 
      CALL KUGETI(KSN)
      CALL KUGETI(JLOC)
      CALL KUGETI(JDAT)
      CALL KUGETI(JMED)
 
      CALL KUGETC(FFORM,LFORM)
      CALL KUGETC(RECFM,LRECF)
      CALL KUGETI(JRECL)
      CALL KUGETI(JBLOCK)
      CALL KUGETI(JSIZE)
      CALL KUGETC(COMM,LCOMM)
 
      CALL KUGETC(CHOPT,LCHOPT)
      IF(LCHOPT.EQ.0) THEN
         CALL VZERO(IOPT,36)
      ELSE
#include "fatmen/fatoptc.inc"
      ENDIF
 
      IF(IOPTI.NE.0) THEN
         IOPTK = 1
         IOPTF = 1
         IOPTL = 1
         IOPTP = 1
         IOPTT = 1
         IOPTC = 1
         IOPTU = 1
      ENDIF
 
      IPROM = IOPTK + IOPTF + IOPTL + IOPTP + IOPTT + IOPTC + IOPTU
 
      CALL RZCDIR(CHDIR,'R')
      LCDIR = LENOCC(CHDIR)
 
      IF(KSN.EQ.0) THEN
         CALL VZERO(KEYS,10)
      ELSE
         KEYS(1) = KSN
      ENDIF
 
      LPATH = INDEXB(GNAME(1:LGN),'/') -1
      FNAME = GNAME(LPATH+2:LGN)
      LNAME = LENOCC(FNAME)
*
*     Check how many copies of this dataset exist
*
      CALL UCOPY(KEYS,KEYSIN,10)
*
*     Don't compare media type, copy level or location code
*
      KEYSIN(MKMTFA) = -1
      KEYSIN(MKCLFA) = -1
      KEYSIN(MKLCFA) = -1
      NMATCH         = 0
      CALL FMSELK(GNAME(1:LGN),KEYSIN,KEYSOU,NMATCH,MAXCOP,IRC)
      IF(NMATCH.EQ.0) THEN
         IF(IDEBFA.GE.0)
     +   PRINT *,'FMKMOD. found 0 matches for ',GNAME(1:LGN)
         IRC = 1
         GOTO 40
      ELSEIF(NMATCH.GT.1.AND.KEYS(1).EQ.0.AND.IOPTA.EQ.0) THEN
         IF(IDEBFA.GE.0) THEN
            PRINT *,'FMKMOD. found ',NMATCH,' matches for ',
     +              GNAME(1:LGN)
            PRINT *,'FMKMOD. Please specify which entry is ',
     +              'to be modified'
         ENDIF
         IRC = 1
         GOTO 40
      ELSE
 
         IF(IDEBFA.GE.1)
     +   PRINT *,'FMKMOD. found ',NMATCH,' matches for ',GNAME(1:LGN)
 
         CALL UCOPY(KEYS,KEYSIN,10)
 
         IFOUND = 0
         DO 30 I=1,NMATCH
*
*     Was a specific key serial number specified?
*
         IF((KEYSIN(1).NE.KEYSOU(1,I)).AND.(KEYSIN(1).NE.0)) GOTO 30
 
         IFOUND = 1
         CALL UCOPY(KEYSOU(1,I),KEYS,10)
 
         IF(IDEBFA.GE.1) THEN
            PRINT *,'FMKMOD. candidate number ',I
            CALL FMPKEY(KEYSOU(1,I),LKEYFA)
         ENDIF
 
         LTDSFA = 0
         CALL FMGETK(GNAME(1:LGN),LTDSFA,KEYSOU(1,I),IRC)
         IF(IRC.NE.0) THEN
            IF(IDEBFA.GE.0) PRINT *,'FMKMOD. Return code ',IRC,' from '
     +      //'FMGETK'
            GOTO 40
         ENDIF
*
*     Prompt?
*
      IF(IPROM.EQ.0) THEN
*
*     No, override fields that were given
*
         IF(JLOC.NE.0) THEN
            IQ(LTDSFA+KOFUFA+MLOCFA) = JLOC
         ENDIF
 
         IF(JDAT.NE.0) THEN
            IQ(LTDSFA+KOFUFA+MCPLFA) = JDAT
         ENDIF
 
         IF(JMED.NE.0) THEN
            IF(JMED.NE.IQ(LTDSFA+KOFUFA+MMTPFA)) THEN
               PRINT *,'FMKMOD. sorry - cannot interchange media types'
               PRINT *,'FMKMOD. please use the mv command'
               GOTO 20
            ENDIF
            IQ(LTDSFA+KOFUFA+MMTPFA) = JMED
         ENDIF
 
         IF(LFORM.NE.0) THEN
            CALL VBLANK(IQ(LTDSFA+KOFUFA+MFLFFA),1)
            CALL UCTOH(FFORM,IQ(LTDSFA+KOFUFA+MFLFFA),4,LFORM)
         ENDIF
 
         IF(LRECF.NE.0) THEN
            CALL VBLANK(IQ(LTDSFA+KOFUFA+MRFMFA),1)
            CALL UCTOH(RECFM,IQ(LTDSFA+KOFUFA+MRFMFA),4,LRECF)
         ENDIF
 
         IF(JRECL.NE.0) THEN
            IQ(LTDSFA+KOFUFA+MRLNFA) = JRECL
         ENDIF
 
         IF(JBLOCK.NE.0) THEN
            IQ(LTDSFA+KOFUFA+MBLNFA) = JBLOCK
         ENDIF
 
         IF(JSIZE.NE.0) THEN
            IQ(LTDSFA+KOFUFA+MFSZFA) = JSIZE
         ENDIF
 
         IF(LCOMM.NE.0) THEN
            CALL VBLANK(IQ(LTDSFA+KOFUFA+MUCMFA),NUCMFA/4)
            CALL UCTOH(COMM,IQ(LTDSFA+KOFUFA+MUCMFA),4,LCOMM)
         ENDIF
 
      ELSE
*
*     Display specified fields and prompt for new values
*
         IF(IOPTK.NE.0) THEN
*
*    Copy level / data representation
*
            IVAL = 0
            WRITE(CHPROM,9001) IQ(LTDSFA+KOFUFA+MCPLFA)
            CALL KUPROI(CHPROM(1:LENOCC(CHPROM)),IVAL)
            IF(IVAL.NE.0) IQ(LTDSFA+KOFUFA+MCPLFA) = IVAL
*
*    Location code
*
            IVAL = 0
            WRITE(CHPROM,9002) IQ(LTDSFA+KOFUFA+MLOCFA)
            CALL KUPROI(CHPROM(1:LENOCC(CHPROM)),IVAL)
            IF(IVAL.NE.0) IQ(LTDSFA+KOFUFA+MLOCFA) = IVAL
*
*    Media type
*
            IVAL = 0
            WRITE(CHPROM,9003) IQ(LTDSFA+KOFUFA+MMTPFA)
            CALL KUPROI(CHPROM(1:LENOCC(CHPROM)),IVAL)
            IF(IVAL.NE.0) IQ(LTDSFA+KOFUFA+MMTPFA) = IVAL
         ENDIF
 
         IF(IOPTF.NE.0) THEN
*
*    Start record
*
            IVAL = 0
            WRITE(CHPROM,9004) IQ(LTDSFA+KOFUFA+MSRDFA)
            CALL KUPROI(CHPROM(1:LENOCC(CHPROM)),IVAL)
            IF(IVAL.NE.0) IQ(LTDSFA+KOFUFA+MSRDFA) = IVAL
*
*    End record
*
            IVAL = 0
            WRITE(CHPROM,9005) IQ(LTDSFA+KOFUFA+MERDFA)
            CALL KUPROI(CHPROM(1:LENOCC(CHPROM)),IVAL)
            IF(IVAL.NE.0) IQ(LTDSFA+KOFUFA+MERDFA) = IVAL
*
*    Start block
*
            IVAL = 0
            WRITE(CHPROM,9006) IQ(LTDSFA+KOFUFA+MSBLFA)
            CALL KUPROI(CHPROM(1:LENOCC(CHPROM)),IVAL)
            IF(IVAL.NE.0) IQ(LTDSFA+KOFUFA+MSBLFA) = IVAL
*
*    End block
*
            IVAL = 0
            WRITE(CHPROM,9007) IQ(LTDSFA+KOFUFA+MEBLFA)
            CALL KUPROI(CHPROM(1:LENOCC(CHPROM)),IVAL)
            IF(IVAL.NE.0) IQ(LTDSFA+KOFUFA+MEBLFA) = IVAL
         ENDIF
 
         IF(IOPTL.NE.0) THEN
*
*    File format
*
            CHVAL1 = ' '
            CHVAL2 = ' '
            CALL UHTOC(IQ(LTDSFA+KOFUFA+MFLFFA),4,CHVAL1,NFLFFA)
            WRITE(CHPROM,9008) CHVAL1
            CALL KUPROC(CHPROM(1:LENOCC(CHPROM)),CHVAL2,LVAL)
            IF(LVAL.NE.0) THEN
               CALL VBLANK(IQ(LTDSFA+KOFUFA+MFLFFA),1)
               CALL UCTOH(CHVAL2,IQ(LTDSFA+KOFUFA+MFLFFA),4,LVAL)
            ENDIF
*
*    User format
*
            CHVAL1 = ' '
            CHVAL2 = ' '
            CALL UHTOC(IQ(LTDSFA+KOFUFA+MFUTFA),4,CHVAL1,NFUTFA)
            WRITE(CHPROM,9009) CHVAL1
            CALL KUPROC(CHPROM(1:LENOCC(CHPROM)),CHVAL2,LVAL)
            IF(LVAL.NE.0) THEN
               CALL VBLANK(IQ(LTDSFA+KOFUFA+MFUTFA),1)
               CALL UCTOH(CHVAL2,IQ(LTDSFA+KOFUFA+MFUTFA),4,LVAL)
            ENDIF
         ENDIF
 
         IF(IOPTP.NE.0) THEN
*
*    Record format
*
            CHVAL1 = ' '
            CHVAL2 = ' '
            CALL UHTOC(IQ(LTDSFA+KOFUFA+MRFMFA),4,CHVAL1,NRFMFA)
            WRITE(CHPROM,9010) CHVAL1
            CALL KUPROC(CHPROM(1:LENOCC(CHPROM)),CHVAL2,LVAL)
            IF(LVAL.NE.0) THEN
               CALL VBLANK(IQ(LTDSFA+KOFUFA+MRFMFA),1)
               CALL UCTOH(CHVAL2,IQ(LTDSFA+KOFUFA+MRFMFA),4,LVAL)
            ENDIF
*
*     Record length
*
            IVAL = 0
            WRITE(CHPROM,9011) IQ(LTDSFA+KOFUFA+MRLNFA)
            CALL KUPROI(CHPROM(1:LENOCC(CHPROM)),IVAL)
            IF(IVAL.NE.0) IQ(LTDSFA+KOFUFA+MRLNFA) = IVAL
*
*     Block length
*
            IVAL = 0
            WRITE(CHPROM,9012) IQ(LTDSFA+KOFUFA+MBLNFA)
            CALL KUPROI(CHPROM(1:LENOCC(CHPROM)),IVAL)
            IF(IVAL.NE.0) IQ(LTDSFA+KOFUFA+MBLNFA) = IVAL
*
*     File size
*
            IVAL = 0
            WRITE(CHPROM,9013) IQ(LTDSFA+KOFUFA+MFSZFA)
            CALL KUPROI(CHPROM(1:LENOCC(CHPROM)),IVAL)
            IF(IVAL.NE.0) IQ(LTDSFA+KOFUFA+MFSZFA) = IVAL
*
*     Use count
*
            IVAL = 0
            WRITE(CHPROM,9014) IQ(LTDSFA+KOFUFA+MUSCFA)
            CALL KUPROI(CHPROM(1:LENOCC(CHPROM)),IVAL)
            IF(IVAL.NE.0) IQ(LTDSFA+KOFUFA+MUSCFA) = IVAL
         ENDIF
 
         IF(IOPTT.NE.0) THEN
*
*     Date and time created
*
            CALL FMUPTM(ID,IT,IQ(LTDSFA+KOFUFA+MCRTFA),IRC)
            CHVAL2 = ' '
            WRITE(CHPROM,9015) ID,IT
            CALL KUPROC(CHPROM(1:LENOCC(CHPROM)),CHVAL2,LVAL)
            IF(LVAL.NE.0) THEN
               ID = ICDECI(CHVAL2,1,LVAL)
               IT = ICDECI(CHVAL2,IS(1)+2,LVAL)
               CALL FMPKTM(ID,IT,IQ(LTDSFA+KOFUFA+MCRTFA),IRC)
            ENDIF
*
*     Date and time catalogued
*
            CALL FMUPTM(ID,IT,IQ(LTDSFA+KOFUFA+MCTTFA),IRC)
            CHVAL2 = ' '
            WRITE(CHPROM,9016) ID,IT
            CALL KUPROC(CHPROM(1:LENOCC(CHPROM)),CHVAL2,LVAL)
            IF(LVAL.NE.0) THEN
               ID = ICDECI(CHVAL2,1,LVAL)
               IT = ICDECI(CHVAL2,IS(1)+2,LVAL)
               CALL FMPKTM(ID,IT,IQ(LTDSFA+KOFUFA+MCTTFA),IRC)
            ENDIF
*
*     Date and time of last access
*
            CALL FMUPTM(ID,IT,IQ(LTDSFA+KOFUFA+MLATFA),IRC)
            CHVAL2 = ' '
            WRITE(CHPROM,9017) ID,IT
            CALL KUPROC(CHPROM(1:LENOCC(CHPROM)),CHVAL2,LVAL)
            IF(LVAL.NE.0) THEN
               ID = ICDECI(CHVAL2,1,LVAL)
               IT = ICDECI(CHVAL2,IS(1)+2,LVAL)
               CALL FMPKTM(ID,IT,IQ(LTDSFA+KOFUFA+MLATFA),IRC)
            ENDIF
         ENDIF
 
         IF(IOPTU.NE.0) THEN
*
*     User words
*
            DO 10 J=1,NUSWFA
               IVAL = 0
               WRITE(CHPROM,9018) J,IQ(LTDSFA+KOFUFA+MUSWFA+J-I)
               CALL KUPROI(CHPROM(1:LENOCC(CHPROM)),IVAL)
               IF(IVAL.NE.0) IQ(LTDSFA+KOFUFA+MUSWFA+J-I) = IVAL
   10       CONTINUE
*
*     Comment
*
            CHVAL1 = ' '
            CHVAL2 = ' '
            CALL UHTOC(IQ(LTDSFA+KOFUFA+MUCMFA),4,CHVAL1,NUCMFA)
            WRITE(CHPROM,9019) CHVAL1
            CALL KUPROC(CHPROM(1:LENOCC(CHPROM)),CHVAL2,LVAL)
            IF(LVAL.NE.0) THEN
               CALL VBLANK(IQ(LTDSFA+KOFUFA+MUCMFA),NUCMFA/4)
               CALL UCTOH(CHVAL2,IQ(LTDSFA+KOFUFA+MUCMFA),4,LVAL)
            ENDIF
 
         ENDIF
 
      ENDIF
*
*     Update keys
*
      KEYS(MKCLFA) = IQ(LTDSFA+KOFUFA+MCPLFA)
      KEYS(MKLCFA) = IQ(LTDSFA+KOFUFA+MLOCFA)
      KEYS(MKMTFA) = IQ(LTDSFA+KOFUFA+MMTPFA)
*
*     Put new file
*
      IFLAG  = 0
      CALL FMMOD(GNAME(1:LGN),LTDSFA,IFLAG,JRC)
      NFMODI = NFMODI + 1
   20 CONTINUE
      CALL MZDROP(IDIVFA,LTDSFA,' ')
      LTDSFA = 0
   30 CONTINUE
 
      ENDIF
 
      IF(KSN.NE.0.AND.IFOUND.EQ.0.AND.IDEBFA.GE.-2) PRINT *,'FMKMOD. ',
     +   ' no match found for ',GNAME(1:LGN),' key = ',KSN
 
   40 CONTINUE
*
*     Reset current directory
*
      CALL RZCDIR(CHDIR(1:LCDIR),' ')
 9001 FORMAT('Data representation: ',I4)
 9002 FORMAT('Location code      : ',I4)
 9003 FORMAT('Media type         : ',I4)
 9004 FORMAT('Start record       : ',I4)
 9005 FORMAT('End record         : ',I4)
 9006 FORMAT('Start block        : ',I4)
 9007 FORMAT('End block          : ',I4)
 9008 FORMAT('File format        : ',A4)
 9009 FORMAT('User format        : ',A4)
 9010 FORMAT('Record format      : ',A4)
 9011 FORMAT('Record length      : ',I4)
 9012 FORMAT('Block  length      : ',I4)
 9013 FORMAT('File size          : ',I4)
 9014 FORMAT('Use count          : ',I4)
 9015 FORMAT('Created     : ',I6,'.',I4)
 9016 FORMAT('Catalogued  : ',I6,'.',I4)
 9017 FORMAT('Last access : ',I6,'.',I4)
 9018 FORMAT('User word # ',I2.2,': ',I10)
 9019 FORMAT('Comment            : ',A)
      END
