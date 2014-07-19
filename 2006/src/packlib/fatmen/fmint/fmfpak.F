*
* $Id: fmfpak.F,v 1.1.1.1 1996/03/07 15:18:12 mclareni Exp $
*
* $Log: fmfpak.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:12  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
#if defined(CERNLIB_FPACK)
      SUBROUTINE FMFPAK(GENAM,LBANK,KEYS,CHSYMB,CHOPT,IRC)
 
      CHARACTER*(*) GENAM,CHSYMB
#include "fatmen/fatbank.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/faspac.inc"
#include "fatmen/fmnkeys.inc"
      DIMENSION     KEYS(LKEYFA)
      CHARACTER*255 CHCOMM,CHFILE
      CHARACTER*12  CHNREC,CHNRC2,CHRECL,CHBLF
      CHARACTER*9   CHACT
      CHARACTER*256 CHDSN
      CHARACTER*8   CHHOST
      CHARACTER*4   CHFORM
#include "fatmen/fatopts.inc"
 
      IRC = 0
 
      LGN = LENOCC(GENAM)
      LSM = LENOCC(CHSYMB)
*
*     Get file name
*
      CALL FMGDSN(LBANK,CHDSN,LDSN,IRC)
*
*     Get host name
*
      CALL UHTOC(IQ(LBANK+KOFUFA+MHSNFA),4,CHHOST,8)
      LHOST  = LENOCC(CHHOST)
*
*     Get package format
*
      CALL UHTOC(IQ(LBANK+KOFUFA+MFLFFA),4,CHFORM,4)
      LFORM  = LENOCC(CHFORM)
*
*     Action
*
      IF((IOPTR.EQ.0).AND.(IOPTW.EQ.0)) IOPTR = 1
 
      IF(IOPTR.EQ.0.AND.IOPTW.NE.0) THEN
         CHACT = 'WRITE'
         LCHACT = 5
      ELSEIF(IOPTR.NE.0.AND.IOPTW.NE.0) THEN
         CHACT = 'READWRITE'
         LCHACT = 9
      ELSE
         CHACT  = 'READ'
         LCHACT = 4
      ENDIF
*
*     build CHCOMM string for FPACK interpreter
*        OPEN symbolic-name FILE=filename HOST=hostname [options...]
*           options: RECL, BLFACTOR, NREC, NREC2, ACTION, ACCESS,
*                    STATUS, FORM, WORDFMT, RECSEP, NOOPEN
*
*           ACCESS   = sequential (FPT, FPS), direct (FPD), keyed (FPK),
*                      ordered (FPO)
*           FORM     = FPT = text, binary otherwise
*           NOOPEN   = IOPTU
*           WORDFMT  = MCPLFA
*           ACTION   = IOPTR & IOPTW (modify not supported)
*           STATUS   = OLD, unless action=write
*           NREC     = number of records, primary allocation
*           NREC2    = number of records, secondary allocation
*           RECSEP   = (not yet implemented)
*           RECL     = MRLNFA*4
*           BLFACTOR = MBLNFA/MRLNFA
*
         CHFILE = CHSYMB(1:LSM)
         LCHLUN = LSM
         CHCOMM = 'OPEN '//CHFILE(1:LCHLUN)//' FILE="'
     +            //CHDSN(1:LDSN)//'"'
     +            //' HOST='//CHHOST(1:LHOST)//' ACTION='
     +            //CHACT(1:LCHACT)
         LCOM   = LENOCC(CHCOMM)
*
*     RECL BLFACTOR
*
         IF(IQ(LBANK+KOFUFA+MRLNFA).GT.0) THEN
            CALL FMITOC(IQ(LBANK+KOFUFA+MRLNFA)*4,CHRECL,JS)
            CHCOMM(LCOM+1:LCOM+JS+6) = ' RECL='//CHRECL(1:JS)
            LCOM = LCOM + JS + 6
            IF(IQ(LBANK+KOFUFA+MBLNFA).GT.0) THEN
               CALL FMITOC(IQ(LBANK+KOFUFA+MBLNFA)/
     +            IQ(LBANK+KOFUFA+MRLNFA),CHBLF,JS)
               CHCOMM(LCOM+1:LCOM+JS+10) = ' BLFACTOR='//CHBLF(1:JS)
               LCOM = LCOM + JS + 10
            ENDIF
         ENDIF
*
*     Status: NEW enforced for ACTION=WRITE
*
         IF(IOPTW.NE.1.AND.IOPTR.EQ.0) THEN
            CHCOMM(LCOM+1:LCOM+11) = ' STATUS=NEW'
*
*      Allocation
*
            IF(NPRIFA.GT.0) THEN
*
*      Primary...
*
               CALL FMITOC(NPRIFA,CHNREC,JS)
               CHCOMM(LCOM+1:LCOM+JS+6) = ' NREC='//CHNREC(1:JS)
               LCOM = LCOM + JS + 6
               IF(IQUEST(13).GT.0) THEN
*
*      Secondary...
*
                  CALL FMITOC(NSECFA,CHNREC,JS)
                  CHCOMM(LCOM+1:LCOM+JS+7) = ' NREC2='//CHNRC2(1:JS)
                  LCOM = LCOM + JS + 7
               ENDIF
            ENDIF
         ELSE
            CHCOMM(LCOM+1:LCOM+11) = ' STATUS=OLD'
         ENDIF
         LCOM = LCOM + 11
*
*     Space: in case of new files, primary/secondary allocations
*     are taken from IQUEST(12-13), if non-zero
*
 
         IF(IOPTU.NE.0) THEN
            CHCOMM(LCOM+1:LCOM+7) = ' NOOPEN'
            LCOM = LCOM + 7
         ENDIF
*
*     WORDFMT...
*
      IF(IQ(LBANK+KOFUFA+MCPLFA).EQ.0) THEN
*
*        'local' i.e. native
*
         CHCOMM(LCOM+1:LCOM+16) = ' WORDFMT=WFLOCAL'
         LCOM = LCOM + 16
 
      ELSEIF(IQ(LBANK+KOFUFA+MCPLFA).EQ.1) THEN
*
*        IEEE big endian
*
         CHCOMM(LCOM+1:LCOM+15) = ' WORDFMT=WFIEEE'
         LCOM = LCOM + 15
 
      ELSEIF(IQ(LBANK+KOFUFA+MCPLFA).EQ.2) THEN
*
*        IBM
*
         CHCOMM(LCOM+1:LCOM+14) = ' WORDFMT=WFIBM'
         LCOM = LCOM + 14
 
      ELSEIF(IQ(LBANK+KOFUFA+MCPLFA).EQ.3) THEN
*
*        VAX
*
         CHCOMM(LCOM+1:LCOM+14) = ' WORDFMT=WFVAX'
         LCOM = LCOM + 14
 
      ELSEIF(IQ(LBANK+KOFUFA+MCPLFA).EQ.4) THEN
*
*        DECstation (IEEE little endian)
*
         CHCOMM(LCOM+1:LCOM+14) = ' WORDFMT=WFDEC'
         LCOM = LCOM + 14
 
      ELSEIF(IQ(LBANK+KOFUFA+MCPLFA).EQ.5) THEN
*
*        CRAY
*
         CHCOMM(LCOM+1:LCOM+15) = ' WORDFMT=WFCRAY'
         LCOM = LCOM + 15
 
      ENDIF
 
*
*     FPACK FORM and ACCESS parameters...
*
         IF(CHFORM(1:3).EQ.'FPT') THEN
*
*     text files
*
            CHCOMM(LCOM+1:LCOM+28) = ' ACCESS=SEQUENTIAL FORM=TEXT'
            LCOM = LCOM + 28
 
         ELSEIF(CHFORM(1:3).EQ.'FPS') THEN
*
*     binary sequential files
*
            CHCOMM(LCOM+1:LCOM+30) = ' ACCESS=SEQUENTIAL FORM=BINARY'
            LCOM = LCOM + 30
 
         ELSEIF(CHFORM(1:3).EQ.'FPD') THEN
*
*     binary direct access files
*
            CHCOMM(LCOM+1:LCOM+26) = ' ACCESS=DIRECT FORM=BINARY'
            LCOM = LCOM + 26
 
         ELSEIF(CHFORM(1:3).EQ.'FPK') THEN
*
*     binary keyed access files
*
            CHCOMM(LCOM+1:LCOM+25) = ' ACCESS=KEYED FORM=BINARY'
            LCOM = LCOM + 25
 
         ELSEIF(CHFORM(1:3).EQ.'FPO') THEN
*
*     binary ordered access files
*
            CHCOMM(LCOM+1:LCOM+27) = ' ACCESS=ORDERED FORM=BINARY'
            LCOM = LCOM + 27
 
         ENDIF
 
         IF(IDEBFA.GE.0) PRINT *,'FMOPEN. call FPARM for ',
     +      CHCOMM(1:LCOM)
         CALL FPARM(CHCOMM(1:LCOM))
         CALL FERMES(CHCOMM,1)
         IRC    = LENOCC(CHCOMM)
         IF(IRC.NE.0.AND.IDEBFA.GE.-3) PRINT *,'FMOPEN. error ',
     +      'from FPARM = ',CHCOMM(1:IRC)
         RETURN
 
      END
#endif
