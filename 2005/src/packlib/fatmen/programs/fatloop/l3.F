*
* $Id: l3.F,v 1.1.1.1 1996/03/07 15:17:38 mclareni Exp $
*
* $Log: l3.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:38  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      PROGRAM L3
      PARAMETER     (NWPAW=500000)
      COMMON/PAWC/NWP,IXPAWC,IHDIV,IXHIGZ,IXKU,FENC(5),LMAIN,HCV(NWPAW)
#include "zebra/zunit.inc"
#include "zebra/rzcl.inc"
#include "zebra/rzdir.inc"
#include "zebra/rzch.inc"
#include "zebra/rzk.inc"
#include "fatmen/fatpara.inc"
      CHARACTER*20  FNAME
      CHARACTER*8   CHVID
      DIMENSION ISD(NLPATM),NSD(NLPATM),IHDIR(4)
 
      DIMENSION KEYS(10)
*
*-----------------------------------------------------------------------
*
      CALL HLIMIT(NWPAW)
 
      LRECL = 0
      CALL RZOPEN(1,'//CERN','cern.fatrz',' ',LRECL,IRC)
 
      CALL RZFILE(1,'CERN',' ')
 
      NDIRS  = 0
      NFILES = 0
 
      IQUEST(1)=0
      CALL RZCDIR(' ',' ')
      CALL RZPAFF(CHPAT,NLPAT,CHL)
      NLPAT0=NLPAT
      ITIME=0
*
*
*            Set CWD to the current level
*
  10  CONTINUE
      IF(ITIME.NE.0)THEN
         CALL RZPAFF(CHPAT,NLPAT,CHL)
         IF(IQUEST(1).NE.0)THEN
            NLPAT=NLPAT-1
            GO TO 20
         ENDIF
         CALL RZCDIR(CHL,' ')
      ENDIF
      IF(IQUEST(1).NE.0)THEN
         NLPAT=NLPAT-1
         GO TO 20
      ENDIF
      ISD(NLPAT)=0
      NSD(NLPAT)=IQ(KQSP+LCDIR+KNSD)
 
      NDIRS = NDIRS + 1
      LK = IQ(KQSP+LCDIR+KLK)
      NK = IQ(KQSP+LCDIR+KNKEYS)
      NWK= IQ(KQSP+LCDIR+KNWKEY)
      DO 22 I=1,NK
         K=LK+(NWK+1)*(I-1)
         DO 11 J=1,NWK
            IKDES=(J-1)/10
            IKBIT1=3*J-30*IKDES-2
            IF(JBYT(IQ(KQSP+LCDIR+KKDES+IKDES),IKBIT1,3).LT.3)THEN
               KEYS(J)=IQ(KQSP+LCDIR+K+J)
            ELSE
               CALL ZITOH(IQ(KQSP+LCDIR+K+J),KEYS(J),1)
            ENDIF
  11     CONTINUE
*
*     Select on media type
*
*---     IF(KEYS(MKMTFA).NE.2) GOTO 22
         IF(KEYS(MKMTFA).EQ.1) GOTO 22
*
*     Select on location code
*---     IF(KEYS(MKLCFA).NE.33) GOTO 22
*
*     Select on copy level
*
*---     IF(KEYS(MKCLFA).NE.1) GOTO 22
 
         CALL RZIN(IXPAWC,LSUP,2,KEYS,9999,' ')
         NFILES = NFILES + 1
*
*    GName
*    VID.FSEQ
*    File Format             *
*    Device Group
*    Creator                 *
*    Node                    *
*    RECFM
*    LRECL
*    BLKSIZE                 *
*    Filesize
*    Date/time creation      *
*
         CALL FMUPTM(IDATE,ITIME,IQ(KQSP+LSUP+MCRTFA),IRC)
 
         WRITE(6,9001) CHL(1:LENOCC(CHL)),(KEYS(J),J=2,6),
     +                 IQ(KQSP+LSUP+MVIDFA),IQ(KQSP+LSUP+MVIDFA+1),
     +                 IQ(KQSP+LSUP+MFSQFA),IQ(KQSP+LSUP+MMTPFA),
     +                 IQ(KQSP+LSUP+MCURFA),IQ(KQSP+LSUP+MCURFA+1),
     +                 IQ(KQSP+LSUP+MCNIFA),IQ(KQSP+LSUP+MCNIFA+1),
     +                 IQ(KQSP+LSUP+MRFMFA),
     +                 IQ(KQSP+LSUP+MRLNFA),IQ(KQSP+LSUP+MBLNFA),
     +                 IQ(KQSP+LSUP+MFSZFA),IDATE,ITIME
9001     FORMAT(' Generic name: ',A,'/',5A4,' VID: ',2A4,
     +          ' FSEQ: ',I3,' device type: ',I2,' creator: ',2A4,
     +          ' node: ',2A4,' recfm: ',A,' lrecl ',I6,' blksize ',I6,
     +          ' filesize: ',I6,' created: ',I6,'/',I4)
 
         CALL SBIT1(IQ(KQSP+LSUP),IQDROP)
*        CALL MZDROP(IXPAWC,LSUP,' ')
         LSUP = 0
  22  CONTINUE
 
*
*            Process possible down directories
*
  20  ISD(NLPAT)=ISD(NLPAT)+1
      IF(ISD(NLPAT).LE.NSD(NLPAT))THEN
         NLPAT=NLPAT+1
         LS=IQ(KQSP+LCDIR+KLS)
         IH=LS+7*(ISD(NLPAT-1)-1)
         CALL ZITOH(IQ(KQSP+LCDIR+IH),IHDIR,4)
         CALL UHTOC(IHDIR,4,CHPAT(NLPAT),16)
         ITIME=ITIME+1
         GO TO 10
      ELSE
         NLPAT=NLPAT-1
         IF(NLPAT.GE.NLPAT0)THEN
            LUP=LQ(KQSP+LCDIR+1)
            CALL MZDROP(JQPDVS,LCDIR,' ')
            LCDIR=LUP
            GO TO 20
         ENDIF
      ENDIF
*
*            Reset CWD
*
  90  CONTINUE
 
      CALL MZEND
 
      PRINT 9002,NFILES,NDIRS
9002  FORMAT(' Number of files = ',I6,' number of directories = ',I6)
*
 8000 FORMAT(' //',4A4,'   LUN=',I6,
     +' Created ',I6,'/',I4,'  Modified ',I6,'/',I4)
      END
