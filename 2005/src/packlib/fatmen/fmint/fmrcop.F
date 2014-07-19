*
* $Id: fmrcop.F,v 1.2 1997/10/23 13:39:40 mclareni Exp $
*
* $Log: fmrcop.F,v $
* Revision 1.2  1997/10/23 13:39:40  mclareni
* NT mods
*
* Revision 1.1.1.1  1996/03/07 15:18:11  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMRCOP(GENAM1,LBANK1,KEYS1,GENAM2,LBANK2,KEYS2,
     +CHOPT,IRC)
      CHARACTER*(*) CHOPT
#include "fatmen/faust.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fatbank.inc"
#include "fatmen/fatstg.inc"
#include "fatmen/fattyp.inc"
#include "fatmen/czsock.inc"
      CHARACTER*8   DDNAM1,DDNAM2
      CHARACTER*4   CHOPE
      CHARACTER*20  FN1,FN2
      CHARACTER*(*) GENAM1,GENAM2
      CHARACTER*256 DSN1,DSN2,LOCAL,REMOTE
      CHARACTER*8   USER1,ADDR1,USER2,ADDR2
      CHARACTER*8   NODE1,NODE2
      CHARACTER*40  DCB1,DCB2
      CHARACTER*4   RECFM1,RECFM2
      CHARACTER*4   FFORM1,FFORM2
      DIMENSION     BUFFER(8172)
      PARAMETER (LKEYFA=10)
      DIMENSION KEYS1(LKEYFA),KEYS2(LKEYFA)
      INTEGER       FMNODE
#if defined(CERNLIB_VAXVMS)
      INCLUDE '($SSDEF)'
      STRUCTURE /ITMLST/
      UNION
      MAP
      INTEGER*2 BUFFER_LENGTH
      INTEGER*2 ITEM_CODE
      INTEGER*4 BUFFER_ADDRESS
      INTEGER*4 RETURN_LENGTH_ADDRESS
      ENDMAP
      MAP
      INTEGER*4 END_LIST /0/
      ENDMAP
      END UNION
      END STRUCTURE
 
      RECORD /ITMLST/ SYI_LIST(2)
 
      INCLUDE '($SYIDEF)'
#endif
#include "fatmen/fatran0.inc"
#include "fatmen/tmsdef0.inc"
#include "fatmen/fatvid0.inc"
#include "fatmen/fatopt0.inc"
#include "fatmen/fatform.inc"
#include "fatmen/fatopt1.inc"
#include "fatmen/fatran1.inc"
#include "fatmen/tmsdef1.inc"
#include "fatmen/fatvid1.inc"
#include "fatmen/fatoptc.inc"
#if !defined(CERNLIB_CSPACK)
      IF(IDEBFA.GE.0) THEN
         PRINT *,'FMRCOP. dummy version of FMRCOP installed.'
         PRINT *,'FMRCOP. +USE,CSPACK to get a functional version.'
      ENDIF
      IRC = 99
 
#endif
#if defined(CERNLIB_CSPACK)
      IRC = 0
      NFMCOPR = NFMCOPR + 1
      LG1 = LENOCC(GENAM1)
      LG2 = LENOCC(GENAM2)
      FATMBN = FATMBN + Q(LBANK1+KOFUFA+MFSZFA)
*
*     Set log level if XZ package to the same as FATMEN
*
      CALL XZLOGL(IDEBFA)
*
*     Set protocol
*
      IPROT = ITRANS
 
      IF(IDEBFA.GE.0) PRINT *,'FMRCOP. enter for',
     +   ' GENAM1 = ',GENAM1(1:LG1),
     +   ' GENAM2 = ',GENAM2(1:LG2)
*
*     Determine whether to do a GET or a PUT...
*
      CALL FMGETC(LBANK1,NODE1,MHSNFA,NHSNFA,IC)
      CALL FMGETC(LBANK2,NODE2,MHSNFA,NHSNFA,IC)
      LNODE1 = LENOCC(NODE1)
      LNODE2 = LENOCC(NODE2)
      IPUT   = FMNODE(NODE1(1:LNODE1))
      IGET   = FMNODE(NODE2(1:LNODE2))
 
      IF(IDEBFA.GE.0) PRINT *,'FMRCOP. NODE1/2 = ',NODE1,NODE2
*
*     Get input and output dsn
*
      CALL FMGETC(LBANK1,DSN1,MFQNFA,NFQNFA,IC)
      CALL FMGETC(LBANK2,DSN2,MFQNFA,NFQNFA,IC)
      LDSN1 = LENOCC(DSN1)
      LDSN2 = LENOCC(DSN2)
 
      IF((IPUT.NE.0).AND.(IGET.NE.0)) THEN
         IF(IDEBFA.GE.0) PRINT *,'FMRCOP. Error - both nodes are remote'
         IRC = 1
         RETURN
      ENDIF
 
      IF((IPUT.EQ.0).AND.(IGET.EQ.0)) THEN
         IF(IDEBFA.GE.0) PRINT *,'FMRCOP. Both nodes are local'
         IRC = -1
         RETURN
      ENDIF
*
*     Get LRECL
*
      IF(IPUT.EQ.0) THEN
         LRECL = IQ(LBANK1+KOFUFA+MRLNFA) * 4
      ELSE
         LRECL = IQ(LBANK2+KOFUFA+MRLNFA) * 4
      ENDIF
*
*     Determine operation: GET/PUTA, B, FZ, P, RZ
*
      CALL FMGETC(LBANK1,FFORM1,MFLFFA,NFLFFA,IC)
      CALL FMGETC(LBANK2,FFORM2,MFLFFA,NFLFFA,IC)
      LFORM1 = LENOCC(FFORM1)
      LFORM2 = LENOCC(FFORM2)
 
      IF(IDEBFA.GE.0) PRINT *,'FMRCOP. FFORM1/2 = ',FFORM1,FFORM2
 
      IFORM1 = ICNTH(FFORM1(1:LFORM1),FATFRM,NFATFM)
      IFORM2 = ICNTH(FFORM2(1:LFORM2),FATFRM,NFATFM)
      IF((IFORM1.NE.IFORM2).AND.(IDEBFA.GE.0)) THEN
*
*     This test may lose validity ...
*
         IF(FFORM1(1:1).NE.FFORM2(1:1)) THEN
            IF(IDEBFA.GE.-3)
     +   PRINT *,'FMRCOP. Error - input and output files have ',
     +   ' conflicting formats: ',
     +   ' Input: ',FFORM1(1:LFORM1),' output: ',FFORM2(1:LFORM2)
            IRC = 1
            RETURN
         ELSE
            IF(IDEBFA.GE.0)
     +   PRINT *,'FMRCOP. Warning - input and output files have ',
     +   ' different formats: ',
     +   ' Input: ',FFORM1(1:LFORM1),' output: ',FFORM2(1:LFORM2)
         ENDIF
      ENDIF
*
*     Ensure that a connection exists to the remote node
*
      IF(IPUT.EQ.0) THEN
         LOCAL  = DSN1
         REMOTE = DSN2
         LENLOC = LDSN1
         LENREM = LDSN2
         CALL CZSWAP(NODE2(1:LNODE2),-1,IOPEN)
         IF(IOPEN.EQ.-1) THEN
            CALL CZOPEN('zserv',NODE2(1:LNODE2),IC)
            IF(IC.NE.0) THEN
               IF(IDEBFA.GE.-3) PRINT *,'FMRCOP. cannot open ', 'commun'
     +         //'ication with ',NODE2(1:LNODE2)
               IRC = IC
               RETURN
            ENDIF
         ENDIF
      ELSE
         LOCAL  = DSN2
         REMOTE = DSN1
         LENREM = LDSN1
         LENLOC = LDSN2
         CALL CZSWAP(NODE1(1:LNODE1),-1,IOPEN)
         IF(IOPEN.EQ.-1) THEN
            CALL CZOPEN('zserv',NODE1(1:LNODE1),IC)
            IF(IC.NE.0) THEN
               IF(IDEBFA.GE.-3) PRINT *,'FMRCOP. cannot open ', 'commun'
     +         //'ication with ',NODE1(1:LNODE1)
               IRC = IC
               RETURN
            ENDIF
         ENDIF
      ENDIF
*
*     Now try to transfer the file...
*
*     FZ:  XZGET/PUTF
*     FA:  XZGET/PUTA
*     FX:  XZGET/PUTB
*     FFX: XZGET/PUTB
*     RZ:  XZGET/PUTR
*     EP:  XZGET/PUTB
*     AS:  XZGET/PUTA
*     UN:  - no can do -
*     DA:  XZGET/PUTD
*
      IRET = 0
 
 
      IF(IDEBFA.GE.3) PRINT *,'FMRCOP. local file = ',
     +   LOCAL(1:LENLOC),' remote file = ',
     +   REMOTE(1:LENREM)
 
      IF(FFORM1(1:1).EQ.'R') THEN
*
*      Transfer an RZ file
*
         IF(IPUT.EQ.0) THEN
            IF(IDEBFA.GE.0) PRINT *,'FMRCOP. transferring ',
     +         'file using XZPUTR'
            CALL XZPUTR(LOCAL(1:LENLOC),REMOTE(1:LENREM),'QS',IRET)
         ELSE
            IF(IDEBFA.GE.0) PRINT *,'FMRCOP. transferring ',
     +         'file using XZGETR'
            CALL XZGETR(LOCAL(1:LENLOC),REMOTE(1:LENREM),'QS',IRET)
         ENDIF
      ELSEIF(FFORM1(1:2).EQ.'DA') THEN
*
*      Transfer a direct access file
*
         IF(IPUT.EQ.0) THEN
            IF(IDEBFA.GE.0) PRINT *,'FMRCOP. transferring ',
     +         'file using XZPUTD'
            CALL XZPUTD(LOCAL(1:LENLOC),REMOTE(1:LENREM),
     +                  LRECL,'QS',IRET)
         ELSE
            IF(IDEBFA.GE.0) PRINT *,'FMRCOP. transferring ',
     +         'file using XZGETD'
            CALL XZGETD(LOCAL(1:LENLOC),REMOTE(1:LENREM),
     +                  LRECL,'QS',IRET)
         ENDIF
      ELSEIF(FFORM1(1:2).EQ.'EP') THEN
*
*      Transfer an EPIO file
*
         IF(IPUT.EQ.0) THEN
            IF(IDEBFA.GE.0) PRINT *,'FMRCOP. transferring ',
     +         'file using XZPUTB'
            CALL XZPUTB(LOCAL(1:LENLOC),REMOTE(1:LENREM),
     +                  LRECL,'QS',IRET)
         ELSE
            IF(IDEBFA.GE.0) PRINT *,'FMRCOP. transferring ',
     +         'file using XZGETB'
            CALL XZGETB(LOCAL(1:LENLOC),REMOTE(1:LENREM),
     +                  LRECL,'QS',IRET)
         ENDIF
      ELSEIF((FFORM1(1:1).EQ.'A').OR.(FFORM1(1:2).EQ.'FA')) THEN
*
*      Transfer a text file
*
         IF(IPUT.EQ.0) THEN
            IF(IDEBFA.GE.0) PRINT *,'FMRCOP. transferring ',
     +         'file using XZPUTA'
            CALL XZPUTA(LOCAL(1:LENLOC),REMOTE(1:LENREM),'QS',IRET)
         ELSE
            IF(IDEBFA.GE.0) PRINT *,'FMRCOP. transferring ',
     +         'file using XZGETA'
            CALL XZGETA(LOCAL(1:LENLOC),REMOTE(1:LENREM),'QS',IRET)
         ENDIF
      ELSEIF(FFORM1(1:1).EQ.'F') THEN
*
*      Transfer an FZ file:
*
         IF((INDEX(FFORM1(1:LFORM1),'FX').NE.0).AND. (INDEX(FFORM1(1:
     +   LFORM1),'FX').NE.0)) THEN
*
*      Exchange format, binary mapping
*
            IF(IPUT.EQ.0) THEN
               IF(IDEBFA.GE.0) PRINT *,'FMRCOP. transferring ', 'file '
     +         //'using XZPUTB'
               CALL XZPUTB(LOCAL(1:LENLOC),REMOTE(1:LENREM),
     +                  LRECL,'QS',IRET)
            ELSE
               IF(IDEBFA.GE.0) PRINT *,'FMRCOP. transferring ', 'file '
     +         //'using XZGETB'
               CALL XZGETB(LOCAL(1:LENLOC),REMOTE(1:LENREM),
     +                  LRECL,'QS',IRET)
            ENDIF
         ELSEIF((INDEX(FFORM1(1:LFORM1),'FA').NE.0).AND. (INDEX(FFORM1(
     +   1:LFORM1),'FA').NE.0)) THEN
*
*      Exchange format, ASCII mapping
*
            IF(IPUT.EQ.0) THEN
               IF(IDEBFA.GE.0) PRINT *,'FMRCOP. transferring ', 'file '
     +         //'using XZPUTA'
               CALL XZPUTA(LOCAL(1:LENLOC),REMOTE(1:LENREM),'QS',IRET)
            ELSE
               IF(IDEBFA.GE.0) PRINT *,'FMRCOP. transferring ', 'file '
     +         //'using XZGETA'
               CALL XZGETA(LOCAL(1:LENLOC),REMOTE(1:LENREM),'QS',IRET)
            ENDIF
         ELSE
*
*     Any other FZ formats/combinations
*
            IF(IPUT.EQ.0) THEN
               IF(IDEBFA.GE.0) PRINT *,'FMRCOP. transferring ', 'file '
     +         //'using XZPUTF'
               CALL XZPUTF(LOCAL(1:LENLOC),REMOTE(1:LENREM),
     +                  LRECL,LFORM,RFORM,'QS',IRET)
            ELSE
               IF(IDEBFA.GE.0) PRINT *,'FMRCOP. transferring ', 'file '
     +         //'using XZGETF'
               CALL XZGETF(LOCAL(1:LENLOC),REMOTE(1:LENREM),
     +                  LRECL,LFORM,RFORM,'QS',IRET)
            ENDIF
         ENDIF
      ELSE
         PRINT *,'FMRCOP. Cannot process requested formats - ', ' '
     +   //'Input: ',FFORM1(1:LFORM1),' output: ',FFORM2(1:LFORM2)
         IRC = 1
         RETURN
      ENDIF
*
*     Close connection to remode node
*
      IF(IOPEN.EQ.-1) THEN
      IF(IOPTK.EQ.0) CALL CZCLOS(ISTAT)
      ELSE
      CALL CZSWAP(' ',-1,ISTAT)
      ENDIF
*
*     Now update catalogue if transfer successful
*
      IF(IRET.EQ.0) THEN
*
*     Get file size transferred
*
*        IQUEST(11) = NR
*        IQUEST(12) = NKILO
*        IQUEST(13) = RATE
*        IQUEST(14) = IHOUR
*        IQUEST(15) = IMIN
*        IQUEST(16) = ISEC
*        IQUEST(17) = T
*
         IF(IOPTU.NE.0) THEN
            IQ(LBANK2+KOFUFA+MFSZFA) = IQUEST(12) / 1024
         ENDIF
 
      ELSE
         PRINT *,'FMRCOP. Return code ',IRET,' from file transfer'
         PRINT *,'FMRCOP. Catalogue will not be updated'
         IRC = IRET
         RETURN
      ENDIF
#endif
      END
