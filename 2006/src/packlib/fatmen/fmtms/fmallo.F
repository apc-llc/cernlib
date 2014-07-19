*
* $Id: fmallo.F,v 1.1.1.1 1996/03/07 15:18:14 mclareni Exp $
*
* $Log: fmallo.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:14  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMALLO(MEDIA,CDENS,COMP,LIBRARY,POOL,LBANK,CHOPT,
     +VSN,VID,IRC)
*
*     Routine to allocate next free piece of media from TMS
*     MEDIA is the medium type, e.g. 3480, 8200 etc.
*     CDENS is the density, e.g. 38K, 76K etc.
*     COMP  is the compression type (to be defined).
*     LIBRARY is the two letter experimental series followed by location
*       e.g. PD_DPVAULT
*     POOL    is a named pool within this library, e.g. MDST
*     LBANK   is the address of the FATMEN bank to be updated
*     CHOPT   M - write multi-file tape until tape is full
*             C - write single file tape until full (maybe one day)
*
*     Return codes: IRC = 0 - success
*                        -1 - media type conflict
*                        >0 - return code from TMS
*
#include "fatmen/slate.inc"
#include "fatmen/faust.inc"
#include "fatmen/fatbank.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fattyp.inc"
      CHARACTER*(*) MEDIA,LIBRARY,POOL,VSN,VID,CDENS,COMP
      CHARACTER*80  COMAND,RETCOM
#include "fatmen/tmsrep.inc"
      PARAMETER     (LENTAP=100)
      CHARACTER*132 TAPREP(LENTAP)
      CHARACTER*8   GROUP
#include "fatmen/fatopts.inc"
 
      NFAVOL = NFAVOL + 1
#if (!defined(CERNLIB_TMS))&&(defined(CERNLIB_VMTAPE))
*
*     Allocation is not actually performed until we do the mount...
*
#endif
#if (!defined(CERNLIB_TMS))&&(!defined(CERNLIB_VMTAPE))
*
*     Call user exit
*
      CALL FMUALL(MEDIA,CDENS,COMP,LIBRARY,POOL,LBANK,CHOPT,
     +VSN,VID,IRC)
#endif
#if defined(CERNLIB_TMS)
      IRC = 0
      NRETRY = 0
*
*     Get list of free tapes in requested pool & library
*
      COMAND = ' '
      WRITE(COMAND,9001) LIBRARY,POOL
 9001 FORMAT('QUERY CONTENTS LIBRARY ',A,' USER ',A,' SLOT 1')
   10 CONTINUE
      LC = LENOCC(COMAND)
      IF(IDEBFA.GT.0)
     +PRINT *,'FMALLO. Issuing TMS command ',COMAND(1:LC)
      I = LENTAP
      CALL FMSREQ('TMS     ',COMAND(1:LC),
     +            IRC,TAPREP,I)
 
      IF((IDEBFA.GE.0).AND.(IRC.GT.2)) THEN
         PRINT *,'FMALLO. Return code ',IRC,' from FMSREQ'
         PRINT *,TAPREP(1)
         RETURN
      ENDIF
*
*     Are there any more tapes in this library for eventual retry?
*
      IF(IRC.EQ.2) THEN
         IRETRY = 1
         RETCOM = TAPREP(I)
         JREP   = I - 1
      ELSE
         IRETRY = 0
         JREP   = I
      ENDIF
 
      IF((TAPREP(1)(1:7).NE.'Library').OR.
     +   (TAPREP(2)(1:7).NE.'-------')) THEN
         PRINT *,'FMALLO. Cannot interpret reply from FMSREQ'
         PRINT *,TAPREP(1)
         PRINT *,TAPREP(2)
         IRC = 1
         RETURN
      ENDIF
*
*     Attempt to allocate the first free tape
*     Should loop over all tapes in list until one with matching
*     characteristics is found.
*
      JVOL   = 3
   20 CONTINUE
      VID    = TAPREP(JVOL)(24:29)
      LMEDIA = LENOCC(MEDIA)
      IF(MEDIA(1:LMEDIA).NE.TAPREP(JVOL)(31:30+LMEDIA)) THEN
         IF(IDEBFA.GE.0) THEN
            PRINT *,'FMALLO. library/pool is not of type ',
     +                           MEDIA(1:LMEDIA)
            PRINT *,TAPREP(JVOL)
         ENDIF
 
         JVOL = JVOL + 1
         IF(JVOL.LE.JREP)   GOTO 20
         IF(IRETRY.NE.0) THEN
            COMAND = RETCOM
            GOTO 10
         ENDIF
         IRC = 2
         RETURN
      ENDIF
*
*     Get VSN, VID, Density and Group
*
      COMAND = ' '
      WRITE(COMAND,9002) VID
 9002 FORMAT('QUERY VID ',A,' (VSN DENSITY GROUP')
      LC = LENOCC(COMAND)
      IF(IDEBFA.GT.0)
     +PRINT *,'FMALLO. Issuing TMS command ',COMAND(1:LC)
      I = LENREP
      CALL FMSREQ('TMS     ',COMAND(1:LC),
     +            IRC,TMSREP,I)
      IF((IDEBFA.GE.0).AND.(IRC.NE.0)) THEN
         PRINT *,'FMALLO. Return code ',IRC,' from FMSREQ'
         PRINT *,TMSREP(1)
         RETURN
      ENDIF
 
      VSN   = TMSREP(1)(8:13)
*     READ(TMSREP(1)(17:22),'(I6)') IDENS
      IDENS = ICDECI(TMSREP(1),17,22)
      IF(IS(2).LE.22.AND.TMSREP(1)(IS(2):IS(2)).EQ.'K') IDENS =
     +   IDENS * 1000
      IF(IOPTG.NE.0) THEN
         IF(IDEBFA.GT.2)
     +   PRINT *,'FMALLO. Option G specified: group set to *None'
         GROUP = '*None   '
      ELSE
         GROUP = TMSREP(1)(24:31)
         IF(IDEBFA.GT.2)
     +   PRINT *,'FMALLO. Group will remain ',GROUP
      ENDIF
 
      COMAND = ' '
      WRITE(COMAND,9003) VID,POOL,GROUP
 9003 FORMAT('GETPOOL VID ',A,' FROM ',A,' NEWGROUP ',A)
      LC = LENOCC(COMAND)
      IF(IDEBFA.GT.0)
     +PRINT *,'FMALLO. Issuing TMS command ',COMAND(1:LC)
      I = LENREP
      CALL FMSREQ('TMS     ',COMAND(1:LC),
     +            IRC,TMSREP,I)
      IF((IDEBFA.GE.0).AND.(IRC.NE.0)) THEN
         PRINT *,'FMALLO. Return code ',IRC,' from FMSREQ'
         PRINT *,TMSREP(1)
         JVOL = JVOL + 1
*
*     Retry using next volume from the last batch returned
*
         IF(JVOL.LE.JREP) GOTO 20
*
*     If batch is exhausted, go and get a new batch (if available)
*
         IF(IRETRY.NE.0) THEN
            COMAND = RETCOM
            GOTO 10
         ENDIF
         RETURN
      ENDIF
 
      IF(LBANK.NE.0) THEN
         CALL UCTOH(VSN,IQ(LBANK+KOFUFA+MVSNFA),4,6)
         CALL UCTOH(VID,IQ(LBANK+KOFUFA+MVIDFA),4,6)
         IQ(LBANK+KOFUFA+MDENFA) = IDENS
         IQ(LBANK+KOFUFA+MFSQFA) = 1
         IQ(LBANK+KOFUFA+MVSQFA) = 1
 
         IQ(LBANK+KOFUFA+MMTPFA) = ICNTH(MEDIA,CHMTYP,NMTYP)
      ENDIF
#endif
      END
 
