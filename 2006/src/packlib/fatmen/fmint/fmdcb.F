*
* $Id: fmdcb.F,v 1.1.1.1 1996/03/07 15:18:11 mclareni Exp $
*
* $Log: fmdcb.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:11  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMDCB(GENAME,LBANK,DSN,RECFM,LRECL,BLOCK,SPACE,
     +                 CHOPT,IRC)
*CMZ :          21/01/91  16.46.13  by  Jamie Shiers
*-- Author :    Jamie Shiers   21/01/91
*
*     Set DCB parameters for GENAME.
*     DCB parameters may be given as parameters, or should
*     be taken from the model DCB specified by DSN
*
      CHARACTER*(*) GENAME,DSN,RECFM
      INTEGER       LBANK,LRECL,BLOCK,SPACE(3),IRC
#if defined(CERNLIB_IBMMVS)
#include "fatmen/fatdcb.inc"
#endif
#include "fatmen/fatbank.inc"
#include "fatmen/fatpara.inc"
*
*     CHOPT: B - space is given in BLOCKS
*            C - space is given in CYLS
*            T - space is given in TRKS
*
#include "fatmen/fatopts.inc"
      LDSN = LENOCC(DSN)
      IF(LDSN.NE.0) THEN
#if defined(CERNLIB_IBMMVS)
*
*     Connect catalogue entry containing model DCB to LUFZFA
*
         MODE = 0
         CALL FTDD(LUFZFA,MODE,DSN(1:LDSN),IC)
*
*     Obtain DCB information
*
         CALL FTINFO(LUFZFA,TYPE,HDSN,DISP,VOL,LABEL,IDCB,UNIT,ISPACE,
     +   IUSED,IC)
         IQ(LBANK+KOFUFA+MRFMFA) = IDCB(1)
         IQ(LBANK+KOFUFA+MRLNFA) = IDCB(2)
         IQ(LBANK+KOFUFA+MBLNFA) = IDCB(3)
#endif
      ELSE
*
*     Use DCB parameters supplied in argument list
*
         IQ(LBANK+KOFUFA+MRLNFA) = LRECL
         IQ(LBANK+KOFUFA+MBLNFA) = BLOCK
         CALL UCTOH(RECFM,IQ(LBANK+KOFUFA+MRFMFA),4,4)
      ENDIF
#if defined(CERNLIB_IBMMVS)
*
*     SPACE only meaningful on IBM/MVS systems
*
      IF(IOPTB.NE.0) THEN
         CALL UCTOH('BLK ',ISPACE(1),4,4)
         CHSPAC = 'BLK '
      ELSEIF(IOPTC.NE.0) THEN
         CALL UCTOH('CYL ',ISPACE(1),4,4)
         CHSPAC = 'CYL '
      ELSEIF(IOPTT.NE.0) THEN
         CALL UCTOH('TRK ',ISPACE(1),4,4)
         CHSPAC = 'TRK '
      ELSE
         CALL UCTOH('BLK ',ISPACE(1),4,4)
         CHSPAC = 'BLK '
      ENDIF
      CALL UCOPY(SPACE,ISPACE(2),3)
#endif
      END
