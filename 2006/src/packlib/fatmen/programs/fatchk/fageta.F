*
* $Id: fageta.F,v 1.1.1.1 1996/03/07 15:17:37 mclareni Exp $
*
* $Log: fageta.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:37  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FAGETA(CHPATH,NFILES,NDIRS)
*
#include "zebra/rzcl.inc"
#include "zebra/rzdir.inc"
#include "zebra/rzch.inc"
#include "zebra/rzk.inc"
      CHARACTER *(*) CHPATH
      DIMENSION ISD(NLPATM),NSD(NLPATM),IHDIR(4)
#include "fatmen/fatusr.inc"
#include "fatmen/fatbug.inc"
#include "fatmen/fmnkeys.inc"
      DIMENSION KEYS(LKEYFA)
*
*-----------------------------------------------------------------------
*
      IQUEST(1) = 0
      NFILES    = 0
      NDIRS     = 0
      IF(LQRS.EQ.0)GO TO 60
*
*            General case
*
      IF(LCDIR.EQ.0)GO TO 60
      CALL RZCDIR(CHWOLD,'R')
      CALL RZCDIR(CHPATH,' ')
      IF(IQUEST(1).NE.0) GOTO 60
      CALL RZPAFF(CHPAT,NLPAT,CHL)
      NLPAT0=NLPAT
      ITIME=0
*
*
*            Set CWD to the current level
*
   10 CONTINUE
      NDIRS = NDIRS + 1
      IF(ITIME.NE.0)THEN
         CALL RZPAFF(CHPAT,NLPAT,CHL)
         IF(IQUEST(1).NE.0)THEN
            NLPAT=NLPAT-1
            GO TO 40
         ENDIF
         CALL RZCDIR(CHL,' ')
      ENDIF
      IF(IQUEST(1).NE.0)THEN
         NLPAT=NLPAT-1
         GO TO 40
      ENDIF
      ISD(NLPAT)=0
      NSD(NLPAT)=IQ(KQSP+LCDIR+KNSD)
*
*     Get all objects in current directory
*
      LK = IQ(KQSP+LCDIR+KLK)
      NK = IQ(KQSP+LCDIR+KNKEYS)
      NWK= IQ(KQSP+LCDIR+KNWKEY)
*
*     No objects...
*
      IF(NK.EQ.0) GOTO 40
      IF(IDEBFA.GE.1) CALL RZCDIR(' ','P')
      DO 30 I=1,NK
         K=LK+(NWK+1)*(I-1)
         DO 20 J=1,NWK
            IKDES=(J-1)/10
            IKBIT1=3*J-30*IKDES-2
            IF(JBYT(IQ(KQSP+LCDIR+KKDES+IKDES),IKBIT1,3).LT.3)THEN
               KEYS(J)=IQ(KQSP+LCDIR+K+J)
            ELSE
               CALL ZITOH(IQ(KQSP+LCDIR+K+J),KEYS(J),1)
            ENDIF
   20    CONTINUE
*
*     Try to get this object
*
         ICYCLE = 9999
         JBIAS = 2
         CALL RZIN(IXDIV,LADDBK,JBIAS,KEYS,ICYCLE,' ')
         IF(IQUEST(1).EQ.0) THEN
            NFILES = NFILES + 1
            IF(IDEBFA.GE.1) CALL FMPKEY(KEYS,LKEYFA)
            CALL MZDROP(IXDIV,LADDBK,' ')
            LADDBK = 0
         ENDIF
   30 CONTINUE
 
*
*     Process possible down directories
*
   40 ISD(NLPAT)=ISD(NLPAT)+1
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
            GO TO 40
         ENDIF
      ENDIF
*
*            Reset CWD
*
   50 CALL RZCDIR(CHWOLD,' ')
*
   60 RETURN
      END
