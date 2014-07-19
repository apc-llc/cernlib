*
* $Id: hinpf.F,v 1.1.1.1 1996/01/16 17:07:40 mclareni Exp $
*
* $Log: hinpf.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:40  mclareni
* First import
*
*
#include "hbook/pilot.h"
#if defined(CERNLIB_CZ)
*CMZ :  4.21/01 27/10/93  17.06.00  by  Fons Rademakers
*-- Author :    Alfred Nathaniel   13/04/93
      SUBROUTINE HINPF(IDH,IREPL)
*
* Receive a histogram
*
* IREPL.LT.0 : print warning if IDH already exists
* IREPL.EQ.0 : replace without warning
* IREPL.GT.0 : add histograms if IDH already exists
*
#include "hbook/hcflag.inc"
#include "hbook/hcbook.inc"
#include "hbook/hcpiaf.inc"
*
      COMMON/QUEST/IQUEST(100)
 
*
* Check if IDH already in the table
*
      IDD=IDH
      NRHIST=IQ(LCDIR+KNRH)
      IDPOS=LOCATI(IQ(LTAB+1),NRHIST,IDD)
      IF(IDPOS.GT.0) THEN
         IF(IREPL.LT.0) THEN
            CALL HBUG('Already existing histogram replaced','HINPF',IDD)
         ENDIF
         IF(IREPL.LE.0) THEN
            CALL HDELET(IDD)
            NRHIST=IQ(LCDIR+KNRH)
         ELSE
*--- allocate a new IDD for adding
            IDD=IQ(LTAB+NRHIST)+1
            IDPOS=NRHIST+1
         ENDIF
         IDPOS=-IDPOS+1
      ENDIF
*
* Enter IDD in the list of ordered IDs
*
      IDPOS=-IDPOS+1
      IF(NRHIST.GE.IQ(LTAB-1)) THEN
         CALL MZPUSH(IHDIV,LTAB,500,500,' ')
      ENDIF
      DO 10 I=NRHIST,IDPOS,-1
         IQ(LTAB+I+1)=IQ(LTAB+I)
         LQ(LTAB-I-1)=LQ(LTAB-I)
 10   CONTINUE
*
* Import histogram data structure
*
      NUH=0
      IF(LIDS.EQ.0)THEN
         CALL FZIN(999,IHDIV,LCDIR,-2,' ',NUH,0)
         IF(IQUEST(1).NE.0) GOTO 99
         LIDS=LQ(LCDIR-2)
         LCID=LIDS
      ELSE
         LLID=LQ(LCDIR-9)
         CALL FZIN(999,IHDIV,LLID,0,' ',NUH,0)
         IF(IQUEST(1).NE.0) GOTO 99
         LCID=LQ(LLID)
      ENDIF
      IQ(LCID-5)=IDD
      LQ(LCDIR-9)=LCID
      IQ(LCDIR+KNRH)=IQ(LCDIR+KNRH)+1
      IQ(LTAB+IDPOS)=IDD
      LQ(LTAB-IDPOS)=LCID
      CALL SBIT0(IQ(LCID),5)
*
* Add histograms
*
      IF(IDD.NE.IDH) THEN
         CALL HOPERA(IDH,'+',IDD,IDH,1.,1.)
         CALL HDELET(IDD)
*
* Existing histogram was updated so clear bit 6 of status word
*
         NRHIST=IQ(LCDIR+KNRH)
         IDPOS=LOCATI(IQ(LTAB+1),NRHIST,IDH)
         CALL SBIT0(IQ(LQ(LTAB-IDPOS)),6)
      ENDIF
*
* On slave servers histograms should always be cleared to prevent multiple
* counting when the partial histograms are added by the master server
*
      IF (SLAVPF) THEN
         CALL HRESET(IDH,' ')
      ENDIF
 
      RETURN
 
 99   CONTINUE
      CALL HBUG('Bad sequence for FZ','HINPF',IDD)
      END
#endif
