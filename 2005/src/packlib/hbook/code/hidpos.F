*
* $Id: hidpos.F,v 1.1.1.1 1996/01/16 17:07:39 mclareni Exp $
*
* $Log: hidpos.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:39  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.21/01 01/11/93  18.24.14  by  Fons Rademakers
*-- Author :    Alfred Nathaniel   13/04/93
      SUBROUTINE HIDPOS(IDH,IDPOS)
*
* locate the position of histogram IDH in LTAB
*
*     CALL HIDPOS(IDH,IDPOS)
* is the replacement for
*     IDPOS=LOCATI(IQ(LTAB+1),IQ(LCDIR+KNRH),IDH)
*
* The Piaf server has to import the histogram from the client in case it
* does not exist in the local /PAWC/.
*
#include "hbook/hcbook.inc"
#include "hbook/hcpiaf.inc"
#if defined(CERNLIB_CZ)
#include "hbook/hcache.inc"
#endif
*
      IDPOS = LOCATI(IQ(LTAB+1),IQ(LCDIR+KNRH),IDH)
*
#if defined(CERNLIB_CZ)
      IF (IDPOS.LE.0 .AND. SERVPF) THEN
*
*-- Check if IDH is in the cache of not existing histograms so we
*-- don't have to ask the master server or the client over-and-over
*-- if the histogram exists
*
         IDNPOS = LOCATI(IDNOT,IDTNOT,IDH)
         IF (IDNPOS .GT. 0) THEN
            IDPOS = 0
            RETURN
         ENDIF
*
         WRITE(CHSMPF,'(A,I12)') 'HEXIST',IDH
         CALL CZPUTA(CHSMPF,ISTAT)
         CALL CZGETA(CHSMPF,ISTAT)
         IF (CHSMPF.EQ.'YES') THEN
            WRITE(CHSMPF,'(A,I12)') 'HOUTPF',IDH
            CALL CZPUTA(CHSMPF,ISTAT)
            CALL HINPF(IDH,0)
            IDPOS = LOCATI(IQ(LTAB+1),IQ(LCDIR+KNRH),IDH)
         ELSE
*
*-- Fill the cache of not existing histograms.
*-- This cache is reset by the command 'histo/delete 0', issued before
*-- executing an Ntuple command.
*
            IF (IDTNOT .LT. IDMXNT) THEN
               IDNPOS = -IDNPOS + 1
               DO 10 I = IDTNOT, IDNPOS, -1
                  IDNOT(I+1) = IDNOT(I)
   10          CONTINUE
               IDNOT(IDNPOS) = IDH
               IDTNOT = IDTNOT + 1
            ENDIF
            IF (MASTPF) THEN
               PRINT *, ' *** Piaf: Unknown histogram, ID=', IDH
            ENDIF
         ENDIF
      ENDIF
#endif
      END
