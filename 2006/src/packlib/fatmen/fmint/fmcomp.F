*
* $Id: fmcomp.F,v 1.1.1.1 1996/03/07 15:18:10 mclareni Exp $
*
* $Log: fmcomp.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:10  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMCOMP(GENAM1,LBANK1,KEYS1,GENAM2,LBANK2,KEYS2,IRC)
*
*     Subroutine to compare the FATMEN banks pointed to by
*     input generic names/keys/bank address
*     If LBANKn not zero, GENAMn is not used.
*     Otherwise the bank is fetched from the RZ file using FMGETK
*
#include "fatmen/fatpara.inc"
#include "fatmen/fatbank.inc"
      CHARACTER*(*) GENAM1,GENAM2
      PARAMETER (LKEYFA=10)
      DIMENSION KEYS1(LKEYFA),KEYS2(LKEYFA)
 
      IRC = 0
 
      IF(LBANK1.EQ.0) THEN
         IF(IDEBFA.GE.0) PRINT *,'FMCOMP. get bank for GENAM1'
         CALL FMGETK(GENAM1,LBANK1,KEYS1,IRC)
         IF(IRC.NE.0) THEN
            IF(IDEBFA.GE.0)
     +      PRINT *,'FMCOMP. Return code ',IRC,' from FMGETK'
            IRC = 1
            RETURN
         ENDIF
      ELSE
         IF(IDEBFA.GE.0)
     +      PRINT *,'FMCOMP. user supplied bank for GENAM1'
      ENDIF
 
      IF(LBANK2.EQ.0) THEN
         IF(IDEBFA.GE.0) PRINT *,'FMCOMP. get bank for GENAM2'
         CALL FMGETK(GENAM2,LBANK2,KEYS2,IRC)
         IF(IRC.NE.0) THEN
            IF(IDEBFA.GE.0)
     +      PRINT *,'FMCOMP. Return code ',IRC,' from FMGETK'
            IRC = 1
            RETURN
         ENDIF
      ELSE
         IF(IDEBFA.GE.0)
     +      PRINT *,'FMCOMP. user supplied bank for GENAM2'
      ENDIF
 
 
      IF(KEYS1(MKMTFA).NE.KEYS2(MKMTFA)) THEN
         IF(IDEBFA.GE.2) PRINT *,'FMCOMP. conflicting media types:',
     +                           KEYS1(MKMTFA),KEYS2(MKMTFA)
         IRC = 3
         RETURN
      ENDIF
 
      IF(KEYS1(MKMTFA) .EQ. 1) THEN
*
*     Disk data set, match MFQNFA, MHSNFA
*
         DO 10 IOFF=MFQNFA+KOFUFA,MCPLFA+KOFUFA
            IF(IQ(LBANK1+IOFF).NE.IQ(LBANK2+IOFF)) THEN
               IF(IDEBFA.GE.2) PRINT 8001,IOFF-KOFUFA, IQ(LBANK1+IOFF),
     +         IQ(LBANK2+IOFF)
 8001 FORMAT(' FMCOMP. Comparison failed at ',I5,1X,
     +       A4,1X,A4,' (old/new)')
               IRC = 4
               RETURN
            ENDIF
 
   10    CONTINUE
 
      ELSE
*
*     Tape data set, match MVSNFA, MVIDFA, MFSQFA
*
         DO 20 IOFF=MVSNFA+KOFUFA,MVIPFA-1+KOFUFA
            IF(IQ(LBANK1+IOFF).NE.IQ(LBANK2+IOFF)) THEN
               IF(IDEBFA.GE.2) PRINT 8001,IOFF-KOFUFA, IQ(LBANK1+IOFF),
     +         IQ(LBANK2+IOFF)
               IRC = 5
               RETURN
            ENDIF
 
   20    CONTINUE
 
         IF(IQ(LBANK1+MFSQFA+KOFUFA).NE. IQ(LBANK2+MFSQFA+KOFUFA))
     +   THEN
            IF(IDEBFA.GE.2) PRINT 8002,MFSQFA, IQ(LBANK1+MFSQFA+KOFUFA)
     +      ,IQ(LBANK2+MFSQFA+KOFUFA)
 8002 FORMAT(' FMCOMP. Comparison failed at ',I5,I4,1X,I4)
            IRC = 5
            RETURN
         ENDIF
 
      ENDIF
*
*     Found a matching bank
*
      IF(IDEBFA.GE.2) PRINT *,'FMCOMP. match succeeded '
      END
