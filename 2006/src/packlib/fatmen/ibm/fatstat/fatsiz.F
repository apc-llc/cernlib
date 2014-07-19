*
* $Id: fatsiz.F,v 1.1.1.1 1996/03/07 15:17:36 mclareni Exp $
*
* $Log: fatsiz.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:36  mclareni
* Fatmen
*
*
#if defined(CERNLIB_IBMVM)
#include "sys/CERNLIB_machine.h"
#include "_fatmen/pilot.h"
      SUBROUTINE FATSIZ
#include "fatpara.inc"
      PARAMETER (LURCOR=200000)
      COMMON/CRZT/IXSTOR,IXDIV,IFENCE(2),LEV,LEVIN,BLVECT(LURCOR)
      DIMENSION    LQ(999),IQ(999),Q(999)
      EQUIVALENCE (IQ(1),Q(1),LQ(9)),(LQ(1),LEV)
*
      COMMON /USRLNK/LUSRK1,LUSRBK,LUSRLS
*
      COMMON /QUEST/IQUEST(100)
      PARAMETER (MAXKEY=10000)
      PARAMETER (MAXFIL=10000)
      PARAMETER (MAXLEV=100)
      PARAMETER (LKEYFA=10)
      DIMENSION KEYS(LKEYFA,MAXKEY)
      CHARACTER*20  FILES(MAXKEY)
      CHARACTER*255  SUBDIR(MAXFIL)
      DATA NTOTAL/0/,NEMPTY/0/
*
*     Find names of subdirectories in this RZ file...
*
      CALL FMTREE('//CERN',SUBDIR,MAXLEV,NFOUND,MAXFIL,IRC)
      IF(IRC.NE.0) THEN
         PRINT 9001,IRC
         RETURN
         ELSE
         PRINT 9002,NFOUND
         ENDIF
*
*     Now process each subdirectory in turn
*
      DO 10 I=1,NFOUND
*
*     Find number of files in current directory
*
      LSUB = LENOCC(SUBDIR(I))
      IF(IDEBFA.GT.2) PRINT 9003,SUBDIR(I)(1:LSUB)
 
      CALL FMFNMS(SUBDIR(I)(1:LSUB),FILES,KEYS,NKEYS,MAXKEY,IRC)
      IF(IRC.NE.0) THEN
         PRINT 9004,IRC
         RETURN
         ENDIF
 
      IF(IDEBFA.GT.2) PRINT 9005,NKEYS,SUBDIR(I)(1:LSUB)
 
      IF(NKEYS.EQ.0) NEMPTY = NEMPTY + 1
      NTOTAL = NTOTAL + NKEYS
 
10    CONTINUE
 
      PRINT 9006,NTOTAL,NFOUND,NEMPTY
 
9001  FORMAT(' FATSIZ. Return code ',I5,' from FMTREE')
9002  FORMAT(' FATSIZ. ',I5,' subdirectories found')
9003  FORMAT(' FATSIZ. Processing directory ',A)
9004  FORMAT(' FATSIZ. Return code ',I5,' from FMFNMS')
9005  FORMAT(' FATSIZ. ',I5,' files in ',A)
9006  FORMAT(' FATSIZ. ',I5,' files in ',I5,' subdirectories of which ',
     +       I5,' are empty')
 
      END
 
#endif
