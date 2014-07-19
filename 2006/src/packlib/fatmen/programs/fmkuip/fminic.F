*
* $Id: fminic.F,v 1.1.1.1 1996/03/07 15:17:43 mclareni Exp $
*
* $Log: fminic.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:43  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMINIC
#include "fatmen/fmpath.inc"
#include "fatmen/fatbug.inc"
      PARAMETER (LURCOR=200000)
      COMMON/CRZT/IXSTOR,IXDIV,IFENCE(2),LEV,LEVIN,BLVECT(LURCOR)
      DIMENSION    LQ(999),IQ(999),Q(999)
      EQUIVALENCE (IQ(1),Q(1),LQ(9)),(LQ(1),LEV)
#include "fatmen/fatusr.inc"
#include "fatmen/fatsys.inc"
#include "fatmen/fstate.inc"
*
      COMMON /QUEST/IQUEST(100)
*
      PARAMETER    (LKEYFA=10)
      DIMENSION    KEYS(LKEYFA)
      CHARACTER*8  CHTAG(10),RTIME
      CHARACTER*80 LINE
      DIMENSION    LUNFAT(3)
#include "fatmen/fatron.inc"
      SAVE         NENTRY
      DATA NENTRY/0/
 
      IF((MFPHAS.NE.3).AND.(NENTRY.EQ.1)) THEN
         PRINT *,'FMINIC. Error - FATMEN has already been initialised'
         RETURN
      ENDIF
 
      IF(NENTRY.EQ.0) THEN
*
*     Initialise ZEBRA
*
         CALL MZSTOR(IXSTOR,'/CRZT/','Q',IFENCE,LEV,BLVECT(1),BLVECT(1),
     +               BLVECT(5000),BLVECT(LURCOR))
         CALL MZLOGL(IXSTOR,-3)
*
* *** Define user division and link area like:
*
         CALL MZDIV  (IXSTOR, IXDIV, 'USERS', 50000, LURCOR, 'L')
         CALL MZLINK (IXSTOR, '/USRLNK/', LUSRK1, LUSRLS, LUSRK1)
 
      ENDIF
*
*     Unit for RZ database
*
      LUNRZ  = 1
      LUNFZ  = 2
      LPRTFA = 6
 
*         read mode
      LTOP   = LENOCC(TOPDIR)
      CALL FMINIT(IXSTOR,LUNRZ,LUNFZ,
     +            TOPDIR(1:LTOP)//'/'//THRONG(1:LTHR),IRC)
*
*     Permit retry if GIME failed / catalogue not found
*
      IF((IRC.EQ.104).OR.(IRC.EQ.28)) THEN
         PRINT *
         PRINT *,'FMINIC. Please retry with a valid FATMEN group'
         PRINT *,'        This is typically the name of your experiment'
         PRINT *,'           e.g. INIT L3'
         PRINT *
         NENTRY = -1
         THRONG = 'NONE'
         LTHR   = 4
         RETURN
      ELSE
         NENTRY = 1
      ENDIF
*
*     Set logical units
*
      LUNFAT(1) = 8
      LUNFAT(2) = 9
      LUNFAT(3) = 4
      CALL FMSETU(LUNFAT,3,IRC)
 
#if defined(CERNLIB_CERNVM)
      CALL FMONIT('Init FATMEN shell')
#endif
      CALL FMLOGL(999)
#if !defined(CERNLIB_CZ)
      CALL RZCDIR(TOPDIR(1:LTOP)//'/'//THRONG(1:LTHR),' ')
      CALL RZCDIR(TOPDIR(1:LTOP)//'/'//THRONG(1:LTHR),'P')
#endif
#if defined(CERNLIB_CZ)
      CALL CZPUTA('MESS :CD '//TOPDIR(1:LTOP)//'/'
     +                       //THRONG(1:LTHR),ISTAT)
1     CONTINUE
      CALL CZGETA(LINE,ISTAT)
      PRINT *,LINE(3:80)
      IF (LINE(1:1) .EQ. '2') GOTO 1
#endif
      CDIR = TOPDIR(1:LTOP)//'/'//THRONG(1:LTHR)
      LCDIR = LENOCC(CDIR)
      CALL KIPRMT('FM>')
 
      END
 
