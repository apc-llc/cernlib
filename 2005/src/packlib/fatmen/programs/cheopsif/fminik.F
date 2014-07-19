*
* $Id: fminik.F,v 1.1.1.1 1996/03/07 15:17:36 mclareni Exp $
*
* $Log: fminik.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:36  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMINIK(IUDIV,LUNRZ,LUNFZ,CHPATH,IRC)
      CHARACTER*(*)     CHPATH
#include "fatmen/fmpath.inc"
#include "fatmen/fatbug.inc"
      PARAMETER (LURCOR=200000)
      COMMON/CRZT/IXSTOR,IXDIV,IFENCE(2),LEV,LEVIN,BLVECT(LURCOR)
      DIMENSION    LQ(999),IQ(999),Q(999)
      EQUIVALENCE (IQ(1),Q(1),LQ(9)),(LQ(1),LEV)
#include "fatmen/fatusr.inc"
#include "fatmen/fatsys.inc"
#include "fatmen/fstate.inc"
#include "fatmen/fatron.inc"
      SAVE         NENTRY
      DATA NENTRY/0/
 
      IF((MFPHAS.NE.3).AND.(NENTRY.EQ.1)) THEN
         PRINT *,'FMINIK. Error - FATMEN has already been initialised'
         RETURN
      ENDIF
 
      IF(NENTRY.EQ.0) THEN
*
*     Initialise ZEBRA
*
         CALL MZEBRA(-3)
         CALL MZSTOR(IXSTOR,'/CRZT/','Q',IFENCE,LEV,BLVECT(1),BLVECT(1),
     +               BLVECT(5000),BLVECT(LURCOR))
         CALL MZLOGL(IXSTOR,-3)
*
* *** Define user division and link area like:
*
         CALL MZDIV  (IXSTOR, IXDIV, 'USERS', 50000, LURCOR, 'L')
         CALL MZLINK (IXSTOR, '/USRLNK/', LUSRK1, LUSRLS, LUSRK1)
 
      ENDIF
      LPRTFA = 6
 
*      read mode
      LPATH  = LENOCC(CHPATH)
      CALL FMINIT(IXSTOR,LUNRZ,LUNFZ,CHPATH(1:LPATH),IRC)
*
*     Permit retry if GIME failed / catalogue not found
*
      IF((IRC.EQ.104).OR.(IRC.EQ.28)) THEN
         NENTRY = -1
         THRONG = 'NONE'
         LTHR   = 4
         RETURN
      ELSE
         NENTRY = 1
      ENDIF
 
      END
 
