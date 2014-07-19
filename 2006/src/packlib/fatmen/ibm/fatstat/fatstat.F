*
* $Id: fatstat.F,v 1.1.1.1 1996/03/07 15:17:36 mclareni Exp $
*
* $Log: fatstat.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:36  mclareni
* Fatmen
*
*
#if defined(CERNLIB_IBMVM)
#include "sys/CERNLIB_machine.h"
#include "_fatmen/pilot.h"
      PROGRAM FATSTAT
*----------------------------------------------------------------------*
*                                                                      *
* Test FATMEN program. Count number of directories, files and          *
* quota in FATMEN RZ file 'CERN FATRZ'                                 *
*                                                                      *
*----------------------------------------------------------------------*
      PARAMETER (LURCOR=200000)
      COMMON/CRZT/IXSTOR,IXDIV,IFENCE(2),LEV,LEVIN,BLVECT(LURCOR)
      DIMENSION    LQ(999),IQ(999),Q(999)
      EQUIVALENCE (IQ(1),Q(1),LQ(9)),(LQ(1),LEV)
*
      COMMON /USRLNK/LUSRK1,LUSRBK,LUSRLS
*
      COMMON /QUEST/IQUEST(100)
*
      PARAMETER       (LKEYFA=10)
      DIMENSION KEY(LKEYFA)
      CHARACTER*8   THRONG
*
*
*     Initialise ZEBRA
*
      CALL MZEBRA(-3)
      CALL MZSTOR(IXSTOR,'/CRZT/','Q',IFENCE,LEV,BLVECT(1),BLVECT(1),
     +            BLVECT(5000),BLVECT(LURCOR))
      CALL MZLOGL(IXSTOR,-3)
 
*
* *** Define user division and link area like:
*
      CALL MZDIV  (IXSTOR, IXDIV, 'USERS', 50000, LURCOR, 'L')
      CALL MZLINK (IXSTOR, '/USRLNK/', LUSRK1, LUSRLS, LUSRK1)
*
*     Units for FATMEN RZ/FZ files
*
      LUNRZ = 1
      LUNFZ = 2
*
*     Get name of throng from REXX variable THRONG
*
      CALL VMREXX('F','THRONG',THRONG,IRC)
*
*     Initialise FATMEN
*
      CALL FMINIT(IXSTOR,LUNRZ,LUNFZ,
     +'//CERN/'//THRONG(1:LENOCC(THRONG)),IRC)
*
*     Set maximum log level
*
      CALL FMLOGL(3)
*
*     Call routine to process RZ file
*
      CALL FATSIZ
*
*     Call routine to count number of zero/non-zero blocks
*
      CALL FATCNT(LUNRZ)
*
*     Terminate cleanly
*
      CALL FMEND(IRC)
 
      END
 
#endif
