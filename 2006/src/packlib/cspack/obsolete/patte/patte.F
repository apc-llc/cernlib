*
* $Id: patte.F,v 1.1.1.1 1996/03/08 15:44:23 mclareni Exp $
*
* $Log: patte.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:23  mclareni
* Cspack
*
*
      PROGRAM PATTE
*
*     Main program of the Paw Automatic Tuple Transfer Executive
*
#include "cspack/pawc.inc"
      CHARACTER*100 CHARGS
      CHARACTER*20  CHGRP,CHPASS
#include "cspack/pzunit.inc"
#include "cspack/czsock.inc"
      EXTERNAL ZEXIT
 
      IADTCP =  0
      ISKIN  = -1
      ISKOUT = -1
*
*     C initialisation
*
#if defined(CERNLIB_IBM)
      IDUMMY = CINIT(DUMMY)
#endif
*
*     Initialise Zebra
*
      CALL HLIMIT(NWPAWC)
*
*     Initialise KUIP
*
      CALL KUINIT(30000)
      CALL KUEXIT(ZEXIT)
      CALL KUQUIT(ZEXIT)
*
*     Initialise XZ
*
      IPRINT = 6
      LOGLVL = 0
      LUNPZI = 1
      LUNPZO = 2
      CALL XZINIT(IPRINT,LOGLVL,LUNPZI,LUNPZO)
*
*     Units for FATMEN
*
      LUNPZR  = 3
      LUNPZF  = 4
*
*     Units for processing .netrc file
*
      LUNPZN = 7
      LUNPZO = 8
*
*     Check out .netrc file
*
      CALL PZNTRC(LUNPZN,CHGRP,CHPASS,'C',IRC)
*
*     Set prompt
*
      CALL KIPRMT('Patte> ')
*
*     CDF initialisation
*
      CALL PZCDF
*
*     Pass control to KUIP
*
#if !defined(CERNLIB_MOTIF)
      CALL KUWHAT
#endif
#if defined(CERNLIB_MOTIF)
      CALL KUWHAM('Patte')
#endif
 
      END
