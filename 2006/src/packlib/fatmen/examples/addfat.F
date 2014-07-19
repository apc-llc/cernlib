*
* $Id: addfat.F,v 1.1.1.1 1996/03/07 15:17:59 mclareni Exp $
*
* $Log: addfat.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:59  mclareni
* Fatmen
*
*
      PROGRAM ADDTEST
      CHARACTER*256 GENAME,DSN
      CHARACTER*80  COMM
      CHARACTER*8   HOST
      CHARACTER*4   FFORM,RECFM,CHOPT
      CHARACTER*6   VSN,VID
#include "fatmen/fatpara.inc"
      DIMENSION     MYVECT(NWDSFA),IVECT(10)
*
*     Initialise FATMEN using novice interface
*
      LUNRZ = 1
      LUNFZ = 2
      CALL FMSTRT(LUNRZ,LUNFZ,'//CERN/CNDIV',IRC)
*
*     Set maximum log level
*
      CALL FMLOGL(3)
*
*     Set location code (to be used for adding following entries)
*
      MYLOC  = 99
      NUMLOC = 1
      CALL FMSETL(MYLOC,NUMLOC,IC)
*
*     Add a tape entry
*
      GENAME = '//CERN/CNDIV/JAMIE/TEST'
      VSN    = 'I29021'
      VID    = 'I29021'
      ISEQ   = 1
      DSN    = 'MINIDST'
      FFORM  = 'FX'
      HOST   = 'ZFATAL'
      RECFM  = 'U'
      COMM   = 'ADDED VIA ADDTEST.F'
      CHOPT  = ' '
 
      CALL FMADDT(GENAME,VSN,VID,ISEQ,
     +            DSN,FFORM,0,HOST,RECFM,80,11,483,2,COMM,
     +            IVECT,CHOPT,IRC)
*
*     Add a disk entry. Here we first fill the bank and
*     then add supplementary information using FMPEEK/FMPOKE
*
      GENAME = '//CERN/CNDIV/JAMIE/TEST'
      DSN    = '<JAMIE.192>BOX.SET'
      FFORM  = 'AS'
      HOST   = 'CERNVM'
      RECFM  = 'V'
      COMM   = 'ADDED VIA ADDTEST.F'
      CHOPT  = 'N'
 
      CALL FMADDD(GENAME,DSN,FFORM,0,HOST,RECFM,80,11,483,COMM,
     +            IVECT,CHOPT,IRC)
*
*     Copy contents of bank created by last FMADDx call into MYVECT
*
      CALL FMPEEK(GENAME,MYVECT,' ',IRC)
*
*     Change some fields...
*        Set use count to 3
*        Change account of creator
*
      MYVECT(MUSCFA) = 3
      CALL UCTOH('JDS$CT',MYVECT(MCIDFA),4,6)
*
*     Add send the update
*
      CALL FMPOKE(GENAME,MYVECT,'P',IRC)
*
*     Terminate cleanly
*
      CALL FMEND(IRC)
      PRINT *,'Return code ',IRC,' from FMEND'
*
      END
