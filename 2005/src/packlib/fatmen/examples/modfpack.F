*
* $Id: modfpack.F,v 1.1.1.1 1996/03/07 15:18:00 mclareni Exp $
*
* $Log: modfpack.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:00  mclareni
* Fatmen
*
*
      PROGRAM FATMOD
      PARAMETER (LURCOR=200000)
      COMMON/CRZT/IXSTOR,IXDIV,IFENCE(2),LEV,LEVIN,BLVECT(LURCOR)
      DIMENSION    LQ(999),IQ(999),Q(999)
      EQUIVALENCE (IQ(1),Q(1),LQ(9)),(LQ(1),LEV)
#include "fatmen/fatpara.inc"
*
      COMMON /USRLNK/LUSRK1,LUSRBK,LUSRLS
*
      COMMON /QUEST/IQUEST(100)
*
      PARAMETER (LKEYFA=10)
      DIMENSION KEY(LKEYFA)
      PARAMETER (MAXCOP=1)
      CHARACTER*255 CHFILE
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
*     Unit for RZ database
*
      LUNRZ = 1
      LUNFZ = 2
 
*         read mode
      CALL FMINIT(IXSTOR,LUNRZ,LUNFZ,'//CERN/CNDIV',IRC)
      CALL FMLOGL(3)
      ICONT = 0
      LBANK = 0
      CALL FMFILS('//CERN/CNDIV/PAVEL/FPACK/TEST1',CHFILE,
     +            KEYS,NCOPY,MAXCOP,ICONT,IRC)
      CALL FMGETK('//CERN/CNDIV/PAVEL/FPACK/TEST1',LBANK,KEYS,IRC)
      CALL FMPUTI(LBANK,5850,MRLNFA,IRC)
      CALL FMPUTI(LBANK,5850,MBLNFA,IRC)
*     Change user name of creator
      CALL FMPUTC(LBANK,'BINKO',MCURFA,5,IRC)
*     Put CERN account
      CALL FMPUTC(LBANK,'BPL$CP',MCIDFA,6,IRC)
*     Change jobname
      CALL FMPUTC(LBANK,'BATCH_32',MCJIFA,8,IRC)
      CALL FMSHOW('//CERN/CNDIV/PAVEL/FPACK/TEST1',LBANK,
     +KEYS,'A',IRC)
      PRINT *,'  now FMMOD !!! with a NEW name !!!,  FMSHOW RC=',IRC
      CALL FMMOD('//CERN/CNDIV/PAVEL/FPACK/TEST2',LBANK,0,IRC)
      PRINT *,'      FMMOD  RC=',IRC
      END
