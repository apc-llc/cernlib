*
* $Id: fatmod.F,v 1.1.1.1 1996/03/07 15:17:59 mclareni Exp $
*
* $Log: fatmod.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:59  mclareni
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
      PARAMETER       (LKEYFA=10)
      DIMENSION KEY(LKEYFA)
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
      PRINT *,'  now FMGET'
      CALL FMGET('//CERN/CNDIV/JAMIE/IN',LBANK,KEYS,IRC)
      PRINT *,'  now FMPUTC,  FMGET RC=',IRC
      CALL FMPUTI(LBANK,133,MUSWFA,IRC)
      CALL FMPUTI(LBANK,256,MUSWFA+5,IRC)
      CALL FMPUTI(LBANK,3,MUSWFA+7,IRC)
      CALL FMPUTI(LBANK,13,MUSWFA+9,IRC)
      PRINT *,'  now FMSHOW,  FMPUT RC=',IRC
      CALL FMSHOW('//CERN/CNDIV/JAMIE/IN',LBANK,
     +KEYS,'A',IRC)
      PRINT *,'  now FMMOD ,  FMSHOW RC=',IRC
      CALL FMMOD('//CERN/CNDIV/JAMIE/IN',LBANK,0,IRC)
      PRINT *,'      FMMOD  RC=',IRC
      END
