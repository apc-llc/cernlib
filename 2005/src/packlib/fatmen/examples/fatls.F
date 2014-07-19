*
* $Id: fatls.F,v 1.1.1.1 1996/03/07 15:17:59 mclareni Exp $
*
* $Log: fatls.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:59  mclareni
* Fatmen
*
*
      PROGRAM FATLS
*----------------------------------------------------------------------*
*                                                                      *
* Example of use of the FMLS subroutine                                *
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
      CHARACTER*4 CHOPT
      CHARACTER*4 KOPT(5)
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
      CALL FMINIT(IXSTOR,LUNRZ,LUNFZ,'//CERN/NA31',IRC)
      CALL FMLOGL(0)
      kopt(3) = 'I'
      kopt(4) = 'W'
      kopt(5) = 'X'
      do 10 i=3,5
      chopt  = kopt(i)
      call fmlogl(3)
      CALL FMLS('//CERN/NA31/88/MIN8/*',CHOPT,IC)
10    continue
      END
 
