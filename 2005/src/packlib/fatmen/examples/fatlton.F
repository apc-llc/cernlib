*
* $Id: fatlton.F,v 1.1.1.1 1996/03/07 15:18:01 mclareni Exp $
*
* $Log: fatlton.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:01  mclareni
* Fatmen
*
*
      PROGRAM FATLTON
*----------------------------------------------------------------------*
*                                                                      *
* Test FMLTON                                                          *
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
      PARAMETER   (LKEYFA=10)
      DIMENSION KEYS(LKEYFA)
      PARAMETER   (MAXNOD=3)
      CHARACTER*8 CHNODE(MAXNOD)
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
 
      LOCCOD = 33
      CALL FMLTON(LOCCOD,MAXNOD,CHNODE,NNODES,' ',IRC)
      PRINT 9001,LOCCOD,NNODES,IRC
9001  FORMAT(' FATNTOL. location code: ',I5,' NNODES: ',I5,
     +       ' IRC = ',I10)
      PRINT *,(CHNODE(I),I=1,MIN(MAXNOD,NNODES))
      END
