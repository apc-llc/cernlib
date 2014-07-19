*
* $Id: cdexam13.F,v 1.1.1.1 1996/02/28 16:23:56 mclareni Exp $
*
* $Log: cdexam13.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:56  mclareni
* Hepdb, cdlib, etc
*
*
c
c*** practice retrieving data from tpr directory
c
c     hr feb 94
c
c***********************************************************************
      PARAMETER (LURCOR=200000)
      COMMON/CRZT/IXSTOR,IXDIV,IFENCE(2),LEV,LEVIN,BLVECT(LURCOR)
      DIMENSION    LQ(999),IQ(999),Q(999)
      EQUIVALENCE (IQ(1),Q(1),LQ(9)),(LQ(1),LEV)
      COMMON /ZUNIT/ IQREAD,IQPRNT,IQPR2,IQLOG,IQPNCH,IQTTIN,IQTYPE
      COMMON/MYLINK/LKEY
 
      CHARACTER*4 CHTOP
      CHARACTER*80 CHFILE,CHPATH,DNAME
      INTEGER IVECT(10)
*
*     Initialise ZEBRA
*
      CALL MZEBRA(0)
      CALL MZSTOR(IXSTOR,'/CRZT/',' ',IFENCE,LEV,BLVECT(1),BLVECT(1),
     +            BLVECT(5000),BLVECT(LURCOR))
      CALL MZLOGL(IXSTOR,3)
 
      CALL MZLINK(IXSTOR,'MYLINK',LKEY,LKEY,LKEY)
      IQPRNT = 6
 
      NWP = NWPAW
*
*     Initialise HBOOK
*
      CALL HLIMIT(-NWP)
*
*     Initialise HEPDB
*
      CALL CDPAW(NWPAW,NHBOOK,IDIV,'USR-DIV',5000,50000,'U',IRC)
 
      WRITE(6,*)'irc cdpaw',irc
 
      LUNCD=1
      LUNFZ=2
      CALL CDPREF(10,'AA',chtop,chfile,irc)
 
      WRITE(6,*)'cdpref:chtop,chfile,irc',chtop,chfile,irc
 
      lrecl=0
 
      call cdopen( luncd, lunfz, chtop, chfile, lrecl, idiv, ' ', irc)
 
      write (6,*)' after cdopen, lrecl=',lrecl,' idiv=',idiv,' irc=',irc
c-----------------------------------------------------------------------
 
      irun=2
      lkey=0
      CALL CDUSE('//CDAA/AUX/CALGEO',lkey,IRUN,'F',IRC)
 
      write(6,*)'cd use:  IRC,LQ(lkey-1)',lkey,IRC,LQ(lkey-1)
 
      ldata = lq(lkey-1)
      print *,(lq(ldata+i),i=1,20)
 
      lvect=10
      CALL CDVECT('//CDAA/AUX/CALGEO',ivect,lvect,LQ(lkey-1),'GI',irc)
 
      print *,(ivect(i),i=1,lvect)
 
      write(6,*)'cdvect: irc,ivect(1)',irc,ivect(1)
 
      CALL CDEND(' ','A',IRC)
 
      write(6,*)'cdend:irc',irc
 
      stop
      end
