*
* $Id: cdexam12.F,v 1.1.1.1 1996/02/28 16:23:55 mclareni Exp $
*
* $Log: cdexam12.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:55  mclareni
* Hepdb, cdlib, etc
*
*
      PROGRAM put_get_data
c
c*** practice putting/retrieving data to/from tpr directory
c
c     hr feb 94
c
c***********************************************************************
      parameter (nwpaw=1000000)
      COMMON /PAWC/NWP,IXPAWC,IHDIV,IXHIGZ,IXKU,FENC(5),LMAIN,HCV(NWPAW)
      DIMENSION IQ(2),Q(2),LQ(8000)
      EQUIVALENCE (LQ(1),LMAIN),(IQ(1),LQ(9)),(Q(1),IQ(1))
 
      parameter (nkeys=2)
      parameter (maxobj=1000)
      parameter (nodirs=13)
      parameter (nhbook = 1000)
 
      character*4 chtop
      character*80 chfile
 
      character*8 chtag(nkeys)
      character*10 chfor
      character*40 dname
 
      integer imask(14)
      integer keys(14)
c-----------------------------------------------------------------------
      character*10 test_text
 
      integer ivect(10)
 
      data ivect/ 10, 9, 8, 7, 6, 5, 3, 4, 2, 1/
 
c=======================================================================
      idiv = 0
 
      call cdpaw(nwpaw, nhbook, idiv, 'USR-DIV', 5000, 500000,
     *     'ZPHU', irc)
 
c------------------------------------------------------------------------
 
      luncd = 1
      lunfz = 2
 
      lun_names = 10
 
      do i=1,10
         write(6,*)' ivect,i',ivect(i),i
      enddo
 
 
      call cdpref( lun_names, 'AA', chtop, chfile, irc )
      if( irc .gt. 4 ) then
         print *,' EGOPEN: stop due to fatal error from CDPREF'
         stop 16
      endif
 
      write(6,*)' after cdpref, chtop=',chtop,' chfile=',chfile,
     *     ' irc=',irc
 
      lrecl = 0
      call cdopen( luncd, lunfz, chtop, chfile, lrecl, idiv, ' ', irc)
 
      write (6,*)' after cdopen, lrecl=',lrecl,' idiv=',idiv,' irc=',irc
c-----------------------------------------------------------------------
 
      dname = '//CDAA/AUX/CALGEO'
 
      call vzero(keys,14)
 
      keys(10) = 1
 
      keys(11) = 2
      keys(12) = 3
      keys(13) = 4HRUD1
      keys(14) = 4HRUD2
 
      call cdvect( ' ',ivect, 10, laddr, 'PI', irc)
 
      write(6,*)'after cdvect laddr',laddr,'irc',irc
 
      print *,(iq(laddr+i),i=1,20)
 
      iudiv=0
      call cdstor(dname, laddr,lkybk,iudiv,keys, ' ', irc)
 
      write(6,*)'after cdstor lkybk',lkybk,'irc',irc
 
      call cdend(' ','A',irc)
c-----------------------------------------------------------------------
      end
