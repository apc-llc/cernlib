*
* $Id: fatnick.F,v 1.1.1.1 1996/03/07 15:18:01 mclareni Exp $
*
* $Log: fatnick.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:01  mclareni
* Fatmen
*
*
      character*80 chname,chdesc
 
      call fmnick(1,'fmdelphi.names','RAWD91',chname,chdesc,' ',irc)
 
      print *,chname(1:lenocc(chname))
      print *,chdesc(1:lenocc(chdesc))
 
      call fmnick(1,'fmdelphi.names','LEPT92/(10:20)',
     +            chname,chdesc,' ',irc)
 
      print *,chname(1:lenocc(chname))
      print *,chdesc(1:lenocc(chdesc))
 
      call fmnick(1,'fmdelphi.names','RAWD91/17-77',
     +            chname,chdesc,' ',irc)
 
      print *,chname(1:lenocc(chname))
      print *,chdesc(1:lenocc(chdesc))
 
      end
