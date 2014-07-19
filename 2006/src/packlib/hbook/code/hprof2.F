*
* $Id: hprof2.F,v 1.1.1.1 1996/01/16 17:07:46 mclareni Exp $
*
* $Log: hprof2.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:46  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.20/03 28/07/93  09.26.34  by  Rene Brun
*-- Author :    Rene Brun   28/07/93
      Subroutine hprof2(id,idp,chopt)
*
*      Converts a 2-d histogram ID into a profile histogram IDP
*      IDP is automatically created if it does not exists.
*      Note:
*         Information may be lost in a cell (i,j) if packing is used
*         Profile histograms cannot be filled with weights. This routine	
*         assumes that ID has been filled with weigths=1
*         CHOPT:
*           'S'  Profile will be withe Spread option (default=error on mean)
*           'X'  Profile will be along X (default)
*           'Y'  Profile will be along Y
*
      character*80 title
      character*1 chopt
      logical hexist
      dimension iopt(3)
      equivalence (iopt(1),ioptx),(iopt(2),iopty),(iopt(3),iopts)
#include "hbook/hcunit.inc"
*._________________________________________________________
*
      call hgive(id,title,ncx,xmin,xmax,ncy,ymin,ymax,nwt,idb)
      if(ncy.le.0)then
         call hbug('Not a 2-d histogram','HPROF2',id)
         return
      endif
      call huoptc(chopt,'XYS',iopt)
      if(iopty.eq.0)ioptx=1
      if(iopty.ne.0.and.ioptx.ne.0)iopty=0
      if(.not.hexist(idp))then
         if(ioptx.ne.0)then
            call hbprof(idp,title,ncx,xmin,xmax,ymin,ymax,chopt)
         else
            call hbprof(idp,title,ncy,ymin,ymax,xmin,xmax,chopt)
         endif
      endif
      dx2 = 0.5*(xmax-xmin)/float(ncx)
      dy2 = 0.5*(ymax-ymin)/float(ncy)
      ibad=0
      do 20 j=1,ncy
         do 10 i=1,ncx
            cont=hij(id,i,j)
            n=cont
            xn=n
            if(xn.ne.cont)ibad=ibad+1
            call hijxy(id,i,j,x,y)
            do 5 k=1,n
               if(ioptx.ne.0)then
                  call hfill(idp,x,y+dy2,1.)
               else
                  call hfill(idp,y,x+dx2,1.)
               endif
   5        continue
  10     continue
  20  continue
      if(ibad.ne.0)then
         write(lout,1000)ibad
 1000    format(' HPROF2:',i6,' cells have non-integer contents')
      endif
      end
