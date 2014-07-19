*
* $Id: hbooknc.F,v 1.1.1.1 1996/01/16 17:07:32 mclareni Exp $
*
* $Log: hbooknc.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:32  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/11 23/08/94  14.17.45  by  Rene Brun
*-- Author :    Rene Brun   10/02/94
      Subroutine HBOOKNC(id, chtitl, nvar, block, tuple, tags)
****************************************************************
*     Action: Book variables to be stored in a CWN described by the
*     old-style mechanism used by RWN's. Book new CWN if neccesary.
*
*     INPUT:
*     id      Id of CWN. If it doesn't already exist, it is created.
*     chtitl  Name of ntuple. Not used if it already exists.
*     nvar    Number of variables per event. Maximum 200 ???
*     block   Name of the block inside CWN. Default 'Block1'.
*     tuple   Array of dimension nvar that will contain values at filling time.
*     tags    See HBOOKN.
*
*     Author: Achim Braemer, braemer@doc.physi.uni-heidelberg.de
*     Rewritten by Rene Brun to remove limitations on nvar
****************************************************************
      implicit none
 
      integer id, nvar, i,nbvmax
      character*(*) chtitl, block
      real tuple(*)
      character*(*) tags(*)
 
      parameter (nbvmax=40)
      integer icold,icnew,nbn,nch,ibl,lenocc
      character*8 tag1,bname
      character*1300 chform
      character*1 type
 
      logical hexist
****************************************************************
 
*-*   -- create CWN if neccessary
      if (.not. hexist(id)) call hbnt(id, chtitl, ' ')
*
      chform=' '
      icold=1
      nbn=0
      type='R'
      do 10 i=1,nvar
         tag1=tags(i)
         if(tag1.eq.' ')then
            write(tag1,30000)i
30000       format('VAR',I3)
            if(tag1(4:4).eq.' ')tag1(4:4)='0'
            if(tag1(5:5).eq.' ')tag1(5:5)='0'
         endif
         nch=lenocc(tag1)
         icnew=icold+nch+2
         chform(icold:icnew)=tag1(1:nch)//':'//type//','
         if(mod(i,nbvmax).eq.0.or.i.eq.nvar)then
            nbn=nbn+1
*-*  Keep user block name for the first block
            if(nbn.eq.1)then
               bname=block
               if(bname.eq.' ')bname='Block1'
*-*  otherwise generate block name automatically
            elseif(nbn.gt.1 .and. nbn.lt.10) then
               write(bname,10000)nbn
10000          format('Block',i1)
            elseif (nbn.ge. 10 .and. nbn.lt. 100) then
               write(bname,10001)nbn
10001          format('Block',i2)
            elseif (nbn.ge.100 .and. nbn.lt.1000) then
               write(bname,10002)nbn
10002          format('Block',i3)
            else
               print *, 'HBOOKNC: In trouble, NBN = ',nbn
               bname = 'BlockXYZ'
            endif
            ibl=(nbn-1)*nbvmax +1
            call hbname(id,bname,tuple(ibl),chform(1:icnew-1))
            icnew=0
            chform=' '
         endif
         icold=icnew+1
   10 continue
*
      end
