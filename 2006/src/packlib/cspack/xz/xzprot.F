*
* $Id: xzprot.F,v 1.1.1.1 1996/03/08 15:44:31 mclareni Exp $
*
* $Log: xzprot.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:31  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
#if defined(CERNLIB_VAXVMS)
      integer function xzprot(fab,rab,lun)
*-- Author :    Jamie Shiers   02/08/91
#include "cspack/vmsprot.inc"
      character*30 spaces
      include '($fabdef)'
      include '($rabdef)'
      include '($xabdef)'
      include '($xabdatdef)'
      include '($xabprodef)'
 
      record /fabdef/ fab
      record /rabdef/ rab
 
      structure /xabdat/
         byte       xab$b_cod
         byte       xab$b_bln
         integer*2  %fill
         integer*4  xab$l_nxt
         integer*2  xab$w_rvn
         integer*2  %fill
         integer*4  xab$q_rdt(2)
         integer*4  xab$q_cdt(2)
         integer*4  xab$q_edt(2)
         integer*4  xab$q_bdt(2)
      end structure
 
      structure /xabpro/
         byte       xab$b_cod
         byte       xab$b_bln
         integer*2  %fill
         integer*4  %fill
         integer*2  xab$w_pro
         byte       xab$b_mtacc
         byte       xab$b_prot_opt
         union
             map
                integer*4 xab$l_uic                 !  UIC CODE
             end map
             map
                integer*2 xab$w_mbm                 !  MEMBER CODE
                integer*2 xab$w_grp                 !  GROUP CODE
             end map
         end union
         union
            map
               integer*4 xab$q_prot_mode(2)
            end map
            map
               byte      xab$b_prot_mode
            end map
         end union
         integer*4 xab$l_aclbuf
         integer*2 xab$w_aclsiz
         integer*2 xab$w_acllen
         integer*4 xab$l_aclctx
         integer*4 xab$l_aclsts
         integer*4 %fill
         integer*4 %fill
         integer*4 %fill
         integer*4 %fill
         integer*4 %fill
         integer*4 %fill
         integer*4 %fill
         integer*4 %fill
         integer*4 %fill
         integer*4 %fill
         integer*4 %fill
         integer*4 %fill
      end structure
 
      record /xabdat/ xab
      record /xabpro/ xab1
      integer status,lun,sys$open,sys$close,recfm
 
      xab.xab$b_cod = xab$c_dat
      xab.xab$b_bln = xab$c_datlen
      fab.fab$l_xab = %loc(xab.xab$b_cod)
      xab.xab$l_nxt = %loc(xab1.xab$b_cod)
      xab1.xab$b_cod = xab$c_pro
      xab1.xab$b_bln = xab$c_prolen
*
*     Set file protection mask
*     Bit is set to deny access of specified type
*
*     File protection mask is passed in IPROT(16)
*     system,owner,group,world
*     IPROT(J) = 0 for access, /= 0 for no access
*
      lprot  = 1
      do 10 j=0,12,4
 
      if(iprot(j+1).ne.0) xab1.xab$w_pro =
     +   ibset(xab1.xab$w_pro,j+xab$v_noread)
      if(iprot(j+2).ne.0) xab1.xab$w_pro =
     +   ibset(xab1.xab$w_pro,j+xab$v_nowrite)
      if(iprot(j+3).ne.0) xab1.xab$w_pro =
     +   ibset(xab1.xab$w_pro,j+xab$v_noexe)
      if(iprot(j+4).ne.0) xab1.xab$w_pro =
     +   ibset(xab1.xab$w_pro,j+xab$v_nodel)
 
10    continue
 
      xzprot = sys$create(fab)
      if(xzprot) status = sys$connect(rab)
 
      end
 
#endif
