*
* $Id: xzfinf.F,v 1.1.1.1 1996/03/08 15:44:30 mclareni Exp $
*
* $Log: xzfinf.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:30  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
#if defined(CERNLIB_VAXVMS)
      integer function xzfinf(fab,rab,lun)
*-- Author :    Jamie Shiers   02/08/91
#include "cspack/vmsinf.inc"
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
 
      status = sys$open(fab)
*
*     Blocks allocated
*
      iblk = fab.fab$l_alq
*
*     Dates & times
*
      status = sys$asctim(,revdat,xab.xab$q_rdt,)
      status = sys$asctim(,credat,xab.xab$q_cdt,)
      status = sys$asctim(,bacdat,xab.xab$q_bdt,)
      status = sys$asctim(,expdat,xab.xab$q_edt,)
*
*     File format
*
      chtype  = ' '
      recfm = fab.fab$b_rfm
      if(recfm.eq.fab$c_fix)   chtype = 'FIXED'
      if(recfm.eq.fab$c_var)   chtype = 'VARIABLE'
      if(recfm.eq.fab$c_stm)   chtype = 'STREAM'
      if(recfm.eq.fab$c_stmcr) chtype = 'STREAM_CR'
      if(recfm.eq.fab$c_stmlf) chtype = 'STREAM_LF'
      if(recfm.eq.fab$c_udf)   chtype = 'UNDEFINED'
      if(recfm.eq.fab$c_vfc)   chtype = 'VFC'
*
*     Check file protection mask
*     Bit is set to deny access of specified type
*
      chprot = '('
      lprot  = 1
      do 10 j=0,12,4
 
      if(.not.btest(xab1.xab$w_pro,j+xab$v_noread)) then
         lprot = lprot + 1
         chprot(lprot:lprot) = 'R'
      endif
 
      if(.not.btest(xab1.xab$w_pro,j+xab$v_nowrite)) then
         lprot = lprot + 1
         chprot(lprot:lprot) = 'W'
      endif
 
      if(.not.btest(xab1.xab$w_pro,j+xab$v_noexe)) then
         lprot = lprot + 1
         chprot(lprot:lprot) = 'E'
      endif
 
      if(.not.btest(xab1.xab$w_pro,j+xab$v_nodel)) then
         lprot = lprot + 1
         chprot(lprot:lprot) = 'D'
      endif
 
      lprot = lprot + 1
      chprot(lprot:lprot) = ','
 
10    continue
      chprot(lprot:lprot) = ')'
*
*     UIC
*
      write (chuic,9001) xab1.xab$w_grp,xab1.xab$w_mbm
9001  format('[',O5,',',O5,']')
 
      chuic = spaces(chuic,0)
 
      status = sys$close(fab)
 
      xzfinf = 1
 
      end
 
#endif
