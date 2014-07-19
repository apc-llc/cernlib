*
* $Id: load.F,v 1.1.1.1 1996/03/08 15:33:02 mclareni Exp $
*
* $Log: load.F,v $
* Revision 1.1.1.1  1996/03/08 15:33:02  mclareni
* Kuip
*
*
#include "sys/CERNLIB_machine.h"
#include "_kuip/pilot.h"
*CMZ :  1.68/00 02/03/92  13.55.43  by  Fons Rademakers
*-- Author :

*****************************************************************
*                                                               *
            macro  load  name=x  os=x  ftn=x  cpu=x  dev=x
*                                                               *
*****************************************************************

mess 'Setting compile flags and loading sequences....'

if [name] = apollo goto apollo
if [os]   = unix   goto unix
if [os]   = vm     goto vm
if [os]   = newlib goto newlib
if [os]   = vms    goto vms
   goto error

apollo:
   sel . apollo abbrev double qxno_sc bsd

   set [ftn]/*.ftn -f
   set kuip.lib    -l

   if [dev] <> x goto devapo
      if [cpu] <> m68k goto a88k
         set 'ftn $compfile -b $compfile.bin -zero -save -indexl -frnd _
              -cpu 3000' -c
         goto endif
      a88k:
         set 'ftn $compfile -b $compfile.bin -zero -save -indexl -natural _
              -cpu at' -c
         goto endif
   devapo:
      if [cpu] <> m68k goto deva88k
         set 'ftn $compfile -b $compfile.bin -zero -save -indexl -pic -frnd _
              -cpu 3000 -dba' -c
         goto endif
      deva88k:
         set 'ftn $compfile -b $compfile.bin -zero -save -indexl -natural _
              -cpu at -dba' -c
         goto endif

unix:
   set [ftn]/*.f  -f
   set c   -lan
   set [ftn]/*.c  -f
   set ftn -lan
   set kuip.a     -l

   if [name] = sgi    goto sgi
   if [name] = hpux   goto hpux
   if [name] = sun    goto sun
   if [name] = decs   goto decs
   if [name] = ibmrt  goto ibmrt
   if [name] = aix370 goto aix370
   if [name] = cray   goto cray
      goto error

   sgi:
      sel . [name] [os] abbrev double bslash csetjmp qx_sc bsd
      OPTF = -static -O -nocpp
      goto endif1

   hpux:
      sel . [name] [os] abbrev double csetjmp qx_sc sys5
      OPTF = -K +ppu -O
      goto endif1

   sun:
      sel . [name] [os] abbrev double bslash csetjmp qx_sc bsd
      OPTF = -O
      goto endif1

   decs:
      sel . [name] [os] abbrev double bslash csetjmp qx_sc sys5
      OPTF = -static -G 3 -O
      goto endif1

   ibmrt:
      sel . [name] [os] abbrev double bslash csetjmp qx_sc sys5
      set 'xlf -qextname -qrndsngl -qcharlen=32767 -O -c $compfile' -c
      goto endif

   aix370:
      sel . [name] [os] abbrev double qx_sc
      set 'xlf -qextname -qrndsngl -qcharlen=32767 -O -c $compfile' -c
      goto endif

   cray:
      sel . [name] abbrev qxcapt
      set 'cft77 -ed -o off -b $compfile.o $compfile' -c
      goto endif

   endif1:
   if [dev] <> x goto devset
      set 'f77 @[OPTF] -c $compfile' -c
      set c   -lan
      set 'cc  @[OPTC] -c $compfile' -c
      set ftn -lan
      goto endif
   devset:
      * default is debug
      goto endif

vm:
   sel . [name] ibmall abbrev double

   set kuip.fortran.[ftn] -f
   set kuip.txtlib.a      -l
   set 'vfort $compfile "(" noprint charlen "("15500")"' -c
   goto endif

newlib:
   sel . [name] ibmmvs ibmall abbrev double

   mess '*** set compiler directive correctly ***'
   goto error
*   set kuip.fortran.[ftn] -f
*   set kuip.txtlib.a      -l
*   set 'vfort $compfile "(" noprint charlen "("15500")"' -c
*   goto endif

vms:
   sel . [name] abbrev double

   set '[.'//[ftn]//']kuip.for' -f
   set kuip.olb -l
   if [dev] <> x goto devvms
      set '$ for/obj=$compfile.obj $compfile' -c
      goto endif
   devvms:
      * default is debug
      goto endif

endif:

seq . /kucdes

goto end

error:
mess *** Error in load: name = [name], os = [os] ***

end:
mess 'Setting compile flags and loading sequences....done'

return

