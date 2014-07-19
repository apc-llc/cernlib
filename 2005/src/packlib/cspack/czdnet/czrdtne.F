*
* $Id: czrdtne.F,v 1.1.1.1 1996/03/08 15:44:24 mclareni Exp $
*
* $Log: czrdtne.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:24  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
#if defined(CERNLIB_VAXVMS)
      integer function lib$get_noecho(input,prompt,length)
 
      implicit integer (s)
 
      character*(*) input,prompt
 
      include '($iodef)'
      integer*2 tt_chan,iosb(4)
 
 
      length = len(prompt)
*
*      Assign a channel to terminal
*
      status = sys$assign('sys$command',tt_chan,,)
      if (.not. status) call lib$signal(%val(status))
 
      modifier = io$_readprompt .or. io$m_noecho
 
      status = sys$qiow(%val(1),%val(tt_chan),%val(modifier),
     +                  iosb,,,%ref(input),%val(80),,,
     +                  %ref(prompt),%val(length))
      if (.not. status) call lib$signal(%val(status))
 
      lib$get_noecho = iosb(1)
      length         = iosb(2)
 
      return
      end
#endif
