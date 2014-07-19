*
* $Id:
*
* $Log:
*
#include "hbook/pilot.h"
      SUBROUTINE HREADONEDIR(CDIR)
*.==========>
*     Service routine for Hrget.
*     This routine is called by the Zebra/RZ routine RZSCAN while scanning
*     a RZ directory tree.
*     Creates a directory in memory with the same name as the directory on file.
*     Reads histograms in memory
*..=========> ( R.Brun )
      character *(*) cdir
      character *80 diskdir,pawcdir
      character*256     directory
      common /hrgetdir/ directory, lendir
*
      nch = lenocc(cdir)
* Search last slash in directory
      do 10 i=nch,7,-1
         if (cdir(i:i).eq.'/') then
            if (i.gt.8) then
               diskdir = cdir(1:i-1)
c               pawcdir = '//PAWC'//cdir(8:i-1)
               pawcdir = directory(1:lendir)//cdir(8:i-1)
            else
               diskdir = '//HRGET'
c               pawcdir = '//PAWC'
               pawcdir = directory(1:lendir)
            endif
            call hcdir(pawcdir,' ')
            call hmdir(cdir(i+1:),'S')
            call hcdir(cdir,' ')
            call hrin(0,999,0)
            return
         endif
  10  continue
      end
