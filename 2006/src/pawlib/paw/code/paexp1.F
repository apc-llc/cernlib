*
* $Id:
*
* $Log:
*
      subroutine paexp1(ext,j,i,ierr)
*
* Used from paexpr.
* The routine compile the file and remove the extension
* from the final expression.
*
#include "paw/pcchar.inc"
*
      character*1 c
      character*(*) ext
*
      ilen         = lenocc(ext)
      CHFILE(j:j)  = '.'
      CHFILE(j+1:) = ext(1:ilen)
      CALL PAWFCA (CHFILE,lenocc(CHFILE),jadf,0)
      if (jadf.le.0) then
         ierr=1
         return
      endif
*
      n = ilen+1
      ilen = lenocc(CHTEMP)
      do j=i,ilen-n
        c           = CHTEMP(j+n:j+n)
        CHTEMP(j:j) = c
      enddo
      CHTEMP(ilen-n+1:ilen) = ' '
*
      end
