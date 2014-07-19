*
* $Id:
*
* $Log:
*
      subroutine paexpr(ierr)
*
* This routine find file names into an expression and compile
* them. The extensions allowed are .f .for .f77 .c (in lower
* or upper case. Note that this function works on the PCCHAR
* common block.
*
#include "paw/pcchar.inc"
#include "paw/pcslas.inc"
*
      logical filling
*
 10   ierr    = 0
      ilen    = lenocc(CHTEMP)
      filling = .false.
*
* First, treat the case where file names are enclosed with "
*
      CHFILE  = ' '
      CHTITL  = ' '
      i       = INDEX(CHTEMP,'"')
      j       = 1
*
      if (i.ne.0) Then
         k = i-1
 11      i = i+1
         if (i.gt.ilen) then
            ierr=1
            goto 999
         endif
         CHFILE(j:j) = CHTEMP(i:i)
         if (CHTEMP(i+1:i+1).eq.'"') then
            CALL PAWFCA (CHFILE,lenocc(CHFILE),jadf,0)
            iF (JADF.LE.0) then
               ierr=1
               goto 999
            else
               i2 = INDEX(CHFILE,'.')-1
               i1 = i2
 12            if (CHFILE(i1:i1).ne.'/'.and.
     +             CHFILE(i1:i1).ne.BSLASH.and.
     +             CHFILE(i1:i1).ne.']') then
                   i1 = i1-1
                   if(i1.eq.0)goto 13
                   goto 12
               endif
 13            i1=i1+1
               if (k.ge.1) then
                  CHTITL = CHTEMP(1:k)//CHFILE(i1:i2)
     +                   //CHTEMP(i+2:lenocc(CHTEMP))
               else
                  CHTITL = CHFILE(i1:i2)//CHTEMP(i+2:lenocc(CHTEMP))
               endif
               CHTEMP = CHTITL
               goto 10
            endif
         endif
         j = j+1
         goto 11
      endif
*
      CHFILE  = ' '
      i       = 1
      j       = 1
*
 1    if (i.gt.ilen) goto 999
*
         if (filling .and. CHTEMP(i:i).eq.'.') then
            if (CHTEMP(i+1:i+1).eq.'c') then 
               call paexp1('c',j,i,ierr)
            else if (CHTEMP(i+1:i+3).eq.'f77') then 
               call paexp1('f77',j,i,ierr)
            else if (CHTEMP(i+1:i+3).eq.'for') then 
               call paexp1('for',j,i,ierr)
            else if (CHTEMP(i+1:i+1).eq.'f') then 
               call paexp1('f',j,i,ierr)
            else if (CHTEMP(i+1:i+1).eq.'C') then 
               call paexp1('C',j,i,ierr)
            else if (CHTEMP(i+1:i+3).eq.'F77') then 
               call paexp1('F77',j,i,ierr)
            else if (CHTEMP(i+1:i+3).eq.'FOR') then 
               call paexp1('FOR',j,i,ierr)
            else if (CHTEMP(i+1:i+1).eq.'F') then 
               call paexp1('F',j,i,ierr)
            else
               ierr = 1
               goto 999
            endif
*
            CHFILE  = ' '
            j       = 1
            i       = i+1
            filling = .false.
            goto 1
         endif
*
         ich = ichar(CHTEMP(i:i))
         if ((ich.ge.65.and.ich.le.90) .or.
     +      (ich.ge.97.and.ich.le.122)) then
            if (filling) then
               CHFILE(j:j) = CHTEMP(i:i)
            else
               filling     = .true.
               j           = 1
               CHFILE(j:j) = CHTEMP(i:i)
            endif
            j = j+1
         else
            if (filling) then
               if(ich.ge.48.and.ich.le.57)then
                  CHFILE(j:j) = CHTEMP(i:i)
                  j           = j+1
               else
                  filling     = .false.
                  CHFILE      = ' '
                  j           = 1
               endif
            endif
         endif
*            
      i = i+1
      goto 1
999   CHTITL = CHTEMP
      end
