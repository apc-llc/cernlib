      double precision function dret(d1,d2,i)
      double precision d1, d2
      if ( i .eq. 1 ) then
         dret = d1
      else
         dret = d2
      end if
      return
      end
