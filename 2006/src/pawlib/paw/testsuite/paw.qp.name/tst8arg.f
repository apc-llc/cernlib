      function tst8arg(r1,i1,b1,d1,d2,b2,i2,r2)
      real r1, r2
      integer i1, i2
      logical b1, b2, ltmp
      double precision d1, d2
      lmtp = (b1.and..not.b2).or.(.not.b1.and.b2)
      if ( r1.ne.r2.or.i1.ne.i2.or.ltmp.or.r1.ne.r2) then
         tst8arg = 0.
      else
         tst8arg = 1.
      endif
      return
      end
