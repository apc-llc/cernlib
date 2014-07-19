       subroutine hisdmp(id,fname)
       integer id
       integer ityp
       integer kind(32)
       character*(*) fname
       character*80 title

       lun = 96
       open( unit=lun, file=fname, status='unknown' )

       call hkind(id,ityp,' ')
       if ( ityp .le. 0 ) then
          write(lun,*) 'No such histogram'
          close( lun )
	  return
       end if

       call hkind(id,kind,'A')
       call hgive(id,title,nx,xl,xh,ny,yl,yh,nwt,loc)

       write(lun,*) title
       write(lun,'(32i2)') kind
       write(lun,'(i4,2f14.6)') nx,xl,xh,ny,yl,yh 

       if ( ny .eq. 0 ) then
	  do 10 i = 0,nx+1
             write(lun,'(i4,2f14.6)') i, hi(id,i),hie(id,i)
 10       continue
       else
	  do 20 i = 0,nx+1
	     do 30 j = 0,ny+1
                write(lun,'(2i4,2f14.6)') i,j,hij(id,i,j),hije(id,i,j)
 30          continue
 20       continue
       end if

       close( lun )
       return
       end
