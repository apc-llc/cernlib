*
* $Id: cron.F,v 1.1.1.1 1996/03/08 15:44:23 mclareni Exp $
*
* $Log: cron.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:23  mclareni
* Cspack
*
*
      PROGRAM CRON
*-- Author :    Jamie Shiers   25/04/91
*     Modified from VXCERN Cron - remote VMS dependancies where possible
#if defined(CERNLIB_VAXVMS)
      implicit integer (s)
 
      structure /itmlst/
      union
      map
      integer*2 buffer_length
      integer*2 item_code
      integer*4 buffer_address
      integer*4 return_length_address
      endmap
      map
      integer*4 end_list /0/
      endmap
      end union
      end structure
#endif
      common/slate/is(40)
      character*119 chrep
      character*23  time
      character*36  months
      character*3   mon
      character*100 filename,save,crontab
      character*256 line(1000),string
      character*30  field
      integer       ifield(5)
      integer       irep(8)
      integer       systemf,sleepf
 
#if defined(CERNLIB_VAXVMS)
      record /itmlst/ lnm_list(2)
 
      include '($lnmdef)'
      include '($ssdef)'
      include '($lckdef)'
      integer  lksb(6)
#endif
 
      real*4  seconds
      integer delta/10/,hour,minute,second
      integer days_in_month(12)/31,28,31,30,31,30,31,31,30,31,30,31/
      data months/'JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC'/
 
#if defined(CERNLIB_VAXVMS)
*
*     Set process name
*
      status = sys$setprn('RUN_Cron LOCKED')
      if (.not. status) call lib$signal(%val(status))
 
      lkmode = lck$k_exmode
      lflags = lck$m_system
      print *,'Waiting for cluster wide CRON lock'
      status = sys$enqw(,%val(lkmode),lksb,%val(lflags),
     + 'CLUSTER$CRON',,,,,,)
      if (.not. status) call lib$signal(%val(status))
      print *,'Lock granted'
*
*     Get name of CRON table
*
      lnm_list(1).buffer_length           = 100
      lnm_list(1).item_code               = lnm$_string
      lnm_list(1).buffer_address          = %loc(crontab)
      lnm_list(1).return_length_address   = %loc(lencron)
 
      lnm_list(2).end_list                = 0
 
      status = sys$trnlnm(,'LNM$SYSTEM_TABLE','CRONTAB',,lnm_list)
 
      if (status .eq. ss$_nolognam) crontab = 'CRON.DAT'
 
      lencron = lenocc(crontab)
      if (index(crontab,';') .eq. 0) then
 
         lencron = lencron + 1
         crontab(lencron:lencron) = ';'
 
      endif
 
      status = sys$setprn('RUN_Cron')
      if (.not. status) call lib$signal(%val(status))
 
#endif
#if defined(CERNLIB_UNIX)
      call getenvf(CRONTAB,crontab)
      if(is(1).eq.0) crontab = 'cron.dat'
      filename = crontab
#endif
      print *,'CRON table is ',crontab(1:lenocc(crontab))
*
*     Initialisation
*
      call datime(id,it)
      seconds = float(delta) * 60.
      call caldat(0,chrep,irep,ic)
      iday = irep(7)
*
*     ifield(1) = minutes
*     ifield(2) = hours
*     ifield(3) = days
*     ifield(4) = months
*     ifield(5) = weekday
*
      ifield(1) = is(5)
      ifield(2) = is(4)
      ifield(3) = is(3)
      ifield(4) = is(2)
      ifield(5) = iday
*
*     Round down minutes field to multiples of 10
*
      ifield(1) = (ifield(1)/10) * 10
      write (time,9001) ifield(3),ifield(4),ifield(2),ifield(1),isecs
      print *,'Internal time = ',time
   30 continue
 
      print *,'Cron awakes...'
      i = 0
#if defined(CERNLIB_VAXVMS)
      call lib$find_file(crontab(1:lencron),filename,i)
      if (save .eq. filename) goto 99
#endif
      save = filename
      lenfile = lenocc(filename)
      print *,'Reloading command table from ',filename
 
      open (unit=1,file=filename,status='OLD',
#if defined(CERNLIB_VAXVMS)
     +readonly,shared,
#endif
     +err=90)
 
      nlines = 0
    1 continue
      read (1,'(a)',end=99) string
 
      if (string(1:1) .eq. '!') goto 1
 
      nlines = nlines + 1
      line(nlines) = string
      goto 1
 
   90 continue
      print *,'*** Error opening CRON.DAT ***'
      goto 91
 
   99 close (1)
   91 continue
 
#if defined(CERNLIB_VAXVMS)
      call szopen(10,'cron_temp.com',80,'NF',IRC)
      write (10,9003)
 9003 format('$ set noon')
#endif
      ncomm = 0
 
      do 10 i=1,nlines
         string = line(i)
         len = lenocc(string)
         do 20 j=1,5
*
*     Now check each field. Fields are:
*     1) Minutes
*     2) Hours
*     3) Days
*     4) Months
*     5) Weekday
*     6) Command
*
            field = string(1:index(string,' '))
            string = string(index(string,' ')+1:len)
 
            if (index(field,'-') .ne. 0 ) then
*
*     Range of values. Check current value is within range
*
               read (field(1:index(field,'-')),*) ilower
               read (field(index(field,'-')+1:30),*) iupper
               if (ifield(j) .lt. ilower .or. ifield(j) .gt. iupper)
     +         goto 10
 
            else if (index(field,',') .ne. 0 ) then
*
*     Series of values, check each range at a time
*
               field(index(field,' '):index(field,' ')) = ','
    2          continue
               read (field(1:index(field,',')),*) icheck
               field = field(index(field,',')+1:30)
*
*     If this is the MINUTE field, have to check backwards DELTA minutes.
*
               if (j .eq. 1) then
                  ilower = ifield(j) - delta
                  iupper = ifield(j)
                  if (ilower .lt. 0) then
                     ilower = ilower + 60
                     iupper = iupper + 60
                     icheck = icheck + 60
                  endif
 
                  if (icheck .gt. ilower .and. icheck .le. iupper)
     +            goto 20
*
*     otherwise just check value is correct
*
               else
                  if (icheck .eq. ifield(j)) goto 20
               endif
 
               if (field(1:1) .ne. ' ') goto 2
               goto 10
 
            else if (field .eq. '*') then
*
*     Star = accept all values
*
               goto 20
 
            else
 
               read(field,*,end=98) itemp
*
*     If this is the MINUTE field, have to check backwards DELTA minutes.
*
               if (j .eq. 1) then
                  ilower = ifield(j) - delta
                  iupper = ifield(j)
                  if (ilower .lt. 0) then
                     ilower = ilower + 60
                     iupper = iupper + 60
                     itemp = itemp + 60
                  endif
 
                  if (itemp .le. ilower .or. itemp .gt. iupper) goto
     +            10
*
*     otherwise just check value is correct
*
               else
                  if (itemp .ne. ifield(j)) goto 10
               endif
 
            endif
 
   20    continue
         ncomm = ncomm + 1
         print *,'Line matches - ',line(i)(1:lenocc(line(i)))
#if defined(CERNLIB_VAXVMS)
         write(10,'(a)') '$ '//string(1:lenocc(string))
#endif
#if defined(CERNLIB_UNIX)
         ic = systemf(string(1:lenocc(string)))
#endif
         goto 10
   98    continue
         print *,'End of file processing line -',line(i)
         print *,'Line ',i,' skipped'
   10 continue
      close (10)
 
#if defined(CERNLIB_VAXVMS)
      if (ncomm .ne. 0) ic = systemf('cron_temp.com')
#endif
 
*
*     Increase time interval by DELTA
*
      ifield(1) = ifield(1) + delta
*
*     Next hour
*
      if (ifield(1) .ge. 60) then
 
         ifield(1) = ifield(1) - 60
         ifield(2) = ifield(2) + 1
 
      endif
*
*     Next day
*
      if (ifield(2) .ge. 24) then
 
         ifield(2) = ifield(2) - 24
         ifield(3) = ifield(3) + 1
         ifield(5) = ifield(5) + 1
 
      endif
*
*     Next month
*
      imonth = index(months,mon)/3 + 1
      if (ifield(3) .gt. days_in_month(imonth)) then
 
         ifield(3) = 1
         ifield(4) = imonth
 
      endif
 
      if (ifield(4) .gt. 12) ifield(4) = 1
      if (ifield(5) .gt. 7)  ifield(5) = ifield(5) - 7
      write (time,9001) ifield(3),ifield(4),ifield(2),ifield(1),isecs
      print *,'Next internal wakeup time = ',time
      call datime(id,it)
      write(time,'(i6,i4,i2)') id,it,is(6)
*     call lib$date_time(time)
      print *,'Current real time = ',time
*     read (time,9002) hour,minute,second
*9002 format(12x,i2,1x,i2,1x,i2)
 
      hour   = is(4)
      minute = is(5)
      second = is(6)
 
      iwait = minute*60 + second
      inter = ifield(1) * 60
 
      icheck = (minute/10) * 10
      if ((hour .eq. ifield(2)) .and. (icheck .eq. ifield(1))) then
 
         print *,'Not waiting - attempting to catch up'
         goto 30
 
      endif
 
      if (inter .lt. iwait) inter = inter + 3600
 
      jwait = inter - iwait
*
*     Now wait
*
      if (wait .gt. 3600) then
 
         print *,'Timing error detected ',jwait,' seconds to wait'
         stop 8
      endif
 
      print *,'Waiting for ',jwait,' seconds'
      ic     = sleepf(jwait)
#if defined(CERNLIB_VAXVMS)
      status = lib$delete_file('cron_temp.com;')
#endif
      goto 30
 9001 format(i2,1x,i2,6x,i2,1x,i2,1x,i2)
      end
