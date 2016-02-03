module interp2d
  implicit none
  save
  integer, parameter       :: maxx = 1001
  integer                  :: irate
  integer, dimension(maxx) :: indxx, indxy
  real, dimension(maxx)    :: ratex, ratey 
end module 
subroutine c_format(x,nc,lab)
!* - cformat - c_format - encodes numbers for writing

implicit none

integer jfmt, ilog, ifmt, ifrac, iwdth
integer i, nc, j1, j2, ixa, ix, nch, ieof
integer nu, imonth, jday, iday, kday, iyear
real x, xx
character fmt(4)*20
character*(*) lab
character*32  ctmp

data fmt(1) /'(f0.0)'/
data fmt(2) /'(1p,e0.0)'/
data fmt(3) /'(''-'',f0.0)'/
data fmt(4) /'(''-'',1p,e0.0)'/

integer, external :: nblank

lab = ' '
if(x >= 0.0)then
  jfmt = 0
else
  jfmt = 1
end if
xx = abs(x)
if (xx /= 0.0) then
  ilog = ifix(float(ifix(alog10(1.0005*xx) + 5000.)-5000))
  if (ilog < -3 .or. ilog >= 4) then
    ifmt = 2
    ifrac = 2
    iwdth = 8
  else
    ifmt = 1
    if (ilog < 0) then
      ifrac = 2 - ilog
      iwdth  = ifrac + 1
    else
      ifrac = max0(2-ilog,1)
      iwdth  = ifrac + 2 + ilog
    end if
  end if
else
  ifmt = 1
  ifrac = 1
  iwdth  = 2
end if

ifmt = ifmt + 2*jfmt
nc = iwdth + jfmt
if (ifmt == 2 .or. ifmt == 4) then 
  j1 = 2*(ifmt -1) + 4
else
  j1 = 2*(ifmt -1) + 3
end if
j2 = j1 + 2
fmt(ifmt)(j1:j1) = char(iwdth+48)
fmt(ifmt)(j2:j2) = char(ifrac+48)
write (lab(1:nc),fmt(ifmt)) xx

if(lab(1:1) == '-')then
  j1 = 2
else
  j1 = 1
end if

if(lab(j1:j1) == '.')then
  ctmp = '0'//lab(j1:nc)
  lab(j1:) = ctmp(1:nc-j1+2)
  nc = nc + 1
end if

return

end

!=======================================================================

subroutine i_format(ix,nc,lab)

character*(*) lab
character*32  ctmp

lab = ' '

ixa = iabs(ix)
write(ctmp,*)ixa

do while (ctmp(1:1) == ' ')
  ctmp = ctmp(2:)
end do

nc = 0
if(ix < 0)then
  nc = nc + 1
  lab(nc:nc) = '-'
end if

nc = nc + 1
lab(nc:nc) = ctmp(1:1)
nch = nblank(ctmp)
do i = 2,nch
  if(mod(nch-i+1,3) == 0)then
    nc = nc + 1
    lab(nc:nc) = ','
  end if
  nc = nc + 1
  lab(nc:nc) = ctmp(i:i)
end do

return
end

!=======================================================================

subroutine d_format(x,nc,lab)

character*(*) lab
character*16  cd,cm,cs

lab = ' '

xa = abs(x)

id = int(xa)
im = int((xa-float(id))*60.)
is = nint(((xa-float(id))*60. - float(im))*60.)
if(is >= 60.)then
  im = im + 1
  is = 0
end if
if(im >= 60.)then
  id = id + 1
  im = 0
end if

write(cd,*)id

do while (cd(1:1) == ' ')
  cd = cd(2:)
end do

write(cm,'(i2.2)')im
write(cs,'(i2.2)')is

nc = 0
if(x < 0)then
  nc = 1
  lab(nc:nc) = '-'
end if

nch = nblank(cd)
lab(nc+1:nc+nch) = cd(1:nch)
nc = nc + nch

lab(nc+1:nc+1) = ':'
nc = nc + 1

nch = nblank(cm)
lab(nc+1:nc+nch) = cm(1:nch)
nc = nc + nch

lab(nc+1:nc+1) = ':'
nc = nc + 1

nch = nblank(cs)
lab(nc+1:nc+nch) = cs(1:nch)
nc = nc + nch

return
end

!=======================================================================

integer function nblank(line)
character*(*) line
do 10 i = len(line),1,-1
   nblank = i
   if(line(i:i)/=' ')return
10      continue
nblank = 0
return
end

!=======================================================================

subroutine get_line (nu, nch, cline, ieof)

character*(*) cline

ieof = 0
read (nu, 100, end=900) cline
 100    format (a)
do i=len(cline),1, -1
  if (cline(i:i) /= ' ') then
    nch = i
    go to 1000
  endif
end do
nch = 0
go to 1000

 900    continue
nch = 0
ieof = -1

 1000   continue
return
end

!=======================================================================

subroutine logn_bin(bin,nb,MMD,sigma,weight)

implicit none

real    bin(1)
integer nb
real    MMD
real    sigma
real    weight(1)

real    sig,arg,fracl,fracu,erfc
integer i

sig = sqrt(2.)*alog(sigma)

fracl = 0.
do i = 1,nb-1
  arg   = alog(bin(i+1)/MMD)/sig
  fracu = 1. - 0.5*erfc(arg)
  weight(i) = fracu - fracl
  fracl = fracu
end do
weight(nb) = 1. - fracu

!       fracl = 0.
!       do i = 1,nb
!         fracl = fracl + weight(i)
!       end do

return

end

!=======================================================================

subroutine check_logn(binl,binu,MMD,sigma,frac)

implicit none

real    binl
real    binu
real    MMD
real    sigma
real    frac

real    sig,argu,argl,erfc


sig = sqrt(2.)*alog(sigma)

argl = alog(binl/MMD)/sig
argu = alog(binu/MMD)/sig
frac = 1. - 0.5*( erfc(argl) - erfc(argu) )

return

end

!=======================================================================

integer function julian_day(imonth,iday,iyear)

!  this function converts year, month and day to julian date

!PKK, ENVIRON, Feb 25, 2015: Bug-fix for leap year
 
logical leap_year

integer nday(12)
integer mday(12)

data nday / 0,31,59,90,120,151,181,212,243,273,304,334 /
data mday /31,28,31,30, 31, 30, 31, 31, 30, 31, 30, 31 /

if( (imonth <=0) .or. (imonth >= 13))then

  jday = -999

else

  jday = nday(imonth)+iday

  kday = mday(imonth)

  if( leap_year(iyear) ) then
    if(imonth > 2)then
        jday = jday + 1
    end if
    if(imonth == 2)then
      kday = kday + 1
      jday = jday + 1
    end if
  end if

  if(iday > kday)jday = -999

end if

julian_day = jday

return

end

!=======================================================================

subroutine get_md_from_julian(jday,iyear,imonth,iday)

!  this function gets the month and day from the julian date and year

implicit none
integer jday,iyear,imonth,iday
logical leap_year
integer i,nn
integer nday(12)

data nday / 31,59,90,120,151,181,212,243,273,304,334,365 /

i = 1
nn = nday(1)
do while (nn < jday)
  i = i + 1
  nn = nday(i)
  if (leap_year(iyear)) then
    nn = nn + 1
  end if
end do

imonth = i
if (imonth > 1) then
  nn = nday(imonth-1)
  if (leap_year(iyear)) then
    nn = nn + 1
  end if
  iday = jday - nn
else
  iday = jday
end if  

return

end

!=======================================================================

logical function leap_year(iyear)

if(mod(iyear,4) /= 0)then
  leap_year = .false.
else
  if(mod(iyear,100) == 0)then
    leap_year = mod(iyear,400) == 0
  else
    leap_year = .true.
  end if
end if

return
end

!=======================================================================

subroutine cupper(line)
character*(*) line
do 10 i = 1 , len(line)
   if( line(i:i) < 'a' ) go to 10
   if( line(i:i) > 'z' ) go to 10
   j = ichar(line(i:i))
   j = j - 32
   line(i:i) = char(j)
10      continue
return
end

!=======================================================================

subroutine get_value(line,nch,paste1,paste2,err)

implicit none

!------ start declarations

character*1   BLANK
parameter (BLANK = ' ')

character*1   paste1,paste2
character*(*) line

logical  err

integer nch, i

!------ end declarations

err = .false.

if((nch <= 0) .or. (line == BLANK))then
  err  = .true.
  go to 9999
end if

do i = 1, nch
  if(line(i:i) == paste1) line(i:i) = paste2
end do

1       if(line(1:1) == BLANK)then
  line = line(2:)
  nch = nch - 1
  if(nch <= 0)then
    err = .true.
    go to 9999
  end if
  goto 1
end if

9999    continue

return

end

!=======================================================================

subroutine get_i(line,nch,i_value,err)

implicit none

!------ start declarations

character*1   BLANK
parameter (BLANK = ' ')

character*(*) line
character*5 cfmt

logical  err

integer i_value, nch, ll, ios

!------ end declarations

err = .false.

if((nch <= 0) .or. (line == BLANK))then
  err  = .true.
  go to 9999
end if

100     if(line(1:1) == BLANK)then
  line = line(2:)
  nch = nch - 1
  if(nch <= 0)then
    err = .true.
    go to 9999
  end if
  goto 100
end if

ll = index(line,BLANK) - 1
if(ll <= 0) ll = nch

write(cfmt,2000) ll
2000    format('(i',i2,')')

read(line,cfmt,iostat=ios)i_value
if (ios /= 0) then
  err = .true.
  goto 9999
end if

line(1:) = line(ll+1:)
nch = nch - ll

9999    continue

return

end

!=======================================================================

subroutine get_r(line,nch,r_value,err)

implicit none

!------ start declarations

character*1   BLANK
parameter (BLANK = ' ')

character*(*) line
character*7 cfmt

logical err

real r_value

integer nch, ll, ios

!------ end declarations

err = .false.

if((nch <= 0) .or. (line == BLANK))then
  err  = .true.
  go to 9999
end if

200     if(line(1:1) == BLANK)then
  line = line(2:)
  nch = nch - 1
  if(nch <= 0)then
    err = .true.
    go to 9999
  end if
  goto 200
end if

ll = index(line,BLANK) - 1
if(ll <= 0) ll = nch

write(cfmt,2000) ll
2000    format('(f',i2,'.0)')

read(line,cfmt,iostat=ios)r_value
if (ios /= 0) then
  err = .true.
  goto 9999
end if

line(1:) = line(ll+1:)
nch = nch - ll

9999    continue

return

end

!=======================================================================

subroutine get_c(line,nch,delimit,c_value,ncc,err)

implicit none

!------ start declarations

character*1   BLANK
parameter (BLANK = ' ')

character*(*) c_value
character*(*) line
character*(*) delimit

logical  err

integer ncc, nch, i, ncd, j, ll

!------ end declarations

err = .false.

if((nch <= 0) .or. (line == BLANK))then
  err  = .true.
  go to 9999
end if

300     if(line(1:1) == BLANK)then
  line = line(2:)
  nch = nch - 1
  if(nch <= 0)then
    err = .true.
    go to 9999
  end if
  goto 300
end if

if(delimit == BLANK)then
  ll = index(line,BLANK) - 1
  if(ll <= 0) ll = nch
  c_value = line(1:ll)
  ncc     = ll
  line(1:) = line(ll+2:)
  nch = nch - (ll+2) + 1
else
  ncd = len(delimit)
  i = index(line,delimit)
  j = index(line(i+ncd:),delimit)
  if((i <= 0) .or. (j <= 0))then
    err = .true.
    go to 9999
  else
    c_value = line(i+ncd:i+ncd+j-2)
    ncc = j-1
    line = line(i+ncd+j-1+ncd:)
    nch  = nch - (i+ncd+j-1+ncd) + 1
  end if
end if

9999    continue

return

end

!=======================================================================

subroutine clower(line)

character*(*) line
do 10 i = 1 , len(line)
   if( line(i:i) < 'A' ) go to 10
   if( line(i:i) > 'Z' ) go to 10
   j = ichar(line(i:i))
   j = j + 32
   line(i:i) = char(j)
10      continue
return
end

!=======================================================================

subroutine interp_2d(xi,nxi,yi,nyi,vi,mxi,xo,nxo,yo,nyo,vo,mxo,iflag)

use interp2d

!------ 2-d linear interpolation routine - no extrapolation

dimension xi(*)
dimension yi(*)
dimension vi(mxi,*)
dimension xo(*)
dimension yo(*)
dimension vo(mxo,*)


!====== check limits

nn = max0( nxo,nyo )
if(nn > maxx .or. iflag < 0)then
  iflag = -1
  return
end if

!====== set rates if iflag > 0

if(iflag > 0)then

!-------- x direction

  do i = 1,nxo

    ii = 1
    do while( xo(i) >= xi(ii) .and. ii <= nxi)
      ii = ii + 1
    end do

    if(ii == 1)then
      ratex(i) = 0.0
      indxx(i) = 2
    else if(ii > nxi)then
      ratex(i) = 1.0
      indxx(i) = nxi
    else
      ratex(i) = (xo(i)-xi(ii-1))/(xi(ii)-xi(ii-1))
      indxx(i) = ii
    end if

  end do

!-------- y direction

  do i = 1,nyo

    ii = 1
    do while( yo(i) >= yi(ii) .and. ii <= nyi)
      ii = ii + 1
    end do

    if(ii == 1)then
      ratey(i) = 0.0
      indxy(i) = 2
    else if(ii > nyi)then
      ratey(i) = 1.0
      indxy(i) = nyi
    else
      ratey(i) = (yo(i)-yi(ii-1))/(yi(ii)-yi(ii-1))
      indxy(i) = ii
    end if

  end do

  iflag = 0
  irate = 1234

end if

!====== interpolate

if(irate /= 1234)then
  iflag = -4
  return
end if

do j = 1,nyo
  jp = indxy(j)
  jm = jp - 1
  ryp = ratey(j)
  rym = 1. - ryp
  do i = 1,nxo
    ip = indxx(i)
    im = ip - 1
    rxp = ratex(i)
    rxm = 1. - rxp
    val = rxp*ryp*vi(ip,jp) + rxp*rym*vi(ip,jm) + &
               rxm*ryp*vi(im,jp) + rxm*rym*vi(im,jm)
    vo(i,j) = val
  end do
end do

return

end

!=======================================================================

subroutine init_interp_2d

use interp2d

irate = 0

return
end

!=======================================================================

subroutine vsort(x,ix,n)

dimension x(1),ix(1)
dimension il(21),iu(21)

nn = n
if (nn<=1) return

m = 1
i = 1
j = nn
r = 0.375

125     k = i
!                                           select a central element of the
!                                           array and save it in location t
ij = i + ifix (float (j-i) * r)
t  = x(ij)
it = ix(ij)
!                                           if first element of array is greater
!                                           than t, interchange with t
if (x(i) > t) then
  x(ij) = x(i)
  x(i)  = t
  t     = x(ij)
  ix(ij) = ix(i)
  ix(i)  = it
  it     = ix(ij)
end if
l = j
!                                           if last element of array is less than
!                                           t, interchange with t
if (x(j) < t) then
  x(ij) = x(j)
  x(j)  = t
  t     = x(ij)
  ix(ij) = ix(j)
  ix(j)  = it
  it     = ix(ij)
!                                           if first element of array is greater
!                                           than t, interchange with t
  if (x(i) > t) then
    x(ij) = x(i)
    x(i)  = t
    t     = x(ij)
    ix(ij) = ix(i)
    ix(i)  = it
    it     = ix(ij)
  end if
end if
!                                           find an element in the second half of
!                                           the array which is smaller than t
140     l = l - 1
do while (x(l) > t)
  l = l - 1
end do
!                                           find an element in the first half of
!                                           the array which is greater than t
k = k + 1
do while (x(k) < t)
  k = k + 1
end do
!                                           interchange these elements
if (k < l) then
  tt   = x(l)
  x(l) = x(k)
  x(k) = tt
  itt   = ix(l)
  ix(l) = ix(k)
  ix(k) = itt
  goto 140
else if (k == l)then
  k = k + 1
  l = l - 1
end if
!                                           save upper and lower subscripts of
!                                           the array yet to be sorted
if (l-i > j-k) then
  il(m) = i
  iu(m) = l
  i = k
  m = m + 1
else
  il(m) = k
  iu(m) = j
  j = l
  m = m + 1
end if
!                                           begin again on another portion of
!                                           the unsorted array

155     if (j-i > 1) then
  go to 125
else if (j-i == 1)then
  k = j
  l = i
  if (x(i) > x(j))then
    tt   = x(l)
    x(l) = x(k)
    x(k) = tt
    itt   = ix(l)
    ix(l) = ix(k)
    ix(k) = itt
  end if
end if

i = i - 1
165     i = i + 1
if (i == j) then
  m = m - 1
  if (m == 0) go to 300
  i = il(m)
  j = iu(m)
  goto 155
end if

t = x(i+1)
it = ix(i+1)
if (x(i) <= t) go to 165
k = i

170     x(k+1) = x(k)
ix(k+1) = ix(k)
k = k - 1
if (t < x(k)) go to 170
x(k+1)  = t
ix(k+1) = it
go to 165

300     continue
return

end

!===========================================================================

function erfc(arg)
 
data p / .3275911 /
data a1,a2,a3,a4,a5 / &
 .254829592,-.284496736,1.421413741,-1.453152027,1.061405429 /
 
t = 1./(1.+p*abs(arg))
 
erfc = (t*(a1 + t*(a2 + t*(a3 + t*(a4 + t*a5)))))*exp(-arg**2)
 
if(arg < 0.0)then
  erfc = 2. - erfc
end if
 
return
end


