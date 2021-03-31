

module towards

  implicit none

  integer, parameter :: chars = 30, line = 40, page = 50, seconds = 60, samples = 48000

  integer, parameter :: numosc = 4, numlayers = 4
  
  character(:), allocatable :: statement
  integer :: systextn = 95
  integer, parameter :: ngbrs = 15
  
  real(8), dimension(4,3) :: alpha, beta
  real(8), dimension(4) :: freq, bw, sinf
  real(8), dimension(4,4) :: trlaycoup ! trans layer coupling
  real(8), dimension(2) :: excoup ! external influence i.e. Gerhard and Ludvig
  real(8), dimension(4,2) :: trpagecoup ! trans page coupling
  real(8), parameter :: dt = 1.0_8 / (2.0_8 * 48000.0_8)
  integer, parameter :: fade = 4800 ! fade length in samples 
  real(8), dimension(seconds*samples) :: ger, lud
  
  real(8), dimension(:,:,:,:), allocatable :: textsys
  ! real(8), dimension(4) :: soundtemp
  ! real(8), dimension(:,:), allocatable :: soundsys ! maybe save all the state and reiterate multiple times
  real(8), dimension(:,:,:,:), allocatable :: soundsyspt, soundsyszt, soundsyst
  ! real(8), dimension(:,:,:,:), pointer :: soundsysp, soundsyst
  ! real(8), dimension(:,:,:,:), allocatable :: soundsysp, soundsyst 
  real(8), dimension(:,:), allocatable :: soundout
  
  real(8), parameter :: dttext = 0.0002  
  real(8), parameter :: pi = 4.0_8 * atan(1.0_8)
  real(8), parameter :: pi2 = 8.0_8 * atan(1.0_8)
  integer, dimension(ngbrs) :: prc, nec
  integer, dimension(ngbrs, 3) :: pplc, nplc

  real(8), dimension(:,:,:,:), allocatable :: systexttmp
  real(8), dimension(:,:,:), allocatable :: couptext
  real(8), dimension(:,:), allocatable :: inhcoup

  real(8), parameter :: inhibit = 0.999_8, inhbt = 0.7_8
  real(8) :: freqtext = 1.0, coupscaletext = 1.0
  real(8), dimension(:,:,:,:), allocatable :: textenarr

  integer, dimension(:,:,:), allocatable :: maxt

contains

  subroutine commit ()
    call system("cp ./David.elm ../src/Texts/David.elm")
    call chdir("../")
    call system("./build.sh")
  end subroutine commit


  subroutine exportForElm ()
    integer :: i, j, k
    open(unit=21,file="./David.elm",status='replace', action='write')
    write(21, "(A)") "module Texts.David exposing (texts)"
    write(21, "(A)") "import Texts"
    write(21, "(A)") "texts : List Texts.EntryWithIndex"
    write(21, "(A)") "texts = ["
    
    do i = 1, page
       if (i .eq. 1) then
          write(21, "(A)", advance="no") " ( "
       else 
          write(21, "(A)", advance="no") " , ("
       end if
       write(21, "(I2)", advance="no") i-1
       write(21, "(A)", advance="no") ", Texts.noNl """
        do j = 1, line
           do k = 1, chars
              write(21, "(A)", advance="no") i2c(maxt(i,j,k))
           end do
        end do
        write(21, "(A)") """ )"
     end do
    write(21, "(A)") " ]"
    flush(21)
    close(21)
    call commit()    
  end subroutine exportForElm
  
  subroutine exportAudio ()

    integer(kind=4) :: io
    integer :: i, j, ss
    character(len=512) :: filename

    ss = seconds*samples
    
    do i = 1, page
    
       open(unit=20,file="./david.au",form='unformatted',access='stream',status='replace',convert='big_endian')
       write(20) ".snd"
       io = 24
       write(20) io
       io = 0
       io = ior(io, z'FFFFFFFF')
       write(20) io
       io = 7
       write(20) io
       io = 48000
       write(20) io
       io = 1
       write(20) io

       do j = 1, ss
          if (j .le. fade) then 
             write(20) soundout(i,j) * sin((pi/2.0_8) * (1.0_8*(j-1))/fade)
          else if ((ss-j) .lt. fade) then
             write(20) soundout(i,j) * sin((pi/2.0_8) * (1.0_8*(ss-j))/fade)
          else
             write(20) soundout(i,j)
          end if
       end do
       flush(20)
       close(20)

       call system("sndfile-convert -pcm24 ./david.au ./david.wav")
       call system("rm ./david.au")
       if ((i-1) .lt. 10) then 
          write (filename, "(A39,I1,A1)") "cp ./david.wav ../../towards_files/page", i-1, "/"
       else
          write (filename, "(A39,I2,A1)") "cp ./david.wav ../../towards_files/page", i-1, "/"
       end if

       print *, trim(filename)
       call system(filename)
       
     end do

  end subroutine exportAudio
  
  
  
  elemental integer function c2i (c)
    character, intent(in) :: c
    c2i = ichar(c) - 31
  end function c2i

  elemental character function i2c (i)
    integer, intent(in) :: i
    if (i .eq. 1) then
       i2c = char(32)
    else
       i2c = char(i+31)
    end if
  end function i2c

  elemental real(8) function gauss(bw, x)
    real(8), intent(in) :: bw, x
    gauss = exp(-sin(x)**2 * bw)
  end function gauss

  elemental real(8) function gaussder(bw, x)
    real(8), intent(in) :: bw, x
    gaussder = exp(-sin(x)**2 * bw) * 2.0_8 * sin(x) * sqrt(bw)
  end function gaussder
  
  subroutine maxtext ( )
    integer :: i, j, k
    do i = 1, page
       do j = 1, line
          do k = 1, chars
             maxt(i,j,k) = maxloc(textenarr(i,j,k,:), 1)
          end do
       end do
    end do
  end subroutine maxtext

  subroutine getneighbours (p,l,c)
    integer, intent(in) :: p, l, c
    integer :: ct, lt, pt, i
    ct = c
    lt = l
    pt = p
    do i = 1, ngbrs
       ct = ct + 1
       if (ct .gt. chars) then
          ct = 1
          lt = lt + 1
       end if
       if (lt .gt. line) then
          lt = 1
          pt = pt + 1
       end if
       if (pt .gt. page) then
          pt = 1
       end if
       nplc(i,1) = pt
       nplc(i,2) = lt
       nplc(i,3) = ct
    end do
    ct = c
    lt = l
    pt = p
    do i = 1, ngbrs
       ct = ct - 1
       if (ct .lt. 1) then
          ct = chars
          lt = lt - 1
       end if
       if (lt .lt. 1) then
          lt = line
          pt = pt - 1
       end if
       if (pt .lt. 1) then
          pt = page
       end if
       pplc(i,1) = pt
       pplc(i,2) = lt
       pplc(i,3) = ct
    end do
    do i = 1, ngbrs
       prc(i) = maxt(pplc(i,1), pplc(i,2), pplc(i,3))
    end do
    do i = 1, ngbrs
       nec(i) = maxt(nplc(i,1), nplc(i,2), nplc(i,3))
    end do

    do i = 2, ngbrs
       inhcoup(prc(i-1), prc(i)) = inhcoup(prc(i-1), prc(i)) * inhibit
       inhcoup(nec(i), nec(i-1)) = inhcoup(nec(i), nec(i-1)) * inhibit
    end do

    inhcoup = inhcoup * (1.0_8 - inhbt) + inhbt

  end subroutine getneighbours

  real(8) function coupCalc (p, l, c, idx)
    integer, intent(in) :: p, l, c, idx
    integer :: i, j, k, d
    integer, dimension(0:ngbrs) :: prce, nece
    real(8) :: o1, o2, o3
    prce(0) = idx
    nece(0) = idx
    prce(1:ngbrs)= prc(:)
    nece(1:ngbrs)= nec(:)

    ! o1 = 1.0_8
    ! o2 = 1.0_8
    o1 = 0.0_8
    o2 = 0.0_8
    do k = 1, ngbrs
       ! o1 = o1 * (1.0_8 + couptext(k, idx, prc(k)))
       ! o1 = o1 + couptext(k, idx, prc(k)) * real(ngbrs) / k
       o3 = 1.0_8
       do i = 1, k
          ! o3 = o3 * couptext(i, prce(0), prce(i))
          o3 = o3 * inhcoup(prce(i-1), prce(i)) * couptext(1, prce(i-1), prce(i))
       end do
       o1 = o1 + o3
       ! o1 = o1 * (1.0_8 + couptext(k, prce(0), prce(k))) !* k * 1.0_8 / ngbrs
       ! o1 = o1 * (1.0_8 + couptext(k, prce(k-1), prce(k))) !* k * 1.0_8 / ngbrs
       ! o1 = o1 * inhcoup(prce(k-1), prce(k)) * couptext(1, prce(k-1), prce(k)) !* k * 1.0_8 / ngbrs
       ! o1 = o1 * couptext(k, prce(0), prce(k)) !* k * 1.0_8 / ngbrs 
       ! o1 = o1 + couptext(k, prce(0), prce(k)) !* k * 1.0_8 / ngbrs
       ! o1 = o1 + inhcoup(prce(k-1), prce(k)) * couptext(1, prce(k-1), prce(k)) !* k * 1.0_8 / ngbrs
    end do
    do k = 1, ngbrs
       ! o2 = o2 * (1.0_8 + couptext(k, nec(k), idx))
       ! o2 = o2 + couptext(k, nec(k), idx)
       o3 = 1.0_8
       do i = 1, k
          ! o3 = o3 * couptext(i, nece(i), nece(0))
          o3 = o3 * inhcoup(nece(i), nece(i-1)) * couptext(1, nece(i), nece(i-1))
       end do
       o2 = o2 + o3
       ! o2 = o2 * (1.0_8 + couptext(k, nece(k), nece(0))) !* k * 1.0_8 / ngbrs ! no converges to one
       ! o2 = o2 * (1.0_8 + couptext(k, nece(k), nece(k-1))) !* k * 1.0_8 / ngbrs ! similar patterns: not super
       ! o2 = o2 * inhcoup(nece(k), nece(k-1)) * couptext(1, nece(k), nece(k-1)) !* k * 1.0_8 / ngbrs ! converges to similar patterns: not super nice ! OK
       ! o2 = o2 * couptext(k, nece(k), nece(0)) !* k * 1.0_8 / ngbrs ! converges to one: no
       ! o2 = o2 + inhcoup(nece(k), nece(k-1)) * couptext(1, nece(k), nece(k-1)) !* k * 1.0_8 / ngbrs !! more noise: not bad for a beginngn ! if 0.1 for all....
       ! o2 = o2 + couptext(k, nece(k), nece(0)) !* k * 1.0_8 / ngbrs ! converges to one, no
    end do
    coupCalc = (o1 + 0.9_8 * o2)! * coup1o! + o2 * coup2o + o3 * coup3o

  end function coupCalc

  function soundsyspn(p, s)
    integer, intent(in) :: p, s
    real(8), dimension(2, numlayers, numosc) :: soundsyspn
    integer :: ss, pn
    ss = samples*seconds

    soundsyspn(1, :, :) = soundsyspt(2,s,:,:) 

    pn = p + 1
    if (pn .le. page) then
       soundsyspn(2,:,:) = soundsyst(pn,s,:,:)
    else
       soundsyspn(2,:,:) = soundsyszt(1,s,:,:)
       pn = 1
    end if
    
    if (s .eq. ss) then
       soundsyspt(2,:,:,:) = soundsyspt(1,:,:,:)
       soundsyspt(1,:,:,:) = soundsyst(pn,:,:,:)
    end if
    
  end function soundsyspn

  
  subroutine dynsound()
    integer :: i, j, p, s, ss, t, pn, sn, li
    real(8) :: tmp
    real(8), dimension(2,numlayers,numosc) :: syspn
    real(8), dimension(numlayers,numosc) :: laysound
    
    print*, "in dynsound"
    
    ! ss = seconds*samples
    ! p = 1
    ! s = 1
    ! do i = 1, ss*page
    !    do j = 1, 4
    !       soundtemp(j) = soundtemp(j) + freq(j) * dt
    !    end do
    !    soundtemp = modulo(soundtemp, pi2)
    !    s = modulo(i-1, ss) + 1
    !    p = int(i/(ss)) + 1
    !    soundout(p, s) = (gauss(bw(1), soundtemp(1)) * sin(soundtemp(1) * sinf(1)) + gauss(bw(2), soundtemp(2)) * sin(soundtemp(2) * sinf(2))) & 
    !          & * (gaussder(bw(3), soundtemp(3)) * sin(soundtemp(3) * sinf(3)) + gaussder(bw(4), soundtemp(4)) * sin(soundtemp(4) * sinf(4))) &
    !          & * 0.2_8
    ! end do
    
    ss = seconds*samples
    p = 1
    s = 1
    do t = 1, ss*page
       s = modulo(t-1, ss) + 1
       p = int((t-1)/ss) + 1

       syspn = soundsyspn(p, s)
       ! print*, "got p and n"
       
       sn = s + 1
       pn = p
       if (sn .gt. ss) then
          sn = 1
          pn = modulo(p,page) + 1
          if (pn .eq. 1) then
             print*, "saving initial page ", p
             soundsyszt(1,:,:,:) = soundsyst(1,:,:,:)
          end if
       end if
       
       ! print*, "get next dxs"

       if (p .ne. pn) then
          print*, "page ", p
       end if

       do i = 1, numlayers
          do j = 1, numosc
             if (i .lt. 3) then 
                laysound(i, j) = gauss(bw(i), soundsyst(p,s,i,j)) * sin(soundsyst(p,s,i,j) * sinf(i))
             else
                laysound(i, j) = gaussder(bw(i), soundsyst(p,s,i,j)) * sin(soundsyst(p,s,i,j) * sinf(i))
             end if
          end do
       end do
       ! print*, "comp sound"

       ! soundout(p, s) = sum(gauss(bw(1), soundsyst(p,s,1,:)) * sin(soundsyst(p,s,1,:) * sinf(1)) &
       !      & + gauss(bw(2), soundsyst(p,s,2,:)) * sin(soundsyst(p,s,2,:) * sinf(2))) & 
       !      & * sum(gaussder(bw(3), soundsyst(p,s,3,:)) * sin(soundsyst(p,s,3,:) * sinf(3)) &
       !      & + gaussder(bw(4), soundsyst(p,s,4,:)) * sin(soundsyst(p,s,4,:) * sinf(4))) &
       !      & * 0.2_8
       soundout(p, s) = ( sum(laysound(1:2,:)) * sum(laysound(3:4,:)) )* 0.1_8
       ! print*, "mixed sound"
       
       do i = 1, numlayers
          li = i - 1
          if (li .eq. 0) then
             li = numlayers
          end if
          
          do j = 1, numosc
             ! print*, p ," page " , s, " sample ", i, " lay : osc : ", j, " : prev and next ", modulo(j-2,numosc) + 1, " ", modulo(j,numosc) + 1
             tmp = freq(i) + &
                  & alpha(i,1) * &
                  & (sin(soundsyst(p, s, i, modulo(j-2,numosc) + 1) - soundsyst(p, s, i,j) - beta(i,1)) &
                  & - sin(soundsyst(p, s, i, modulo(j,numosc) + 1) - soundsyst(p, s, i,j) - beta(i,1))) &
                  & + alpha(i,2) * &
                  & (sin(2.0_8*(soundsyst(p, s, i, modulo(j-2,numosc) + 1) - soundsyst(p, s, i,j)) - beta(i,2)) &
                  & - sin(2.0_8*(soundsyst(p, s, i, modulo(j,numosc) + 1) - soundsyst(p, s, i,j)) - beta(i,2))) & 
                  & + alpha(i,3) * &
                  & (sin(3.0_8*(soundsyst(p, s, i, modulo(j-2,numosc) + 1) - soundsyst(p, s, i,j)) - beta(i,3)) &
                  & - sin(3.0_8*(soundsyst(p, s, i, modulo(j,numosc) + 1) - soundsyst(p, s, i,j)) - beta(i,3))) &
                  & + trlaycoup(i,li) * laysound(li, j) &
                  & + sum(trpagecoup(i,:) * syspn(:,i,j))
             soundsyst(pn, sn, i,j) = soundsyst(p, s, i,j) + tmp * dt          
          end do
       end do
       ! print*, " and return pn and sn", pn, sn 

       p = pn
       s = sn
       do i = numlayers, 1, -1
          do j = numosc, 1, -1
             tmp = freq(i) + &
                  & alpha(i,1) * &
                  & (sin(soundsyst(p, s, i, modulo(j-2,numosc) + 1) - soundsyst(p, s, i,j) - beta(i,1)) &
                  & - sin(soundsyst(p, s, i, modulo(j,numosc) + 1) - soundsyst(p, s, i,j) - beta(i,1))) &
                  & + alpha(i,2) * &
                  & (sin(2.0_8*(soundsyst(p, s, i, modulo(j-2,numosc) + 1) - soundsyst(p, s, i,j)) - beta(i,2)) &
                  & - sin(2.0_8*(soundsyst(p, s, i, modulo(j,numosc) + 1) - soundsyst(p, s, i,j)) - beta(i,2))) & 
                  & + alpha(i,3) * &
                  & (sin(3.0_8*(soundsyst(p, s, i, modulo(j-2,numosc) + 1) - soundsyst(p, s, i,j)) - beta(i,3)) &
                  & - sin(3.0_8*(soundsyst(p, s, i, modulo(j,numosc) + 1) - soundsyst(p, s, i,j)) - beta(i,3))) &
                  & + trlaycoup(i,li) * laysound(li, j) &
                  & + sum(trpagecoup(i,:) * syspn(:,i,j))
             soundsyst(pn, sn, i,j) = soundsyst(p, s, i,j) + tmp * dt          
          end do
       end do

       soundsyst(pn,sn,:,:) = modulo(soundsyst(pn,sn,:,:), pi2)       

    end do

    
  end subroutine dynsound

  subroutine dyntext ( )

    integer :: i, j, k, c, nmi
    real(8) :: x, y, r
    real(8) :: pv


    do i = 1, page
       do j = 1, line
          do k = 1, chars
             call getneighbours(i, j, k)
             do c = 1, systextn       
                pv = coupscaletext * coupCalc(i,j,k,c)
                systexttmp(i,j,k,c) = freqtext + pv
                textenarr(i,j,k,c) = pv
             end do
          end do
       end do
    end do

    textsys = textsys + systexttmp * dttext

    textsys = modulo(textsys, pi2)

  end subroutine dyntext

end module towards

program digest

  use towards
  implicit none

  integer(kind=4) :: io
  real(8) :: fo
  integer :: i, j, k, l, sl, it
  integer :: i1, i2, i3
  integer, dimension(:), allocatable :: is
  character :: at

  print*, "0 for init, 1 for txt, 2 for sound"
  read*, io

  if (io .eq. 0) then
     print*, "got init"

     ! open(unit=17,file="./soundsys.bin",form='unformatted',access='stream',status='replace')
     ! do i = 1, page
     !    do j = 1, seconds*samples
     !       do k = 1, numlayers
     !          do l = 1, numosc
     !             write(17) rand(0) * pi2
     !          end do
     !       end do
     !    end do
     ! end do
     ! flush(17)
     ! close(17)

     open(unit=18,file="./textsys.bin",form='unformatted',access='stream',status='replace')
     do i = 1, page
        do j = 1, line
           do k = 1, chars
              do l = 1, systextn
                 write(18) rand(0) * pi2
              end do
           end do
        end do
     end do
     flush(18)
     close(18)

     open(unit=19,file="./text.txt",status='replace', action='write')
     do i = 1, page
        do j = 1, line
           do k = 1, chars
              write(19, "(A)", advance="no") i2c(floor(rand(0) * systextn) + 1)
           end do
        end do
     end do
     flush(19)
     close(19)

  else

     if (io .eq. 1) then
        allocate(systexttmp(page,line,chars,systextn))
        allocate(is(ngbrs))
        allocate(couptext(ngbrs, systextn, systextn))
        allocate(inhcoup(systextn, systextn))
        allocate(textenarr(page,line,chars,systextn))
        allocate(maxt(page,line,chars))
        
        inhcoup = 1.0_8
        textenarr = 0.0_8
        couptext = 0.0_8

        print*, "reading"
        open(unit=19,file="./text.txt",status='old', action='read')

        ! read in
        ! open(unit=17,file="./soundsys.bin",form='unformatted',access='stream',status='old',action='read')
        ! do i = 1, page
        !    do j = 1, seconds*samples
        !       do k = 1, numlayers
        !          do l = 1, numosc
        !             read(17) soundsys(i,j,k,l)
        !          end do
        !       end do
        !    end do
        ! end do
        ! close(17)

        do i = 1, page
           do j = 1, line
              do k = 1, chars
                 read(19, "(A)", advance="no") at
                 maxt(i,j,k) = c2i(at) 
              end do
           end do
        end do
        close(19)

        open(unit=18,file="./textsys.bin",form='unformatted',access='stream',status='old',action='read')
        do i = 1, page
           do j = 1, line
              do k = 1, chars
                 do l = 1, systextn
                    read(18) textsys(i,j,k,l)
                 end do
              end do
           end do
        end do
        close(18)

        ! print*, "size", systextn

        statement = "Artistic research is in its essence a collective and collaborative endeavour. It is situated particular and subjective: neither valid nor objective. Artistic research does not produce nor transport knowledge. Staging the conditions for aesthetic thinking. Formal and material practices are interwoven into each other, but nevertheless maintain an incompressible difference. This is where the generative potential for aesthetic research lies, exploring and exposing this gap."

        couptext = 0.1_8 ! if mutiplicative
        sl = len(statement)
        do i = 1, sl
           j = c2i(statement(i:i))        
           do k = 1, ngbrs
              is(k) = modulo(i-1-k,sl) + 1
           end do
           do k = 1, ngbrs
              is(k) = c2i(statement(is(k):is(k)))
           end do
           do k = 1, ngbrs
              ! couptext(k, j, is(k)) = couptext(k, j, is(k)) + 0.1_8
              if (couptext(k, j, is(k)) .gt. 0.5_8) then
                 couptext(k, j, is(k)) = couptext(k, j, is(k)) * 1.1_8
              else
                 couptext(k, j, is(k)) = 1.1_8
              end if
           end do
        end do


     else
        allocate(textsys(page,line,chars,systextn))
        ! allocate(soundsys(numlayers,numosc))
        allocate(soundsyspt(2,seconds*samples,numlayers,numosc))
        allocate(soundsyszt(1,seconds*samples,numlayers,numosc))
        allocate(soundsyst(page,seconds*samples,numlayers,numosc))
        allocate(soundout(page,seconds*samples))
        
        soundsyspt = 0.0_8
        soundsyszt = 0.0_8
        ! soundsyst = 0.0_8
        
        ! soundsysp => soundsyspt
        ! soundsyst => soundsystt
        
        alpha(:,1) = 1.0_8
        alpha(:,2) = 2.0_8
        alpha(:,3) = 2.0_8
        
        beta(:,1) = 1.1_8
        beta(:,2) = 0.2_8
        beta(:,3) = 0.71_8
        
        freq(1) = 0.05_8
        freq(2) = 1.5_8
        freq(3) = 200.0_8
        freq(4) = 1000.0_8
        
        bw(1) = 10.0_8
        bw(2) = 100.0_8
        bw(3) = 1000.0_8
        bw(4) = 500.0_8
        

        sinf(1) = 100.0_8
        sinf(2) = 100.0_8
        sinf(3) = 1000.0_8
        sinf(4) = 10000.0_8
        
        trlaycoup(:,1) = 0.5_8
        trlaycoup(:,2) = 0.2_8
        trlaycoup(:,3) = 0.1_8
        trlaycoup(:,4) = 0.01_8

        trpagecoup(1,:) = -0.7_8
        trpagecoup(2,:) = -0.5_8
        trpagecoup(3,:) = -0.5_8
        trpagecoup(4,:) = -2.5_8
        
        ger = 0.0_8
        lud = 0.0_8
        excoup(1) = 100.0_8
        excoup(2) = 100.0_8

        do i = 1, numlayers
           do j = 1, numosc
              soundsyst(1,1,i,j) = rand(0) * pi2
           end do
        end do

     end if
     
     print*, "n for iterations"
     read*, it
     
     print*, "computing"
     do i = 1, it
        ! if (mod(i,100) .eq. 0) then
        print*, "iteration", i
        ! end if
        if (io .eq. 1) then 
           call dyntext()
           call maxtext()
           print*, "done textsys"
        else if (io .eq. 2) then   
           call dynsound()
           print*, "done soundsys"
        end if
     end do
     
     if (io .eq. 1) then 

        ! open(unit=17,file="./soundsys.bin",form='unformatted',access='stream',status='replace')
        ! do i = 1, page
        !    do j = 1, seconds*samples
        !       do k = 1, numlayers
        !          do l = 1, numosc
        !             write(17) soundsys(i,j,k,l)
        !          end do
        !       end do
        !    end do
        ! end do
        ! flush(17)
        ! close(17)

        open(unit=18,file="./textsys.bin",form='unformatted',access='stream',status='replace')
        do i = 1, page
           do j = 1, line
              do k = 1, chars
                 do l = 1, systextn
                    write(18) textsys(i,j,k,l)
                 end do
              end do
           end do
        end do
        flush(18)
        close(18)

        open(unit=19,file="./text.txt",status='replace', action='write')
        ! test
        do i = 1, page
           do j = 1, line
              do k = 1, chars
                 write(19, "(A)", advance="no") i2c(maxt(i,j,k))
              end do
           end do
        end do
        flush(19)
        close(19)

        call exportForElm()
     else if (io .eq. 2) then   
        call exportAudio()
     end if

  end if
  
end program digest
