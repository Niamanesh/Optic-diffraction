!!Niamanesh 28.01.2022
!!Edited 29.01.2022
program Diffraction

  implicit none
  real(8), allocatable, dimension(:):: r8_d
  real(8), allocatable, dimension(:,:):: r8_data
  real(8), allocatable, dimension(:,:):: r8_result
  real(8):: usr_st_num, r0, r1, r2
  integer:: i, j = 0, counter, st_num, f_check, r_check
  logical:: check = .TRUE., end = .True.
  character(len = 25):: Name


  allocate(r8_d(0:2))
  r8_d(0) = 0.06003E-3

  do while(check)
    print*, "Enter the number of station: "
    read(*,*) usr_st_num

    if(usr_st_num == 1 .or. usr_st_num == 2 .or. usr_st_num == 3) then
      check = .FALSE.
      st_num = usr_st_num - 1
    endif

  end do

  print*, "Enter the quantity of data: "
  read(*,*) counter
  counter = counter - 1
  allocate(r8_data(0:2,0:counter)) !! 0 - число М, 1 - z, 2 - L
  allocate(r8_result(0:1,0:counter))

  do while(end)
    print*, "Enter the file full name :"
    read(*,*) name
    open(1, file = name,status = 'old', IOSTAT=f_check)

    if(f_check .NE. 0) then
      print*, "File can't be found"

    else

      print*, "----------------"
      print*, "Reading data ..."
      do i = 0, counter, 1
        read(1, *, IOSTAT=r_check) r0, r1, r2

          if(r_check == 0) then
            r8_data(0,j) = r0
            r8_data(1,j) = r1
            r8_data(2,j) = r2
            j = j + 1
          endif
      enddo

    counter = j - 1
    end = .FALSE.

    endif
  end do

  close(1)

  print*, "Calculating ..."
  do i = 0, counter, 1

    r8_result(0,i) = (r8_d(st_num) / r8_data(0,i)) * SIN(ATAN(r8_data(1,i)/r8_data(2,i)))
    r8_result(1,i) = sqrt(((SIN(ATAN(r8_data(1,i)/r8_data(2,i))) * 0.000001)/r8_data(0,i))**2 + &
    ((r8_d(st_num) * COS(ATAN(r8_data(1,i)/r8_data(2,i))) * 0.001)/((r8_data(0,i)*(r8_data(2,i)&
    + ((r8_data(1,i)**2)/r8_data(2,i))))))**2 + &
    ((COS(ATAN(r8_data(1,i)/r8_data(2,i))) * r8_d(st_num) * r8_data(1,i) &
    * 0.001)/(r8_data(0,i) * (r8_data(2,i)**2 + r8_data(1,i)**2)))**2 )

  end do

  print*, "Writing in output.csv ..."
  open(2, file = "output.csv", IOSTAT=f_check)

  write(2,*) "M", ",   ", "Z",  ",   ", "L",  ",   ", "λ", ",   ", "Δλ"

  do i = 0, counter, 1
    write(2,*) r8_data(0,i), ",   ", r8_data(1,i),  ",   ", r8_data(2,i),  ",   ", r8_result(0,i),&
    ",   ", r8_result(1,i)
  end do

  close(2)

  print*, "-------All done-------"
end program Diffraction
